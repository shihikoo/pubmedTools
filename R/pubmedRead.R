# ----- Functions ----------
GetPubmedUrlLinkWithPmid <- function(pmid)
{
  base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&cmd=llinks&id="
  return(paste0(base,pmid,sep=""))
}

GetElinkContentWithPmid <- function(pmid)
{
  tryCatch({
    eLink <- GetPubmedUrlLinkWithPmid(pmid)
    r0 <- GET(eLink)
    content <- content(r0, "text")
    return(content)
  }, error=function(e){
    return(NULL)
  })
}

RetriveUrlfromContent <-function(content, category = "All")
{
  doc <- xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

  myData <- do.call(rbind, xpathApply(doc, "//ObjUrl", function(node)
  {
    url <- xmlValue(node[["Url"]])
    category <- xmlValue(node[["Category"]])
    return(as.data.frame(cbind(url, category),stringsAsFactors = F, col.names = c("Url", "Category")))
  }))
  if(category == "All") return(myData)

  index <- which(myData$category == category)
  if(length(index) > 0) myData <- myData[index, ] else return(NULL)
  return(myData)
}

#' GetUrlsFromPmid
#'
#' Get related full text urls from given pmid
#'
#' @param pmid a number or a string of characters. The number of pmid.
#' @param fulltext a boolean
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#'
#' @export
#'
#' @examples  pmid <- "28852052"
#' urls <-  GetUrlsFromPubMed(pmid)
#' print(urls)

GetUrlsFromPmid <- function(pmid, fulltext = T)
{
    content <- GetElinkContentWithPmid(pmid)
  if(is.null(content)) {return (NULL)}

  if(fulltext == T) category <- "Full Text Sources"
  urls <- RetriveUrlfromContent(content, "All")

  if(is.null(urls)) return(NULL)

  return(urls[,"url"])
}

#' RetriveUrlsFromPmids
#'
#' Retrive urls from pmids
#'
#' @param pmids a list of numbers or characters. The number of pmid.
#' @param fulltext a boolean
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#' @export
#'
#' @examples pmid <- c("28852052", "29041955")
#' urls <-  GetUrlsFromPubMed(pmid)
#' print(urls)
#'
RetriveUrlsFromPmids <- function(pmids, fulltext = T)
{
  ncores <- detectCores(all.tests = FALSE, logical = TRUE)
  cl <- makeCluster(round(ncores), outfile="") #determines how many parallel processes are used for the pdf downloading
  registerDoParallel(cl)

  UrlFromPMIDs <- foreach(i=1:length(pmids), .packages=c('GetUrlsFromPmid')) %dopar% {
    GetUrlsFromPmid(PMIDs[i], fulltext = fulltext)
  }
  return(UrlFromPMIDs)
}

# ----- Functions ----------
GetBaselink <- function(db,id, apiKey = ""){
  baseUrl <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  links <- data.frame(
    ELinkURLsLink = paste0(baseUrl, "elink.fcgi?dbfrom=",db,"&cmd=llinks&id=",id),
    EfetcLink = paste0(baseUrl, "efetch.fcgi?db=",db,"&id=",id,"&retmode=xml"),
    EsummaryLink = paste0(baseUrl, "esummary.fcgi?db=",db,"&id=",id),
    stringsAsFactors = F
  )
  if(apiKey != "") links <- sapply(links, function(x)  paste0(x, "&api_key=",apiKey))

return(links)
}

GetContentWithLink <- function(link){
  tryCatch({
    r0 <- GET(as.character(link))
    content <- content(r0, "text")
    return(content)
  }, error=function(e){
    return(NULL)
  })
}

RetriveXmlNodeValuefromContent <-function(content, nodePosition){
  doc <- xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)
  node <- xpathApply(doc, nodePosition )
  result <- xmlValue(node[[1]])
  if(is.na(node)) return(NULL)
  return(result)
}

RetriveXmlNodeValuefromDoc <-function(doc, nodePosition){
  node <- xpathApply(doc, nodePosition )
  if(length(node) == 0) return(NULL)
  result <- xmlValue(node[[1]])
  return(result)
}

GetPmidDoiFromPmcid <- function(pmcid, apiKey){
  GetPmidContentFromPmcid <- function(pmcid, apiKey){
    links <- GetBaselink("pmc", pmcid,apiKey)
    content <- GetContentWithLink(links["EsummaryLink"])
    return(content)
  }

  content <- GetPmidContentFromPmcid(pmcid, apiKey)
  if(is.null(content)) {return (NULL)}
  doc <- xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

  pmid <- RetriveXmlNodeValuefromDoc(doc, "//Item[@Name='pmid']")
  if(is.null(pmid)) return(NULL)

  doi <- RetriveXmlNodeValuefromDoc(doc, "//Item[@Name='doi']")
  if(is.null(doi)) return(NULL)

  return(data.frame(pmid=pmid,doi=doi))
}

GetInfoFromPmid <- function(pmid, apiKey){
  GetEfetchContentFromPmid <- function(pmid, apiKey){
    links <- GetBaselink("pubmed", pmid, apiKey)
    content <- GetContentWithLink(links["EfetcLink"])
    return(content)
  }

  content <- GetEfetchContentFromPmid(pmid, apiKey)
  if(is.null(content)) {return (NULL)}

  doc <- xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

  journal <- RetriveXmlNodeValuefromDoc(doc,  "//Journal//Title")
  journalCountry <- RetriveXmlNodeValuefromDoc(doc,  "//MedlineJournalInfo//Country")
  publicationYear <- RetriveXmlNodeValuefromDoc(doc,  "//JournalIssue//PubDate//Year")
  authors <- do.call(rbind, xpathApply(doc, "//Author", function(node)
  {
    forename <- xmlValue(node[["ForeName"]])
    lastname <- xmlValue(node[["LastName"]])
    return(paste(forename, lastname))
  }))
  authors <- paste(authors, collapse = ", ")
  affiliation <- RetriveXmlNodeValuefromDoc(doc,  "//Affiliation")

  myData <- data.frame(
    journal = journal,
    journalCountry = journalCountry,
    publicationYear = publicationYear,
    authors = authors,
    affiliation = affiliation,
    stringsAsFactors = F
  )

  return(myData)
}

RetriveInfoFromPmids <- function(pmids, apiKey){
  infoFromPMIDs <- sapply(pmids, GetInfoFromPmid, apiKey = apiKey)
  return(infoFromPMIDs)
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
#' apiKey <- ""
#' url <-  GetUrlsFromPubMed(pmid, apiKey)
#' print(url)
#'
GetUrlsFromPmid <- function(pmid, apiKey, fulltext = T){
  GetUrlsContentWithPmid <- function(pmid, apiKey){
    links <- GetBaselink("pubmed", pmid, apiKey)
    content <- GetContentWithLink(links["ELinkURLsLink"])
    return(content)
  }
  RetriveUrlfromContent <-function(content, category = "All"){
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

  content <- GetUrlsContentWithPmid(pmid, apiKey)
  if(is.null(content)) {return (NULL)}

  if(fulltext == T) category <- "Full Text Sources" else category = "All"
  urls <- RetriveUrlfromContent(content, category)

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
#' @examples pmids <- c("28852052", "29041955")
#' apiKey <- ""
#' urls <-  RetriveUrlsFromPmids(pmids, apiKey)
#' print(urls)
#'
RetriveUrlsFromPmids <- function(pmids, apiKey, fulltext = T){
  urlFromPMIDs <- sapply(pmids, GetUrlsFromPmid, fulltext = fulltext, apiKey = apiKey)
  return(urlFromPMIDs)
}

#' RetriveUrlsFromPmidsParallel
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
#' @examples pmids <- c("28852052", "29041955")
#' apiKey <- ""
#' urls <-  RetriveUrlsFromPmidsParallel(pmids, apiKey)
#' print(urls)
#'
RetriveUrlsFromPmidsParallel <- function(pmids, apiKey, fulltext = T){
  ncores <- detectCores(all.tests = FALSE, logical = TRUE)
  cl <- makeCluster(round(ncores), outfile="") #determines how many parallel processes are used for the pdf downloading
  registerDoParallel(cl)

  urlFromPMIDs <- foreach(i=1:length(pmids), .packages=c('GetUrlsFromPmid')) %dopar% {
    GetUrlsFromPmid(PMIDs[i], fulltext = fulltext)
  }
  return(urlFromPMIDs)
}

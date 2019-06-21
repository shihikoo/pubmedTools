# ----- Functions ----------

#' GetBaselink
#'
#' @param db a string of characters, valid database name from NCBI: https://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.chapter2_table1
#' @param id a string of characters, valid id matching the database
#' @param apiKey a string of characters, API Key of the user
#' @param email a string of characters, email address of the user
#'
#' @return a list of links
#' @export
#'
#' @examples links <- GetBaselink("pubmed", "4804230", "","")
#'
GetBaselink <- function(db,id, apiKey = "", email = ""){
  baseUrl <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  links <- data.frame(
    ELinkURLsLink = paste0(baseUrl, "elink.fcgi?dbfrom=",db,"&cmd=llinks&id=",id),
    EfetcLink = paste0(baseUrl, "efetch.fcgi?db=",db,"&id=",id,"&retmode=xml"),
    EsummaryLink = paste0(baseUrl, "esummary.fcgi?db=",db,"&id=",id),
    stringsAsFactors = F
  )
  if(apiKey != "") links <- sapply(links, function(x)  paste0(x, "&api_key=", apiKey))
  if(email != "") links <- sapply(links, function(x)  paste0(x, "&email=", email))

  return(links)
}

#' GetContentWithLink
#'
#' @param link a string of characters
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples baselink <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
#' link <- paste0(baselink, "efetch.fcgi?db=pubmed&rettype=xml&id=26502666")
#' content <- GetContentWithLink(link)
#'
#'
#' @import httr
#'
GetContentWithLink <- function(link){
  tryCatch({
    if(grep("api_key",link)) Sys.sleep(0.1)
    else Sys.sleep(0.3)
    r0 <- httr::GET(as.character(link))
    content <- httr::content(r0, "text")
    return(content)
  }, error=function(e){
    return(NULL)
  })
}

#' RetriveXmlNodeValuefromDoc
#'
#' @param doc the parsed XML file
#' @param nodePosition the node position of the xml file
#'
#' @return the values of the node
#' @export
#'
#' @import XML
#'
RetriveXmlNodeValuefromDoc <-function(doc, nodePosition){
  node <- xpathApply(doc, nodePosition )
  if(length(node) == 0) return(NULL)
  result <- XML::xmlValue(node[[1]])
  return(result)
}


#' GetPmidDoiFromPmcid
#'
#' @param pmcid a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a string: pmid
#' @export
#'
#' @examples pmid <- GetPmidDoiFromPmcid("28852052", "",  "")
#'
#' @import XML
#'
GetPmidDoiFromPmcid <- function(pmcid, apiKey, email){
  GetPmidContentFromPmcid <- function(pmcid, apiKey, email){
    links <- GetBaselink("pmc", pmcid,apiKey, email)
    content <- GetContentWithLink(links["EsummaryLink"])
    return(content)
  }

  content <- GetPmidContentFromPmcid(pmcid, apiKey, email)
  if(is.null(content)) {return (NULL)}
  doc <- XML::xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

  pmid <- RetriveXmlNodeValuefromDoc(doc, "//Item[@Name='pmid']")
  if(is.null(pmid)) return(NULL)

  doi <- RetriveXmlNodeValuefromDoc(doc, "//Item[@Name='doi']")
  if(is.null(doi)) return(NULL)

  return(data.frame(pmid=pmid,doi=doi))
}

#' GetMetaDataFromPmid
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples pmid <- "28852052"
#' apiKey <- ""
#' email <- ""
#' metaData <-  GetMetaDataFromPmid(pmid, apiKey, email)
#' print(metaData)
#'
#' @import XML
#'
GetMetaDataFromPmid <- function(pmid, apiKey, email){
  GetEfetchContentFromPmid <- function(pmid, apiKey, email){
    links <- GetBaselink("pubmed", pmid, apiKey, email)
    content <- GetContentWithLink(links["EfetcLink"])
    return(content)
  }

  content <- GetEfetchContentFromPmid(pmid, apiKey, email)
  if(is.null(content)) {return (NULL)}

  doc <- XML::xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

  journal <- RetriveXmlNodeValuefromDoc(doc,  "//Journal//Title")
  journalCountry <- RetriveXmlNodeValuefromDoc(doc,  "//MedlineJournalInfo//Country")
  publicationYear <- RetriveXmlNodeValuefromDoc(doc,  "//JournalIssue//PubDate//Year")
  authors <- do.call(rbind, xpathApply(doc, "//Author", function(node)
  {
    forename <- XML::xmlValue(node[["ForeName"]])
    lastname <- XML::xmlValue(node[["LastName"]])
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



#' RetriveMetaDataFromPmids
#'
#' @param pmids a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  pmids <- c("28852052", "29041955")
#' apiKey <- ""
#' email <- ""
#' metaData <-  RetriveMetaDataFromPmids(pmids, apiKey, email)
#' print(metaData)
#'
#'
RetriveMetaDataFromPmids <- function(pmids, apiKey, email){
  metaDataFromPMIDs <- sapply(pmids, GetMetaDataFromPmid, apiKey = apiKey, email = email)
  return(as.data.frame(t(metaDataFromPMIDs)))
}

#' GetUrlsFromPmid
#'
#' Get related full text urls from given pmid
#'
#' @param pmid a number or a string of characters. The number of pmid.
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param fulltext a boolean. If TRUE, function only searches for full text link
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#'
#' @export
#'
#' @examples  pmid <- "28852052"
#' url <-  GetUrlsFromPmid(pmid, "", "")
#'
#' @import XML
#'
GetUrlsFromPmid <- function(pmid, apiKey, email, fulltext = T){
  GetUrlsContentWithPmid <- function(pmid, apiKey, email){
    links <- GetBaselink("pubmed", pmid, apiKey, email)
    content <- GetContentWithLink(links["ELinkURLsLink"])
    return(content)
  }
  RetriveUrlfromContent <-function(content, category = "All"){
    doc <- XML::xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

    myData <- do.call(rbind, xpathApply(doc, "//ObjUrl", function(node)
    {
      url <- XML::xmlValue(node[["Url"]])
      category <- XML::xmlValue(node[["Category"]])
      return(as.data.frame(cbind(url, category), stringsAsFactors = F, col.names = c("Url", "Category")))
    }))
    if(category == "All") return(myData)

    index <- which(myData$category == category)
    if(length(index) > 0) myData <- myData[index, ] else return(NULL)
    return(myData)
  }

  content <- GetUrlsContentWithPmid(pmid, apiKey, email)
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
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param fulltext a boolean. If TRUE, function only searches for full text link
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#' @export
#'
#' @examples pmids <- c("28852052", "29041955")
#' apiKey <- ""
#' email <- ""
#' urls <-  RetriveUrlsFromPmids(pmids, apiKey, email)
#' print(urls)
#'
RetriveUrlsFromPmids <- function(pmids, apiKey, email, fulltext = T){
  urlFromPMIDs <- sapply(pmids, GetUrlsFromPmid, fulltext = fulltext, apiKey = apiKey, email = email)
  return(urlFromPMIDs)
}


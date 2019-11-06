#' DownloadMetaDataWithPmidsBatch
#'
#' @param pmids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param endpoint a string of characters. The API endpoint to use. e.g. "esummary", "efetch"
#' @param email a string of characters. Your email address
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples DownloadXMLWithPmidsBatch(c("28852052", "29041955"), endpoint = "esummary", fileBaseName="test.xml")
#'
#' @import XML
#'
DownloadXMLWithPmidsBatch <- function(pmids, apiKey = "", endpoint = "esummary", email = "", fileBaseName = "") {
    db <- "pubmed"
    nids <- length(pmids)
    grid <- 500
    nloop <- ceiling(nids / grid)
    for (iloop in 1:nloop) {
      iindex <-
        ((iloop - 1) * grid) + 1:ifelse(iloop * grid > nids, nids, iloop * grid)
      doc <-
        GetDoc(id =pmids[iindex], db = db, endpoint = endpoint, apiKey = apiKey, email = email)

      outputFile <-
        XML::saveXML(doc, file = paste0(
          gsub("[.]xml", "", fileBaseName),
          min(iindex),
          "_",
          max(iindex),
          ".xml"
        ))
    }
    return(nloop)
  }

#' RetriveJournalFromPmidEfetcXML
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  journal <- RetriveJournalFromPmidEfetcXML(c("28852052", "29041955","31230181"))
#' @import XML
#'
RetriveJournalFromPmidEfetcXML <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
        journal <- RetriveXmlNodeValuefromDoc(XML::xmlDoc(x),  "//Journal//Title")
        return(paste0(journal, collapse = "||"))}
      ))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#' RetriveFunderFromPmidEfetcXML
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  RetriveFunderFromPmidEfetcXML(c("31353518", "29041955","31230181"))
#' @import XML
#'
RetriveFunderFromPmidEfetcXML <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
      funders <- RetriveXmlNodeValuefromDoc(XML::xmlDoc(x),  "//GrantList//Grant//Agency")
      return(paste0(funders, collapse = "||"))}
    ))

    return(as.data.frame(results, stringsAsFactors = F))
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
#' @examples  metaData1 <-  GetMetaDataFromPmid(c("28852052", "29041955","31230181"))
#' @import XML
#'
GetMetaDataFromPmid <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <-
      do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
        article <- XML::xmlDoc(x)
        pmid <- RetriveXmlNodeValuefromDoc(article,  "//PMID")
        journal <-
          RetriveXmlNodeValuefromDoc(article,  "//Journal//Title")
        journalCountry <-
          RetriveXmlNodeValuefromDoc(article,  "//MedlineJournalInfo//Country")
        publicationYear <-
          RetriveXmlNodeValuefromDoc(article,  "//JournalIssue//PubDate//Year")

        authors <-
          do.call(rbind, XML::xpathApply(article, "//Author", function(subnode)
          {
            forename <- XML::xmlValue(subnode[["ForeName"]])
            lastname <- XML::xmlValue(subnode[["LastName"]])
            return(paste(forename, lastname))
          }))
        authors <- paste(authors, collapse = "||")
        affiliations <-
          paste0(unique(RetriveXmlNodeValuefromDoc(article,  "//Affiliation")), collapse = "||")

        return(cbind(
          pmid,
          journal,
          journalCountry,
          publicationYear,
          authors,
          affiliations
        ))
      }))

    return(as.data.frame(results, stringsAsFactors = F))
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
#' @examples  metaData <-  RetriveMetaDataFromPmids(c("28852052", "29041955"))
#'
RetriveMetaDataFromPmids <- function(pmids, apiKey = "", email = "") {
    metaDataFromPMIDs <-
      sapply(
        pmids,
        GetMetaDataFromPmid,
        apiKey = apiKey,
        email = email
      )
    return(as.data.frame(t(metaDataFromPMIDs)))
  }

#' RetriveUrlfromELinkXML
#'
#' Retrive Url from Elink xml doc
#'
#' @param doc a XMLInternalDocument.
#' @param category a string of characters. A link category. Defaul is "All"
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#'
#' @export
#'
#' @examples doc <- GetDoc(id="28852052", db="pubmed", endpoint="elink", cmd = "llinks")
#' RetriveUrlfromELinkXML(doc, "Full Text Sources")
#'
#' @import XML
#'
RetriveUrlfromELinkXML <- function(doc, category = "All") {
  myData <-
    do.call(rbind, XML::xpathApply(doc, "//ObjUrl", function(node)
    {
      url <- XML::xmlValue(node[["Url"]])
      category <- XML::xmlValue(node[["Category"]])
      return(as.data.frame(
        cbind(url, category),
        stringsAsFactors = F,
        col.names = c("Url", "Category")
      ))
    }))
  if (category == "All")
    return(myData)

  index <- which(myData$category == category)
  if (length(index) > 0) myData <- myData[index,]
  else return(NULL)

  return(myData)
}

#' GetUrlsFromPmid
#'
#' Get related url links from given pmid
#'
#' @param pmid a number or a string of characters. The number of pmid.
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param fulltext a boolean. If TRUE, function only searches for full text link
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T
#' Return NULL if none is found.
#'
#' @export
#'
#' @examples GetUrlsFromPmid("28852052",fulltext = TRUE)
#'
#' @import XML
#'
GetUrlsFromPmid <- function(pmid, apiKey = "", email = "", fulltext = TRUE) {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "elink", apiKey = apiKey, email = email, cmd = "llinks")

    if (fulltext == T) category <- "Full Text Sources" else category = "All"
    urls <- RetriveUrlfromELinkXML(doc, category)
    if (is.null(urls)) return(NULL)

    return(urls[, "url"])
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
#' @examples  RetriveUrlsFromPmids(c("28852052", "29041955"), "", "", TRUE)
#'
RetriveUrlsFromPmids <- function(pmids, apiKey = "", email = "", fulltext = TRUE) {
    urlFromPMIDs <-
      sapply(
        pmids,
        GetUrlsFromPmid,
        apiKey = apiKey,
        email = email,
        fulltext = fulltext
      )
    return(urlFromPMIDs)
  }

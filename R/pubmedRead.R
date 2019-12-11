#---------- Download  -----

#' DownloadXMLWithPmidsBatch
#'
#' @param pmids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param endpoint a string of characters. The API endpoint to use. e.g. "esummary", "efetch"
#' @param email a string of characters. Your email address
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return the output file names
#' @export
#'
#' @examples DownloadXMLWithPmidsBatch(c("28852052", "29041955"), endpoint = "esummary", fileBaseName="test.xml")
#'
#' @import XML
#'
DownloadXMLWithPmidsBatch <- function(pmids, apiKey = "", endpoint = "esummary", email = "", fileBaseName = "test.xml") {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids / grid)
  outputFiles <- matrix("", nrow=nloop)
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
    outputFiles[iloop] <-outputFile
  }
  return(outputFiles)
}

#---------- Single parser with pubmed efetch xml as input -----
#' RetrivePMIDFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetrivePMIDFromPubmedEfetch(doc)
#' @import XML
#'
RetrivePMIDFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//PMID")[[1]])
}

#' RetrivePublicationYearFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetrivePublicationYearFromPubmedEfetch(doc)
#' @import XML
#'
RetrivePublicationYearFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//MedlineCitation//DateCompleted//Year"))
}

#' RetriveJournalFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetriveJournalFromPubmedEfetch(doc)
#' @import XML
#'
RetriveJournalFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//Journal//Title"))
}

#' RetriveJournalCountryFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetriveJournalCountryFromPubmedEfetch(doc)
#' @import XML
#'
RetriveJournalCountryFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//MedlineJournalInfo//Country")
  )
}

#' RetrivePMCIDFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetrivePMCIDFromPubmedEfetch(doc)
#' @import XML
#'
RetrivePMCIDFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//ArticleId[@IdType='pmc']"))
}

#' RetriveFundersFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetriveFundersFromPubmedEfetch(doc)
#' @import XML
#'
RetriveFundersFromPubmedEfetch <- function(doc){
  funders <- unique(RetriveXmlNodeValuefromDoc(doc,  "//GrantList//Grant//Agency"))
  funderStr <- paste0(stats::na.omit(funders), collapse = "||")
  return(funderStr)
}

#' RetriveAuthorsFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#' RetriveAuthorsFromPubmedEfetch(doc)
#' @import XML stats
#'
RetriveAuthorsFromPubmedEfetch <- function(doc){
  authors <- paste(stats::na.omit(
    do.call(rbind, XML::xpathApply(doc, "//Author", function(subnode)
    {
      forename <- XML::xmlValue(subnode[["ForeName"]])
      lastname <- XML::xmlValue(subnode[["LastName"]])
      return(paste(forename, lastname))
    }))), collapse = "||")

  return(authors)
}

#' RetriveAffiliationFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#' RetriveAffiliationFromPubmedEfetch(doc)
#' @import XML
#'
RetriveAffiliationFromPubmedEfetch <- function(doc){
  affs <- paste0(unique(RetriveXmlNodeValuefromDoc(doc,  "//Affiliation")), collapse = "||")
  return(affs)
}

#' RetriveTitleFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#' RetriveTitleFromPubmedEfetch(doc)
#' @import XML
#'
RetriveTitleFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//Article//ArticleTitle"))
}

#' RetriveAbstractFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
#'  RetriveAbstractFromPubmedEfetch(doc)
#' @import XML
#'
RetriveAbstractFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//Abstract"))
}

#------- Single parser with pmids as input ----
#' RetriveJournalWithPmids
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a nx3 data frame
#' @export
#'
#' @examples  journal <- RetriveJournalWithPmids(c("28852052", "29041955","31230181"))
#' @import XML
#'
RetriveJournalWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
      article <- XML::xmlDoc(x)
      journal <- RetriveJournalFromPubmedEfetch(article)
      pmid <- RetrivePMIDFromPubmedEfetch(article)
      journalCountry <- RetriveJournalCountryFromPubmedEfetch(article)
      return(cbind(
        pmid,
        journal,
        journalCountry
      ))}))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#' RetriveFunderWithPmids
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  RetriveFunderWithPmids(c("29041955","31230181"))
#' @import XML
#'
RetriveFunderWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
      article <- XML::xmlDoc(x)
      funders <- RetriveFundersFromPubmedEfetch(article)
      pmid <- RetrivePMIDFromPubmedEfetch(article)
      return(cbind(
        pmid,
        funders
      ))}))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#' RetriveTiAbWithPmids
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  RetriveTiAbWithPmids(c("29041955","31230181"))
#' @import XML
#'
RetriveTiAbWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
      article <- XML::xmlDoc(x)
      title <- RetriveTitleFromPubmedEfetch(article)
      abstract <- RetriveAbstractFromPubmedEfetch(article)
      return(cbind(
        title,
        abstract
      ))}))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#' RetrivePmcidWithPmids
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  pmcids <- RetrivePmcidWithPmids(c("28852052", "29041955","31230181"))
#' @import XML
#'
RetrivePmcidWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
      article <- XML::xmlDoc(x)
      pmcid <- RetrivePMCIDFromPubmedEfetch(article)
      pmid <- RetrivePMIDFromPubmedEfetch(article)
      return(cbind(
        pmid,
        pmcid
      ))}))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#---------- Multiple parsers with pubmed efetch xml as input ----
#' RetriveMetaDataFromPubmedEfetch
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  doc <- GetDoc(id = c("28852052", "29041955","31230181"), db = "pubmed", endpoint = "efetch")
#' metaData <- RetriveMetaDataFromPubmedEfetch(doc)
#' @import XML
#'
RetriveMetaDataFromPubmedEfetch <-
  function(doc) {
    results <-
      do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
        article <- XML::xmlDoc(x)
        pmid <- RetrivePMIDFromPubmedEfetch(article)
        journal <- RetriveJournalFromPubmedEfetch(article)
        journalCountry <- RetriveJournalCountryFromPubmedEfetch(article)
        publicationYear <- RetrivePublicationYearFromPubmedEfetch(article)
        pmcid <- RetrivePMCIDFromPubmedEfetch(article)
        funders <- RetriveFundersFromPubmedEfetch(article)
        authors <- RetriveAuthorsFromPubmedEfetch(article)
        affiliations <- RetriveAffiliationFromPubmedEfetch(article)

        title <- RetriveTitleFromPubmedEfetch(article)
        abstract <- RetriveAbstractFromPubmedEfetch(article)

        return(cbind(
          pmid,
          pmcid,
          journal,
          journalCountry,
          publicationYear,
          funders,
          authors,
          affiliations,
          title,
          abstract
        ))
      }))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#' RetriveMetaDataFromPubmedEfetchParallel
#'
#' @description Good for used in local multiple xmls
#' @param files a list of string. A list of xml filenames
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  files <- system.file("example/xml/22427380.xml", package="pubmedTools")
#' files <- c("example/xml/22427380.xml", "example/xml/31230181.xml")
#'
RetriveMetaDataFromPubmedEfetchParallel <- function(files) {
  metaDataFromPMIDs <-
    sapply(
      files,
      function(x){
        doc <- XML::xmlTreeParse(file = readLines(x), encoding = "UTF-8", useInternalNodes = TRUE, trim = FALSE)
        RetriveMetaDataFromPubmedEfetch(doc)
      }
    )
  return(as.data.frame(t(metaDataFromPMIDs)))
}

#---------- Multiple parsers with pmids as input ----
#' RetriveMetaDataFromPmids
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param outputFilename a string of characters. Output XML file name

#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  metaData <- RetriveMetaDataFromPmids(c("28852052", "29041955","31230181"), apiKey = "2ab938c0c3cfbfe6446b544c8e37c5a4e609")
#' @import XML
#'
RetriveMetaDataFromPmids <-
  function(pmid,
           apiKey = "",
           email = "", outputFilename = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    if(outputFilename != ""){
      outputFile <- XML::saveXML(doc, file = outputFilename)
    }

    results <-
      do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x) {
        return(RetriveMetaDataFromPubmedEfetch( XML::xmlDoc(x)))
      }))

    return(as.data.frame(results, stringsAsFactors = F))
  }

#' RetriveMetaDataFromPmidsBatch
#'
#' @description this function is good for retrive data directly
#' @param pmids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param outputFileBaseName a string of characters. The base name of output xml files. If default, there will be no xml saved.
#' @return a nx7 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples RetriveMetaDataFromPmidsBatch(c("28852052", "29041955"))
#'
#' @import XML
#'
RetriveMetaDataFromPmidsBatch <- function(pmids, apiKey = "", email = "", outputFileBaseName = "") {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids / grid)
  results <- as.data.frame(matrix(nrow = nids, ncol = 10))

  for (iloop in 1:nloop) {
    iindex <- ((iloop - 1) * grid) + 1:ifelse(iloop * grid > nids, nids, iloop * grid)

    if(outputFileBaseName != ""){
      outputFilename <- paste0(
        gsub("[.]xml", "", outputFileBaseName),
        min(iindex),
        "_",
        max(iindex),
        ".xml"
      )
    } else {outputFilename <- ""}

    result <- RetriveMetaDataFromPmids(pmids[iindex], apiKey = apiKey, email = email, outputFilename = outputFilename)
    results[iindex, ] <- result
  }

  names(results) <- names(result)
  return(results)
}

#---- Parser for pubmed Elink ----
#' RetriveUrlFromPubmedElink
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
#' @examples doc <- GetDoc(id = c("28852052", "29041955"), db = "pubmed", endpoint = "elink", cmd = "llinks")
#' RetriveUrlFromPubmedElink(doc, "Full Text Sources")
#'
#' @import XML
#'
RetriveUrlFromPubmedElink <- function(doc, category = "All") {
  RetriveUrlFromPubmedElinkSingle <- function(x, category = "All"){
    myData <-
      do.call(rbind, XML::xpathApply(x, "//ObjUrl", function(node)
      {
        url <-XML::xmlValue(node[["Url"]])
        category <- XML::xmlValue(node[["Category"]])
        return(as.data.frame(
          cbind(url, category),
          stringsAsFactors = F,
          col.names = c("Url", "Category")
        ))
      }))
    if (category != "All") {
      index <- which(myData$category == category)
      if (length(index) == 0) return(NULL)
      myData <- myData[index,]
    }
    pmid <- RetriveXmlNodeValuefromDoc(x,"//Id")[[1]]
    return(data.frame(pmid = pmid, url = paste0(myData$url, collapse = ";"), stringsAsFactors = F))
  }

  results <- do.call(rbind, XML::xpathApply(doc, "//IdUrlSet", function(x)
  {
    return(RetriveUrlFromPubmedElinkSingle(XML::xmlDoc(x), category = category))
  }))

  return(results)
}

#' RetriveUrlsFromPmids
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
#' @examples RetriveUrlsFromPmids(c("28852052", "29041955"),fulltext = TRUE)
#'
#' @import XML
#'
RetriveUrlsFromPmids <- function(pmid, apiKey = "", email = "", fulltext = TRUE) {
  doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "elink", apiKey = apiKey, email = email, cmd = "llinks")
  if (fulltext == T) category <- "Full Text Sources" else category = "All"

  urls <- RetriveUrlFromPubmedElink(doc, category)

  if (is.null(urls)) return(NULL)
  return(urls[,])
}

#' RetriveUrlsFromPmidsBatch
#'
#' @description this function is good for retrive data directly
#' @param pmids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param fulltext a boolean. Whether or not only retrive full text
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples RetriveUrlsFromPmidsBatch(c("28852052", "29041955"))
#'
#' @import XML
#'
RetriveUrlsFromPmidsBatch <- function(pmids, apiKey = "", email = "", fulltext = TRUE) {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids / grid)
  results <- as.data.frame(matrix(nrow = nids, ncol = 2))

  for (iloop in 1:nloop) {
    iindex <- ((iloop - 1) * grid) + 1:ifelse(iloop * grid > nids, nids, iloop * grid)
    result <- RetriveUrlsFromPmids(pmids[iindex], apiKey = apiKey, email = email, fulltext = fulltext)
    results[iindex, ] <- result
  }

  names(results) <- names(result)
  return(results)
}

#' RetriveUrlsFromPmidParallel
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
#' @examples  RetriveUrlsFromPmidParallel(c("28852052", "29041955"), "", "", TRUE)
#'
RetriveUrlsFromPmidParallel <- function(pmids, apiKey = "", email = "", fulltext = TRUE) {
  urlFromPMIDs <-
    sapply(
      pmids,
      RetriveUrlsFromPmids,
      apiKey = apiKey,
      email = email,
      fulltext = fulltext
    )
  return(urlFromPMIDs)
}

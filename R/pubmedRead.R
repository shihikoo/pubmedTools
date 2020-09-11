#---------- Download  -----

#' GetPmidsWithSearch
#'
#' @param searchTerm a string of characters. search string
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return the output file names
#' @export
#'
#' @examples term = "pinkeye"
#' GetPmidsWithSearch(term)
#'
#' @import jsonlite
#'
GetPmidsWithSearch <- function(searchTerm, apiKey = "",  email = "") {
  db <- "pubmed"
  endpoint = "esearch"
  batchSize <- 500
  
  result_json <-
    GetJson(term = searchTerm, db = db, endpoint = endpoint, apiKey = apiKey, email = email, usehistory = T, retmax = batchSize)
  
  esearchresult <- result_json$esearchresult
  
  ids <- esearchresult$idlist
  nTotal <- as.numeric(esearchresult$count)
  while(length(ids) < nTotal){
    result_json <-
      GetJson(term = searchTerm, db = db, endpoint = endpoint, apiKey = apiKey, email = email, usehistory = T, retmax = batchSize, WebEnv = esearchresult$webenv, retstart = length(ids))
    
    esearchresult <- result_json$esearchresult
    
    ids <- c(ids, esearchresult$idlist)
  }
  
  return(ids)
}

#' #' DownloadJsonWithPmidsBatch
#' #'
#' #' @param pmids a string of character. PubMed central Id
#' #' @param apiKey a string of characters. The API Key obtained through NCBI account
#' #' @param endpoint a string of characters. The API endpoint to use. e.g. "esummary", "efetch"
#' #' @param email a string of characters. Your email address
#' #' @param fileBaseName a string of character. The base name of the to be saved xml files
#' #'
#' #' @return the output file names
#' #' @export
#' #'
#' #' @examples DownloadJsonWithPmidsBatch(c("28852052", "29041955"), endpoint = "efetch", fileBaseName="test.json")
#' #'
#' #' @import jsonlite
#' #'
#' DownloadJsonWithPmidsBatch <- function(pmids, apiKey = "", endpoint = "efetch", email = "", fileBaseName = "test.json") {
#'   db <- "pubmed"
#'   nids <- length(pmids)
#'   grid <- 400
#'   nloop <- ceiling(nids / grid)
#'   outputFiles <- matrix("", nrow=nloop)
#'   for (iloop in 1:nloop) {
#'     iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)
#'     
#'     result_josn <-
#'       GetJson(id =pmids[iindex], db = db, endpoint = endpoint, apiKey = apiKey, email = email)
#'     
#'     outputFile <-
#'       jsonlite::write_json(result_josn, path = paste0(
#'         gsub("[.]json", "", fileBaseName),
#'         min(iindex),
#'         "_",
#'         max(iindex),
#'         ".json"
#'       ))
#'     outputFiles[iloop] <- outputFile
#'   }
#'   return(outputFiles)
#' }


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
#' @examples DownloadXMLWithPmidsBatch(c("28852052", "29041955"), endpoint = "efetch", fileBaseName="test.xml")
#'
#' @import xml2
#'
DownloadXMLWithPmidsBatch <- function(pmids, apiKey = "", endpoint = "esummary", email = "", fileBaseName = "test.xml") {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 400
  nloop <- ceiling(nids / grid)
  outputFiles <- matrix("", nrow=nloop)
  for (iloop in 1:nloop) {
    iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)

    doc <-
      GetDoc(id =pmids[iindex], db = db, endpoint = endpoint, apiKey = apiKey, email = email)
    filename <- paste0(
      gsub("[.]xml", "", fileBaseName),
      min(iindex),
      "_",
      max(iindex),
      ".xml"
    )
    outputFile <-
      xml2::write_xml(doc, file =filename )
    outputFiles[iloop] <- filename
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
#' @import xml2
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
#' @import xml2
#'
RetrivePublicationYearFromPubmedEfetch <- function(doc){
  year <- RetriveXmlNodeValuefromDoc(doc,  "//Journal//Year")[[1]]
  if(is.na(year)) year <- RetriveXmlNodeValuefromDoc(doc,  "//History//Year")[[1]]
  return(year)
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
#' @import xml2
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
#' @import xml2
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
#' @import xml2
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
#' @import xml2
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
#' @import xml2 stats
#'
RetriveAuthorsFromPubmedEfetch <- function(doc){
  nodesets <- xml2::xml_find_all(doc, "//Author")
  nodesetList <- xml2::as_list(nodesets)
  
  authorList <- sapply(nodesetList, function(x) {
    author <- trimws(paste(x[["ForeName"]],  x[["LastName"]]))
  })
  authors <- paste(authorList, collapse = "||")
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
#' @import xml2
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
#' @import xml2
#'
RetriveTitleFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//ArticleTitle"))
}

#' RetriveBookTitleFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "20704052", db = "pubmed", endpoint = "efetch")
#' RetriveBookTitleFromPubmedEfetch(doc)
#' @import xml2
#'
RetriveBookTitleFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//BookTitle"))
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
#' @import xml2
#'
RetriveAbstractFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//Abstract"))
}

#' RetriveDOIFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  
#' doc <- GetDoc(id = "31524133", db = "pubmed", endpoint = "efetch")
#' RetriveDOIFromPubmedEfetch(doc)
#' 
#' @import xml2
#'
RetriveDOIFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//ELocationID[@EIdType = 'doi']"))
  # return(RetriveXmlNodeValuefromDoc(doc,  "//ArticleId[@IdType = 'doi']"))
}

#' RetriveIBSNFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "31524133", db = "pubmed", endpoint = "efetch")
#' RetriveIBSNFromPubmedEfetch(doc)
#'
RetriveIBSNFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//Isbn")[[1]])
}

#' RetriveVolumeFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "31524133", db = "pubmed", endpoint = "efetch")
#' RetriveVolumeFromPubmedEfetch(doc)
#'
RetriveVolumeFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//JournalIssue//Volume")[[1]])
}

#' RetriveIssueFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "31524133", db = "pubmed", endpoint = "efetch")
#' RetriveIssueFromPubmedEfetch(doc)
#'
RetriveIssueFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//JournalIssue//Issue"))
}

#' RetrivePagesFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "31524133", db = "pubmed", endpoint = "efetch")
#' RetrivePagesFromPubmedEfetch(doc)
#'
RetrivePagesFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//MedlinePgn"))
}

#' RetriveKeywordsFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples  doc <- GetDoc(id = "31524133", db = "pubmed", endpoint = "efetch")
#' RetriveKeywordsFromPubmedEfetch(doc)
#'
RetriveKeywordsFromPubmedEfetch <- function(doc){
  return(RetriveXmlNodeValuefromDoc(doc,  "//Keyword"))
}

#------- Multiple parser with pmids as input ----

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
#' @import xml2
#'
RetriveJournalWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)
    nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedArticle"))

    resultList <- sapply(nodesetList, function(x) {
      x <- xml2::as_xml_document(list(x))

      journal <- RetriveJournalFromPubmedEfetch(x)
      pmid <- RetrivePMIDFromPubmedEfetch(x)
      journalCountry <- RetriveJournalCountryFromPubmedEfetch(x)
      return(cbind(
        pmid,
        journal,
        journalCountry
      ))
    })
    
    result <- as.data.frame(t(resultList), stringsAsFactors = F)
    names(result) <- c("pmid","journal","journalCountry")
    return(result)
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
#' @import xml2
#'
RetriveFunderWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)
    nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedArticle"))
    
    resultList <- sapply(nodesetList, function(x) {
      x <- xml2::as_xml_document(list(x))
      
      funders <- RetriveFundersFromPubmedEfetch(x)
      pmid <- RetrivePMIDFromPubmedEfetch(x)
      return(cbind(
        pmid,
        funders
      ))
    })
    
    result <- as.data.frame(t(resultList), stringsAsFactors = F)
    names(result) <- c("pmid","funders")

    return(result)
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
#' @import xml2
#'
RetriveTiAbWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)
    nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedArticle"))
    
    resultList <- sapply(nodesetList, function(x) {
      x <- xml2::as_xml_document(list(x))
  
      title <- RetriveTitleFromPubmedEfetch(x)
      abstract <- RetriveAbstractFromPubmedEfetch(x)
      return(cbind(
        title,
        abstract
      ))
    })
    
    result <- as.data.frame(t(resultList), stringsAsFactors = F)
    names(result) <- c("title","abstract")
    
    return(result)
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
#' @import xml2
#'
RetrivePmcidWithPmids <-
  function(pmid,
           apiKey = "",
           email = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedArticle"))
    
    resultList <- sapply(nodesetList, function(x) {
      x <- xml2::as_xml_document(list(x))
      
      pmid <- RetrivePMIDFromPubmedEfetch(x)
      pmcid <- RetrivePMCIDFromPubmedEfetch(x)
      return(cbind(
        pmid,
        pmcid
      ))
    })
    
    result <- as.data.frame(t(resultList), stringsAsFactors = F)
    names(result) <- c("pmid","pmcid")
    
    return(result)
  }

#---------- Multiple parsers with pubmed efetch xml as input ----
#' RetriveMetaDataFromPubmedEfetch
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#' @param columns a list of string of requested column names
#' 
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  
#' doc <- GetDoc(id = c("28852052", "29041955","31230181"), db = "pubmed", endpoint = "efetch")
#' RetriveMetaDataFromPubmedEfetch(doc, columns = c("pmid","pmcid","title","doi"))
#'
#' doc <- GetDoc(id = c("31524133"), db = "pubmed", endpoint = "efetch")
#' RetriveMetaDataFromPubmedEfetch(doc, columns = c("pmid","pmcid","title","doi"))
#'
#' doc <- GetDoc(id = c("20704052"), db = "pubmed", endpoint = "efetch")
#' RetriveMetaDataFromPubmedEfetch(doc, columns = c("pmid","pmcid","title","doi"))
#'
#' @import xml2
#'
RetriveMetaDataFromPubmedEfetch <-
  function(doc, columns = "") {
    nodesetList1 <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedArticle"))
    nodesetList2 <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedBookArticle"))
    nodesetList <- c(nodesetList1, nodesetList2)
    resultList <- sapply(nodesetList, function(x) {
      # y <- xml2::xml_find_first(xml2::as_xml_document(x), "//MedlineCitation")
      article <- xml2::as_xml_document(list(x))
      
      pmid <- RetrivePMIDFromPubmedEfetch(article)
      pmcid <- RetrivePMCIDFromPubmedEfetch(article)
      journal <- RetriveJournalFromPubmedEfetch(article)
      journalCountry <- RetriveJournalCountryFromPubmedEfetch(article)
      publicationYear <- RetrivePublicationYearFromPubmedEfetch(article)
      funders <- RetriveFundersFromPubmedEfetch(article)
      authors <- RetriveAuthorsFromPubmedEfetch(article)
      affiliations <- RetriveAffiliationFromPubmedEfetch(article)
      title <- RetriveTitleFromPubmedEfetch(article)
      if(is.na(title)) title <- RetriveBookTitleFromPubmedEfetch(article)
      abstract <- RetriveAbstractFromPubmedEfetch(article)
      isbn <- RetriveIBSNFromPubmedEfetch(article)
      volume <- RetriveVolumeFromPubmedEfetch(article)
      issue <- RetriveIssueFromPubmedEfetch(article)
      pages <- RetrivePagesFromPubmedEfetch(article)
      keywords <- paste(RetriveKeywordsFromPubmedEfetch(article),collapse = "; ")
      doi <- RetriveDOIFromPubmedEfetch(article)
        
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
        abstract,
        isbn,
        volume,
        issue,
        pages,
        keywords,
        doi
      ))
    })

    result <- as.data.frame(t(resultList), stringsAsFactors = F)
    names(result) <- c("pmid","pmcid", "journal",
                       "journalCountry",
                       "publicationYear",
                       "funders",
                       "authors",
                       "affiliations",
                       "title",
                       "abstract","isbn",
                       "volume",
                       "issue",
                       "pages", "keywords","doi")
    if(length(columns) == 1 && all(columns == "")) columns <- names(result)
    
    return(result[, intersect(names(result), columns)])
  }

#' RetriveMetaDataFromPubmedEfetchParallel
#'
#' @description Good for used in local multiple xmls
#' @param files a list of string. A list of xml filenames
#' @param columns the columns of output requested

#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples 
#' baselink <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
#' link <- paste0(baselink, "?db=pubmed&id=20704052&retmode=xml")
#' RetriveMetaDataFromPubmedEfetchParallel(link)
#'
RetriveMetaDataFromPubmedEfetchParallel <- function(files, columns = "") {
  metaDataFromPMIDs <-
    sapply(
      files,
      function(x){
        doc <- xml2::read_xml(x, encoding = "UTF-8", useInternalNodes = TRUE, trim = FALSE)
        RetriveMetaDataFromPubmedEfetch(doc, columns = columns)
      }
    )
  return(as.data.frame(t(metaDataFromPMIDs), stringsAsFactors = F))
}

#---------- Multiple parsers with pmids as input ----
#' RetriveMetaDataFromPmids
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param outputFilename a string of characters. Output XML file name
#' @param columns the columns of output requested

#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  metaData <- RetriveMetaDataFromPmids(c("28852052", "29041955","31230181", "20704052"))
#'  metaData <- RetriveMetaDataFromPmids(c("20704052"))
#' @import xml2
#'
RetriveMetaDataFromPmids <-
  function(pmid,
           apiKey = "",
           email = "", outputFilename = "", columns = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch", apiKey = apiKey, email = email)

    if(outputFilename != "") outputFile <- xml2::write_xml(doc, file = outputFilename)
    results <- RetriveMetaDataFromPubmedEfetch(doc, columns = columns)
    
    return(results)
  }

#' RetriveMetaDataFromPmidsBatch
#'
#' @description this function is good for retrive data directly
#' @param pmids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param outputFileBaseName a string of characters. The base name of output xml files. If default, there will be no xml saved.
#' @param columns the columns of output requested
#' 
#' @return a nx7 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples RetriveMetaDataFromPmidsBatch(c("28852052", "29041955"))
#'
#' @import xml2
#'
RetriveMetaDataFromPmidsBatch <- function(pmids, apiKey = "", email = "", outputFileBaseName = "", columns = "") {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 400
  nloop <- ceiling(nids / grid)
  # results <- as.data.frame(matrix(nrow = nids, ncol = 16))

  for (iloop in 1:nloop) {
    iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)

    if(outputFileBaseName != ""){
      outputFilename <- paste0(
        gsub("[.]xml", "", outputFileBaseName),
        min(iindex),
        "_",
        max(iindex),
        ".xml"
      )
    } else {outputFilename <- ""}

    result <- RetriveMetaDataFromPmids(pmids[iindex], apiKey = apiKey, email = email, outputFilename = outputFilename, columns = columns)
    if(exists("results")) results[iindex, ] <- result else results <- result
    
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
#' @import xml2
#'
RetriveUrlFromPubmedElink <- function(doc, category = "All") {
  RetriveUrlFromPubmedElinkSingle <- function(node){
    # node <- xml2::as_xml_document(list(x))
    
    pmid <- node["Id"][[1]][[1]]
    objUrls <- node["ObjUrl"]
    myData <- as.data.frame(t(sapply(objUrls, function(x){
      url <- x["Url"][[1]]
      category <- x["Category"][[1]]
      return(cbind(url,category))
    })), stringsAsFactors = F, drop=F, colnames = c("Url", "Category"))
    
    names(myData) <- c("Url", "Category")
    
    if (category != "All") {
      index <- which(myData$Category == category)
      if (length(index) == 0) return(NULL)
      myData <- myData[index,]
    }
    
    return(data.frame(pmid = pmid, url = paste0(myData$Url, collapse = ";"), stringsAsFactors = F))
  }
  nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//IdUrlSet"))
  
  results <- sapply(nodesetList, RetriveUrlFromPubmedElinkSingle)
  return(as.data.frame(t(results)))
}

#' RetriveUrlsFromPmids
#'
#' @description Get related url links from given pmid. The number of pmids sent should be less than 500.
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
#' @import xml2
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
#' @import xml2
#'
RetriveUrlsFromPmidsBatch <- function(pmids, apiKey = "", email = "", fulltext = TRUE) {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids / grid)
  results <- as.data.frame(matrix(nrow = nids, ncol = 2))

  for (iloop in 1:nloop) {
    iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)
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
    as.data.frame(t(sapply(
      pmids,
      RetriveUrlsFromPmids,
      apiKey = apiKey,
      email = email,
      fulltext = fulltext
    )))
  return(urlFromPMIDs)
}

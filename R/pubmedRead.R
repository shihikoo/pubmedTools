
#' GetSearchSummaryWithSearch
#'
#' @param searchTerm a string of characters. search string
#' @param retmax a integer as the retmax parameter, the maximum id to render
#'
#' @return a list of strings - searchSummary
#' @export
#'
#' @examples
#' GetSearchSummaryWithSearch("pinkeye")
#'
#'
GetSearchSummaryWithSearch <- function(searchTerm, retmax = 1) {
  db <- "pubmed"
  endpoint = "esearch"

  result_json <-
    GetJson(term = searchTerm, db = db, endpoint = endpoint, usehistory = 'y', retmax = retmax)

  searchSummary <- result_json$esearchresult[c("count" ,"webenv" ,"querykey")]

  searchSummary$id <- paste(result_json$esearchresult$idlist, collapse = ",")

  return(searchSummary)
}

#' Retrive_pmid_with_TitleYear
#' Year is optional.
#'
#' @param title a string of characters. title of the publication
#' @param year a integer. Publication year. Optional
#' @param retmax a integer as the retmax parameter, the maximum id to render
#'
#' @return a list of strings - searchSummary
#' @export
#'
#' @examples
#' title <- "Causes of Acute Stroke: A Patterned Approach."
#' Retrive_pmid_with_TitleYear(title)
#' Retrive_pmid_with_TitleYear(title, 2019)
#'
#'
Retrive_pmid_with_TitleYear <- function(title, year = NULL, retmax = 1){
  db <- "pubmed"
  endpoint = "esearch"

  title_term <- paste0('"',title,'"')

  if(is.null(year)) term = title_term  else term <- paste(title_term, paste0('("', year, '/01/01"[Date - Publication] : "', year,'/12/31"[Date - Publication])'), sep = " AND ")

  result_json <-
    GetJson(term = term, db = db, endpoint = endpoint, usehistory = 'y', retmax = retmax)

  searchSummary <- result_json$esearchresult[c("count" ,"webenv" ,"querykey")]
  searchSummary$id <- paste(result_json$esearchresult$idlist, collapse = ",")

  return(searchSummary$id)
}

#' GetPmidsWithSearch
#'
#' @param searchTerm a string of characters. search string
#'
#' @return the output file names
#' @export
#'
#' @examples searchTerm = "pinkeye"
#' GetPmidsWithSearch(searchTerm)
#'
#' @import jsonlite
#'
GetPmidsWithSearch <- function(searchTerm) {
  db <- "pubmed"
  endpoint = "esearch"
  batchSize <- 500

  result_json <-
    GetJson(term = searchTerm, db = db, endpoint = endpoint, usehistory = 'y', retmax = batchSize)

  esearchresult <- result_json$esearchresult

  ids <- esearchresult$idlist
  nTotal <- as.numeric(esearchresult$count)
  while(length(ids) < nTotal){
    result_json <-
      GetJson(term = searchTerm, db = db, endpoint = endpoint, usehistory = T, retmax = batchSize, WebEnv = esearchresult$webenv, retstart = length(ids))

    esearchresult <- result_json$esearchresult

    ids <- c(ids, esearchresult$idlist)
  }

  return(ids)
}

#' DownloadJsonWithPmidsBatch
#'
#' @param pmids a string of character. PubMed central Id
#' @param endpoint a string of characters. The API endpoint to use. e.g. "esummary", "efetch"
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return the output file names
#' @export
#'
#' @examples
#' DownloadJsonWithPmidsBatch(c("28852052", "29041955"), endpoint = "efetch", fileBaseName="test.json")
#'
#' @import jsonlite
#'
DownloadJsonWithPmidsBatch <- function(pmids,  endpoint = "efetch", fileBaseName = "test.json") {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids / grid)
  outputFiles <- matrix("", nrow=nloop)
  for (iloop in 1:nloop) {
    iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)

    result_josn <-
      GetJson(id =pmids[iindex], db = db, endpoint = endpoint)

    filename <- paste0(
      gsub("[.]json", "", fileBaseName),
      min(iindex),
      "_",
      max(iindex),
      ".json"
    )

    outputFile <-
      jsonlite::write_json(result_josn, path = filename)
    outputFiles[iloop] <- filename
  }
  return(outputFiles)
}


#' DownloadXMLWithPmidsBatch
#'
#' @param pmids a string of character. PubMed central Id
#' @param endpoint a string of characters. The API endpoint to use. e.g. "esummary", "efetch"
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return the output file names
#' @export
#'
#' @examples
#' DownloadXMLWithPmidsBatch(c("28852052", "29041955"), endpoint = "efetch", fileBaseName="test.xml")
#'
#' @import xml2
#'
#'
DownloadXMLWithPmidsBatch <- function(pmids, endpoint = "esummary",fileBaseName = "test.xml") {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids / grid)
  outputFiles <- matrix("", nrow=nloop)
  for (iloop in 1:nloop) {
    iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)

    doc <-
      GetDoc(id =pmids[iindex], db = db, endpoint = endpoint)
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
  return(paste(RetriveXmlNodeValuefromDoc(doc,  "//AbstractText"), collapse = " "))
}

#' RetriveDOIFromPubmedEfetch
#'
#' @param doc an XMLInternalDocument class file read from a pubmed efetch xml file
#'
#' @return a string
#' @export
#'
#' @examples
#' doc <- GetDoc(id = "9214527", db = "pubmed", endpoint = "efetch")
#' RetriveDOIFromPubmedEfetch(doc)
#'
#' @import xml2
#'
RetriveDOIFromPubmedEfetch <- function(doc){
  doi <- RetriveXmlNodeValuefromDoc(doc,  "//ELocationID[@EIdType = 'doi']")
  if(is.na(doi)) doi = RetriveXmlNodeValuefromDoc(doc,  "//ArticleId[@IdType = 'doi']")
  return(doi)
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
#'
#' @return a nx3 data frame
#' @export
#'
#' @examples  journal <- RetriveJournalWithPmids(c("28852052", "29041955","31230181"))
#' @import xml2
#'
RetriveJournalWithPmids <-
  function(pmid) {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch")
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
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  RetriveFunderWithPmids(c("29041955","31230181"))
#' @import xml2
#'
RetriveFunderWithPmids <-
  function(pmid) {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch")
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
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  RetriveTiAbWithPmids(c("29041955","31230181"))
#' @import xml2
#'
RetriveTiAbWithPmids <-
  function(pmid) {

    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch")
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
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  pmcids <- RetrivePmcidWithPmids(c("28852052", "29041955","31230181"))
#' @import xml2
#'
RetrivePmcidWithPmids <-
  function(pmid) {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch")

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
#' @param columns a list of string of requested column names: "pmid","pmcid", "journal", "journalCountry","publicationYear","funders","authors","affiliations","title","abstract","isbn","volume","issue","pages", "keywords","doi"
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
  function(doc, columns = c("pmid", "title", "abstract","fulltext","studytype", "journal", "publisher", "publicationDate","authors", "emails","affiliations", "correspondingAuthors","correspondingAuthorAffs")) {
    nodesetList1 <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedArticle"))
    nodesetList2 <- xml2::as_list(xml2::xml_find_all(doc, "//PubmedBookArticle"))
    nodesetList <- c(nodesetList1, nodesetList2)
    resultList <- sapply(nodesetList, function(x) {
      # y <- xml2::xml_find_first(xml2::as_xml_document(x), "//MedlineCitation")
      article <- xml2::as_xml_document(list(x))

      if("pmid" %in% columns) pmid <- RetrivePMIDFromPubmedEfetch(article) else pmid <- ""
      if("pmcid" %in% columns) pmcid <- RetrivePMCIDFromPubmedEfetch(article) else pmcid <- ""
      if("journal" %in% columns) journal <- RetriveJournalFromPubmedEfetch(article) else journal <- "" 
      if("journalCountry" %in% columns) journalCountry <- RetriveJournalCountryFromPubmedEfetch(article) else journalCountry <- ""
      if("publicationYear" %in% columns) publicationYear <- RetrivePublicationYearFromPubmedEfetch(article) else publicationYear <- ""
      if("funders" %in% columns) funders <- RetriveFundersFromPubmedEfetch(article) else funders <- "" 
      if("authors" %in% columns) authors <- RetriveAuthorsFromPubmedEfetch(article) else authors <- ""
      if("affiliations" %in% columns) affiliations <- RetriveAffiliationFromPubmedEfetch(article) else affiliations <- ""
      if("title" %in% columns) {
        title <- RetriveTitleFromPubmedEfetch(article)
      if(is.na(title)) title <- RetriveBookTitleFromPubmedEfetch(article)
      } else title <- ""
      if("abstract" %in% columns) abstract <- RetriveAbstractFromPubmedEfetch(article) else abstract <- ""
      if("isbn" %in% columns) isbn <- RetriveIBSNFromPubmedEfetch(article) else isbn <- ""
      if("volume" %in% columns) volume <- RetriveVolumeFromPubmedEfetch(article) else volume <- ""
      if("issue" %in% columns) issue <- RetriveIssueFromPubmedEfetch(article) else issue <- ""
      if("pages" %in% columns) pages <- RetrivePagesFromPubmedEfetch(article) else pages <- ""
      if("keywords" %in% columns) keywords <- paste(RetriveKeywordsFromPubmedEfetch(article),collapse = "; ") else keywords <- ""
      if("doi" %in% columns) doi <- RetriveDOIFromPubmedEfetch(article) else doi <- ""

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
    # print(dim(result))
    names(result) <-  c("pmid","pmcid", "journal", "journalCountry","publicationYear", "funders", "authors","affiliations","title","abstract","isbn","volume","issue", "pages", "keywords","doi")
    if(length(columns) == 1 && all(columns == "")) columns <- names(result)
    
    return(result[, intersect(names(result), columns)])
  }

#' RetriveMetaDataFromPubmedEfetchParallel
#'
#' @description Good for used in local multiple xmls
#' @param files a list of string. A list of xml filenames
#' @param columns a list of string of requested column names: "pmid","pmcid", "journal", "journalCountry","publicationYear","funders","authors","affiliations","title","abstract","isbn","volume","issue","pages", "keywords","doi"
#' 
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
#' @param outputFilename a string of characters. Output XML file name
#' @param columns a list of string of requested column names: "pmid","pmcid", "journal", "journalCountry","publicationYear","funders","authors","affiliations","title","abstract","isbn","volume","issue","pages", "keywords","doi"
#' 
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  metaData <- RetriveMetaDataFromPmids(c("28852052", "29041955","31230181", "20704052"))
#'  metaData <- RetriveMetaDataFromPmids(c("20704052"))
#' @import xml2
#'
RetriveMetaDataFromPmids <-
  function(pmid, outputFilename = "", columns = "") {
    doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "efetch")

    if(outputFilename != "") {
      outputFile <- xml2::write_xml(doc, file = outputFilename)
      print(paste0("Save file ", outputFilename))
    }
    results <- merge(data.frame(pmid = pmid)
                     , RetriveMetaDataFromPubmedEfetch(doc, columns = columns),by="pmid" )

    if(outputFilename != "") {
    print(paste0("Completed retrive results for", outputFilename))
    }
    return(results)
  }

#---------- Multiple parsers with pmids as input ----
#' RetriveMetaDataFromSearch
#'
#' @param searchTerm a string of character. search term
#' @param columns a list of string of requested column names: "pmid","pmcid", "journal", "journalCountry","publicationYear","funders","authors","affiliations","title","abstract","isbn","volume","issue","pages", "keywords","doi"
#' @param outputFilename a string of characters. Output XML file name
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples
#' searchTerm <-  "pinkeye"
#' metaData <- RetriveMetaDataFromSearch(searchTerm)
#'
#' @import xml2 utils
#'
RetriveMetaDataFromSearch <-
  function(searchTerm, columns = "", outputFilename="") {
    searchSummary <- GetSearchSummaryWithSearch(searchTerm)

    db <- "pubmed"
    endpoint <- "efetch"
    webenv <- searchSummary$webenv
    queryKey <- searchSummary$querykey

    nids <- as.numeric(searchSummary$count)
    print(paste(nids, "publications found by the search."))
    grid <- 500
    nloop <- ceiling(nids / grid)
    outputFileBaseName <- outputFilename
    for (iloop in 1:nloop) {
      iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)

      if(outputFileBaseName != ""){
        xmloutputFilename <- paste0(
           outputFileBaseName,
          min(iindex),
          "_",
          max(iindex),
          ".xml"
        )

        csvoutputFilename <- paste0(
          outputFileBaseName,
          min(iindex),
          "_",
          max(iindex),
          ".csv"
        )

      } else {xmloutputFilename <- ""
      csvoutputFilename <- ""}

      doc <- GetDoc(db = db, endpoint =endpoint , WebEnv = webenv, retmax = grid, queryKey=queryKey, retstart = iindex[1]-1)

      if(xmloutputFilename != "") {
        xml2::write_xml(doc, file = xmloutputFilename)
        print(paste0("Save file ", xmloutputFilename))
      }

      result <- RetriveMetaDataFromPubmedEfetch(doc, columns = columns)

      if(xmloutputFilename != "") {
        utils::write.csv(result, file = csvoutputFilename)
        print(paste0("Save file ", csvoutputFilename))
      }
  
      print(paste("To retrieve publications: ", min(iindex), "-", max(iindex)))
      print(paste("Retrieved publications: ", nrow(result)))

      if(exists("results")) results <- rbind(results, result) else results <- result
    }

    return(results)
  }


#' RetriveMetaDataFromPmidsBatch
#'
#' @description this function is good for retrive data directly
#' @param pmids a string of character. PubMed central Id
#' @param outputFileBaseName a string of characters. The base name of output xml files. If default, there will be no xml saved.
#' @param columns a list of string of requested column names: "pmid","pmcid", "journal", "journalCountry","publicationYear","funders","authors","affiliations","title","abstract","isbn","volume","issue","pages", "keywords","doi"
#' @param grid the number of ids to run in each loop
#'
#' @return a nx7 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples RetriveMetaDataFromPmidsBatch(c("28852052", "29041955"))
#'
#' @import xml2
#'
RetriveMetaDataFromPmidsBatch <- function(pmids, outputFileBaseName = "", columns = "", grid = 500) {
  db <- "pubmed"
  nids <- length(pmids)
  nloop <- ceiling(nids / grid)

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

    result <- RetriveMetaDataFromPmids(pmids[iindex], outputFilename = outputFilename, columns = columns)

    # if(exists("results")) results[iindex, ] <- result else results <- result

    if(exists("results")) results <- rbind(results, result) else results <- result

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
RetriveUrlsFromPmids <- function(pmid, fulltext = TRUE) {
  doc <- GetDoc(id = pmid, db = "pubmed", endpoint = "elink", cmd = "llinks")
  if (fulltext == T) category <- "Full Text Sources" else category = "All"

  urls <- RetriveUrlFromPubmedElink(doc, category)

  if (is.null(urls)) return(NULL)
  return(urls[,])
}

#' RetriveUrlsFromPmidsBatch
#'
#' @description this function is good for retrive data directly
#' @param pmids a string of character. PubMed central Id
#' @param fulltext a boolean. Whether or not only retrive full text
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples RetriveUrlsFromPmidsBatch(c("28852052", "29041955"))
#'
#' @import xml2
#'
RetriveUrlsFromPmidsBatch <- function(pmids, fulltext = TRUE) {
  db <- "pubmed"
  nids <- length(pmids)
  grid <- 200
  nloop <- ceiling(nids / grid)
  results <- as.data.frame(matrix(nrow = nids, ncol = 2))

  for (iloop in 1:nloop) {
    iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)
    result <- RetriveUrlsFromPmids(pmids[iindex],  fulltext = fulltext)

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
#' @param fulltext a boolean. If TRUE, function only searches for full text link
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#' @export
#'
#' @examples  RetriveUrlsFromPmidParallel(c("28852052", "29041955"), "")
#'
RetriveUrlsFromPmidParallel <- function(pmids, fulltext = TRUE) {
  urlFromPMIDs <-
    as.data.frame(t(sapply(
      pmids,
      RetriveUrlsFromPmids,
      fulltext = fulltext
    )))
  return(urlFromPMIDs)
}

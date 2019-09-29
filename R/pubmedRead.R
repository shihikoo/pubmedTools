#' GetBaselink
#'
#' @param db a string of characters, valid database name from NCBI: https://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.chapter2_table1
#' @param ids a string of characters, valid id matching the database
#' @param apiKey a string of characters, API Key of the user
#' @param email a string of characters, email address of the user
#'
#' @return a list of links
#' @export
#'
#' @examples links <- GetBaselink("pubmed", "4804230", "","")
#'
GetBaselink <- function(db,ids, apiKey = "", email = ""){
  idsStr <- paste0(ids,collapse = ",")
  baseUrl <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  links <- data.frame(
    elink = paste0(baseUrl, "elink.fcgi?dbfrom=",db,"&cmd=llinks&id=",idsStr),
    efetch = paste0(baseUrl, "efetch.fcgi?db=",db,"&id=",idsStr,"&retmode=xml"),
    esummary = paste0(baseUrl, "esummary.fcgi?db=",db,"&id=",idsStr),
    stringsAsFactors = F
  )
  if(apiKey != "") links <- sapply(links, function(x)  paste0(x, "&api_key=", apiKey))
  if(email != "") links <- sapply(links, function(x)  paste0(x, "&email=", email))

  return(links)
}

#' GetContentWithLink
#'
#' @param link a string of characters
#' @param waitTime a number. Waiting of the program
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples baselink <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
#' link <- paste0(baselink, "efetch.fcgi?db=pubmed&rettype=xml&id=26502666")
#' content <- GetContentWithLink(link, 0.3)
#'
#' @import httr
#'
GetContentWithLink <- function(link, waitTime){
  httr::set_config(httr::config(http_version = 0))
  content = NULL
  while(is.null(content)){
    tryCatch({
      Sys.sleep(waitTime)
      r0 <- httr::GET(as.character(link))
      content <- httr::content(r0, "text")
    }, error=function(e){
      print(e)
    })
  }
  return(content)
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
  nodes <- XML::xpathApply(doc, nodePosition)
  if(length(nodes) == 0) return(NA)
  results <- sapply(nodes, XML::xmlValue)
  return(results)
}

#' GetXmlDocFromIds
#'
#' @param ids a string of chacters. Valid NCBI ids
#' @param database a string of chacters. Valid NCBI
#' @param target a string of chacters. "efetch", "esummary", "elink"
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Time to wait during the http quest
#'
#' @return a XMLInternalDocument
#' @export
#'
#' @import XML
#'
#' @examples GetXmlDocFromIds("5575286", "pmc", "efetch", "", "", 0.3)
#'
GetXmlDocFromIds <- function(ids, database, target, apiKey, email, waitTime){
  links <- GetBaselink(database, ids, apiKey, email)
  content <- GetContentWithLink(links[target], waitTime)
  doc <- XML::xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)
  return(doc)
}

#' ReadPmidDoiFromPmcidEsummaryDoc
#'
#' @param doc a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return extract pmid and doi for all articles in the doc
#' @export
#'
#' @examples  doc <- GetXmlDocFromIds(c("5575286", "4804230"), "pmc", "esummary", "", "", 0)
#' ReadPmidDoiFromPmcidEsummaryDoc(doc)
#'
#' @import XML
#'
ReadPmidDoiFromPmcidEsummaryDoc <- function(doc){
  results <- do.call(rbind, XML::xpathApply(doc, "//DocSum", function(x)
  {
    node <- XML::xmlDoc(x)
    pmid <- RetriveXmlNodeValuefromDoc(node,  "//Item[@Name='ArticleIds']//Item[@Name='pmid']")
    doi <- RetriveXmlNodeValuefromDoc(node,  "//Item[@Name='ArticleIds']//Item[@Name='doi']")
    return(cbind(pmid, doi))
  }))
  return(as.data.frame(results, stringsAsFactors = F))
}

#' GetPmidDoiFromPmcid
#'
#' @param pmcid a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#' @param writeFileName a string of characters.
#'
#' @return a string: pmid
#' @export
#'
#' @examples GetPmidDoiFromPmcid(c("5575286", "4804230"))
#'
#' @import XML
#'
GetPmidDoiFromPmcid <- function(pmcid, apiKey="", email="", waitTime=0.3, writeFileName=""){
  doc <- GetXmlDocFromIds(pmcid, "pmc", "esummary", apiKey, email, waitTime)
  if(writeFileName != "") XML::saveXML(doc, file = writeFileName)
  result <- ReadPmidDoiFromPmcidEsummaryDoc(doc)
  return(result)
}

#' GetPmidDoiFromPmcidBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples pmid <- GetPmidDoiFromPmcidBatch(c("5575286", "4804230"), waitTime = 0.3)
#'
#' @import XML
#'
GetPmidDoiFromPmcidBatch <- function(pmcids, apiKey="", email="", waitTime = 0){
  nids <- length(pmcids)
  grid <- 500
  nloop <- ceiling(nids/grid)
  results <- as.data.frame(matrix(nrow = nids, ncol = 3))
  colnames(results) <- c("pmcid", "pmid", "doi")
  for(iloop in 1:nloop){
    iindex <- (((iloop-1)*grid)+1) : ifelse(iloop*grid > nids, nids,iloop*grid)
    results[iindex,1] <- pmcids[iindex]
    results[iindex,2:3] <- GetPmidDoiFromPmcid(pmcids[iindex], apiKey, email, waitTime)
  }
  return(results)
}

#' DownloadMetaDataWithPmcidsBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples DownloadMetaDataWithPmcidsBatch(c("5575286", "4804230"), fileBaseName="test.xml")
#'
#' @import XML
#'
DownloadMetaDataWithPmcidsBatch <- function(pmcids, apiKey="", email="", waitTime=0, fileBaseName=""){
  nids <- length(pmcids)
  grid <- 500
  nloop <- ceiling(nids/grid)
  for(iloop in 1:nloop){
    iindex <- ((iloop-1)*grid)+1 : ifelse(iloop*grid > nids, nids,iloop*grid)
    doc <- GetXmlDocFromIds(pmcids[iindex], "pmc", "efetch", apiKey, email, waitTime)
    outputFile <- XML::saveXML(doc, file = paste0(gsub("[.]xml","", fileBaseName), min(iindex),"_", max(iindex), ".xml"))
  }
  return(nloop)
}

#' DownloadMetaDataWithPmidsBatch
#'
#' @param pmids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples DownloadMetaDataWithPmidsBatch(c("28852052", "29041955","31230181"),fileBaseName="test.xml")
#'
#' @import XML
#'
DownloadMetaDataWithPmidsBatch <- function(pmids, apiKey="", email="", waitTime=0, fileBaseName=""){
  nids <- length(pmids)
  grid <- 500
  nloop <- ceiling(nids/grid)
  for(iloop in 1:nloop){
    iindex <- ((iloop-1)*grid)+1 : ifelse(iloop*grid > nids, nids,iloop*grid)
    doc <- GetXmlDocFromIds(pmids[iindex], "pubmed", "efetch", apiKey, email, waitTime)
    outputFile <- XML::saveXML(doc, file = paste0(gsub("[.]xml","", fileBaseName), min(iindex),"_", max(iindex), ".xml"))
  }
  return(nloop)
}

#' ReadMetaDataFromPmcidEfetchDoc
#'
#' @param doc a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples  doc <- GetXmlDocFromIds(c("3324826", "3339580"), "pmc", "efetch", "", "", 0)
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#'
#' @import XML
#'
ReadMetaDataFromPmcidEfetchDoc <- function(doc){
  results <- do.call(rbind, XML::xpathApply(doc, "//article", function(x){
    article <- XML::xmlDoc(x)
    pmid <- RetriveXmlNodeValuefromDoc(article,  "//article-id[@pub-id-type='pmid']")
    journal <- RetriveXmlNodeValuefromDoc(article,  "//journal-title")
    journalLocation <- RetriveXmlNodeValuefromDoc(article,  "//publisher")

    epubDateNode <- RetriveXmlNodeValuefromDoc(article,  "//pub-date[@pub-type='epub']")
    if(!is.null(epubDateNode) && length(epubDateNode) > 0 && !is.na(epubDateNode) & !is.null(epubDateNode)){
      publicationDate <- paste(RetriveXmlNodeValuefromDoc(article,  "//pub-date[@pub-type='epub']//year")
                               ,RetriveXmlNodeValuefromDoc(article,  "//pub-date[@pub-type='epub']//month"), sep="-")

    }else{
      publicationDate <- paste(RetriveXmlNodeValuefromDoc(article,  "//pub-date[@pub-type='ppub']//year")
                               ,RetriveXmlNodeValuefromDoc(article,  "//pub-date[@pub-type='ppub']//month"), sep="-")
    }

    authorsNode <- XML::xpathApply(article,  "//contrib[@contrib-type='author']//name")
    if(is.null(authorsNode)) {
      authors = NA
    } else {
      authors <- do.call(rbind, XML::xpathApply(article,  "//contrib[@contrib-type='author']//name", function(author){
        forename <- XML::xmlValue(author[["given-names"]])
        lastname <- XML::xmlValue(author[["surname"]])
        return(paste(forename, lastname))
      }))
    }

    if(!is.null(authors) && length(authors) > 0 && !is.na(authors)) {
      authors <- paste(authors, collapse = "; ")
      # affiliation <- paste0(unique(RetriveXmlNodeValuefromDoc(article,  "//aff")), collapse = "; ")

      correspondingAuthorsNode <-  XML::xpathApply(article,  "//contrib-group//contrib[@corresp='yes']//name")
      if(is.null(correspondingAuthorsNode) | length(correspondingAuthorsNode) == 0) {
        correspondingAuthors <- authors
      } else{
        correspondingAuthors <- do.call(rbind, XML::xpathApply(article,  "//contrib-group//contrib[@corresp='yes']//name" , function(corrAuthor){
          forename <- XML::xmlValue(corrAuthor[["given-names"]])
          lastname <- XML::xmlValue(corrAuthor[["surname"]])
          return(paste(forename, lastname))
        }))
      }

      if(!is.null(correspondingAuthors) && length(correspondingAuthors) > 0 && !is.na(correspondingAuthors[[1]]))  correspondingAuthors <- paste(correspondingAuthors, collapse = "; ")

      affiliations <- XML::xpathApply(article,  "//aff")
      correspondingAuthorAffIdsNode <- XML::xpathApply(article,  "//contrib-group//contrib[@corresp='yes']//xref[@ref-type='aff']")
      if(is.null(correspondingAuthorAffIdsNode)) {
        correspondingAuthorAffIds <- NA
        correspondingAuthorAffs <- NA
      } else {
        correspondingAuthorAffIds <- unique(do.call(rbind, XML::xpathApply(article,  "//contrib-group//contrib[@corresp='yes']//xref[@ref-type='aff']", function(corrAuthorAff){
          # affid <- as.numeric(XML::xmlValue(corrAuthorAff))
          affid <- as.numeric(gsub("Aff","",xmlAttrs(corrAuthorAff)["rid"]))
          # affid <- as.numeric(XML::xmlAttrs (corrAuthorAff))
          return(affid)
        })))
        correspondingAuthorAffs <- paste0(sapply(affiliations[correspondingAuthorAffIds], XML::xmlValue,recursive=F),collapse = "; ")
      }

      affiliations <- ifelse(is.null(affiliations), NA, paste0(unique(sapply(affiliations, XML::xmlValue,recursive=F)),collapse = "; "))
      if(length(correspondingAuthorAffs) > 0 & is.na(correspondingAuthorAffs[[1]]))correspondingAuthorAffs <- affiliations
      return(cbind(pmid,journal, journalLocation, publicationDate, authors
                   , correspondingAuthors, affiliations, correspondingAuthorAffs))
    } else {cbind(pmid,journal, journalLocation, publicationDate, NA, NA, NA, NA)
    }}))

  return(as.data.frame(results, stringsAsFactors = F))
}

#' GetMetaDataFromPmcid
#'
#' @param pmcid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#' @param writeFileName a string of characters. The file name you would like to download to.
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples GetMetaDataFromPmcid(c("5575286", "4804230"))
#'
#' @import XML
#'
GetMetaDataFromPmcid <- function(pmcid, apiKey="", email="", waitTime=0.3, writeFileName = ""){
  doc <- GetXmlDocFromIds(pmcid, "pmc", "efetch", apiKey, email, waitTime)
  if(writeFileName != "") XML::saveXML(doc, file = writeFileName)
  result <- ReadMetaDataFromPmcidEfetchDoc(doc)
  return(result)
}

#' GetMetaDataFromPmcidBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#' @param writeFileName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples GetMetaDataFromPmcidBatch(c("5575286", "4804230"))
#'
#' @import XML
#'
GetMetaDataFromPmcidBatch <- function(pmcids, apiKey="", email="", waitTime=0, writeFileName=""){
  nids <- length(pmcids)
  grid <- 500
  nloop <- ceiling(nids/grid)
  results <- as.data.frame(matrix(nrow = nids, ncol = 9))
  # colnames(results) <- c("pmcid", "pmid", "doi")
  for(iloop in 1:nloop){
    iindex <- (((iloop-1)*grid)+1) : ifelse(iloop*grid > nids, nids,iloop*grid)
    results[, 1] <- pmcids[iindex]
    temp <- GetMetaDataFromPmcid(pmcids[iindex], apiKey=apiKey, email=email, writeFileName = writeFileName)
    results[, -1] <- temp
    colnames(results) <- c("pmcid",names(temp))
  }
  return(results)
}

#' GetMetaDataFromPmid
#'
#' @param pmid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  metaData1 <-  GetMetaDataFromPmid(c("28852052", "29041955","31230181"))
#' @import XML
#'
GetMetaDataFromPmid <- function(pmid, apiKey="", email="", waitTime=0.3){
  GetEfetchContentFromPmid <- function(pmid, apiKey, email, waitTime){
    links <- GetBaselink("pubmed", pmid, apiKey, email)
    content <- GetContentWithLink(links["efetch"], waitTime)
    return(content)
  }

  content <- GetEfetchContentFromPmid(pmid, apiKey, email, waitTime)
  if(is.null(content)) {return (NULL)}

  doc <- XML::xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

  results <- do.call(rbind, XML::xpathApply(doc, "//PubmedArticle", function(x){
    article <- XML::xmlDoc(x)
    pmid <- RetriveXmlNodeValuefromDoc(article,  "//PMID")
    journal <- RetriveXmlNodeValuefromDoc(article,  "//Journal//Title")
    journalCountry <- RetriveXmlNodeValuefromDoc(article,  "//MedlineJournalInfo//Country")
    publicationYear <- RetriveXmlNodeValuefromDoc(article,  "//JournalIssue//PubDate//Year")

    authors <- do.call(rbind, XML::xpathApply(article, "//Author", function(subnode)
    {
      forename <- XML::xmlValue(subnode[["ForeName"]])
      lastname <- XML::xmlValue(subnode[["LastName"]])
      return(paste(forename, lastname))
    }))
    authors <- paste(authors, collapse = "; ")
    affiliations <- paste0(unique(RetriveXmlNodeValuefromDoc(article,  "//Affiliation")), collapse = "; ")

    return(cbind(pmid,journal, journalCountry,publicationYear, authors,affiliations))
  }))

  return( as.data.frame(results, stringsAsFactors = F))
}

#' RetriveMetaDataFromPmids
#'
#' @param pmids a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples  metaData <-  RetriveMetaDataFromPmids(c("28852052", "29041955"))
#'
RetriveMetaDataFromPmids <- function(pmids, apiKey = "", email="", waitTime=0.3){
  metaDataFromPMIDs <- sapply(pmids, GetMetaDataFromPmid, apiKey = apiKey, email = email, waitTime = waitTime)
  return(as.data.frame(t(metaDataFromPMIDs)))
}

#' GetUrlsFromPmid
#'
#' Get related full text urls from given pmid
#'
#' @param pmid a number or a string of characters. The number of pmid.
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param waitTime a number. Waiting of the program
#' @param fulltext a boolean. If TRUE, function only searches for full text link
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#'
#' @export
#'
#' @examples GetUrlsFromPmid("28852052", "", "",0.3)
#'
#' @import XML
#'
GetUrlsFromPmid <- function(pmid, apiKey="", email="", waitTime = 0.3, fulltext = TRUE){
  GetUrlsContentWithPmid <- function(pmid, apiKey, email, waitTime){
    links <- GetBaselink("pubmed", pmid, apiKey, email)
    content <- GetContentWithLink(links["elink"], waitTime)
    return(content)
  }
  RetriveUrlfromContent <-function(content, category = "All"){
    doc <- XML::xmlTreeParse(content, encoding="UTF-8", useInternalNodes = TRUE)

    myData <- do.call(rbind, XML::xpathApply(doc, "//ObjUrl", function(node)
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

  content <- GetUrlsContentWithPmid(pmid, apiKey, email, waitTime)
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
#' @param waitTime a number. Waiting of the program
#' @param fulltext a boolean. If TRUE, function only searches for full text link
#'
#' @return a list of characters. A list of urls. Return fulltext urls if fulltext parameter is T.
#' Return NULL if none is found.
#' @export
#'
#' @examples  RetriveUrlsFromPmids(c("28852052", "29041955"), "", "", 0.3, TRUE)
#'
RetriveUrlsFromPmids <- function(pmids, apiKey="", email="", waitTime=0.3, fulltext = TRUE){
  urlFromPMIDs <- sapply(pmids, GetUrlsFromPmid, apiKey = apiKey, email = email, waitTime = waitTime, fulltext = fulltext)
  return(urlFromPMIDs)
}

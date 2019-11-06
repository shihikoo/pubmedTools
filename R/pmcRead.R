#' ReadPmidDoiFromPmcidEsummaryDoc
#'
#' @param doc a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return extract pmid and doi for all articles in the doc
#' @export
#'
#' @examples  doc <- GetDoc(id=c("5575286", "4804230"), db="pmc", endpoint="esummary")
#' ReadPmidDoiFromPmcidEsummaryDoc(doc)
#'
#' @import XML
#'
ReadPmidDoiFromPmcidEsummaryDoc <- function(doc) {
  results <-
    do.call(rbind, XML::xpathApply(doc, "//DocSum", function(x)
    {
      node <- XML::xmlDoc(x)
      pmid <-
        RetriveXmlNodeValuefromDoc(node,  "//Item[@Name='ArticleIds']//Item[@Name='pmid']")
      doi <-
        RetriveXmlNodeValuefromDoc(node,  "//Item[@Name='ArticleIds']//Item[@Name='doi']")
      return(cbind(pmid, doi))
    }))
  return(as.data.frame(results, stringsAsFactors = F))
}

#' GetPmidDoiFromPmcid
#'
#' @param pmcid a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param writeFileName a string of characters.
#'
#' @return a string: pmid
#' @export
#'
#' @examples GetPmidDoiFromPmcid(c("5575286", "4804230"))
#'
#' @import XML
#'
GetPmidDoiFromPmcid <-
  function(pmcid,
           apiKey = "",
           email = "",
           writeFileName = "") {
    doc <-
      GetDoc(id=pmcid,db= "pmc", endpoint="esummary",apiKey= apiKey,email= email)
    if (writeFileName != "")
      XML::saveXML(doc, file = writeFileName)
    result <- ReadPmidDoiFromPmcidEsummaryDoc(doc)
    return(result)
  }

#' GetPmidDoiFromPmcidBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples pmid <- GetPmidDoiFromPmcidBatch(c("5575286", "4804230"))
#'
#' @import XML
#'
GetPmidDoiFromPmcidBatch <-
  function(pmcids,
           apiKey = "",
           email = "") {
    nids <- length(pmcids)
    grid <- 500
    nloop <- ceiling(nids / grid)
    results <- as.data.frame(matrix(nrow = nids, ncol = 3))
    colnames(results) <- c("pmcid", "pmid", "doi")
    for (iloop in 1:nloop) {
      iindex <-
        (((iloop - 1) * grid) + 1):ifelse(iloop * grid > nids, nids, iloop * grid)
      results[iindex, 1] <- pmcids[iindex]
      results[iindex, 2:3] <-
        GetPmidDoiFromPmcid(pmcids[iindex], apiKey, email)
    }
    return(results)
  }

#' DownloadMetaDataWithPmcidsBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples DownloadMetaDataWithPmcidsBatch(c("5575286", "4804230"), fileBaseName="test.xml")
#'
#' @import XML
#'
DownloadMetaDataWithPmcidsBatch <-
  function(pmcids,
           apiKey = "",
           email = "",
           fileBaseName = "") {
    nids <- length(pmcids)
    grid <- 500
    nloop <- ceiling(nids / grid)
    for (iloop in 1:nloop) {
      iindex <-
        ((iloop - 1) * grid) + 1:ifelse(iloop * grid > nids, nids, iloop * grid)
      doc <-
        GetDoc(id = pmcids[iindex], db="pmc", endpoint = "efetch",apiKey= apiKey, email = email)
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

#' GetEpubDateFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string: publicationDate
#' @export
#'
#' @examples doc <- GetDoc(id=c("5304250","4415024","4804230"),db= "pmc", endpoint="efetch")
#' GetEpubDateFromPmcidEfetchDoc(doc)
#'
#' @import XML
#'
GetEpubDateFromPmcidEfetchDoc <- function(article){
  epubYear <-
    RetriveXmlNodeValuefromDoc(article,  "//article-meta//pub-date[@pub-type='epub']//year")
  epubMonth <-
    RetriveXmlNodeValuefromDoc(article,  "//article-meta//pub-date[@pub-type='epub']//month")
  epubDate <- paste(na.omit(c(epubYear[[1]], epubMonth[[1]])), collapse = "-" )

  ppubYear <-
    RetriveXmlNodeValuefromDoc(article,  "//article-meta//pub-date[@pub-type='ppub']//year")
  ppubMonth <-
    RetriveXmlNodeValuefromDoc(article,  "//article-meta//pub-date[@pub-type='ppub']//month")
  ppubDate <- paste(na.omit(c(ppubYear[[1]], ppubMonth[[1]])), collapse = "-" )

  allYear <-
    RetriveXmlNodeValuefromDoc(article,  "//article-meta//pub-date//year")
  allMonth <-
    RetriveXmlNodeValuefromDoc(article,  "//article-meta//pub-date//month")
  allDate <- paste(na.omit(c(allYear[[1]], allMonth[[1]])), collapse = "-" )

  if(epubDate != "") publicationDate <- epubDate
  else if(ppubDate != "") publicationDate <- ppubDate
  else publicationDate <- allDate

  temp <- gsub("\n", "", publicationDate, fixed = T)
  temp <- gsub(" ", "", temp, fixed = T)
  temp <- gsub("^0", "", temp)
  publicationDate <- temp

  return(publicationDate)
}

#' ReadMetaDataFromPmcidEfetchDoc
#'
#' @param doc a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples  doc <- GetDoc(id=c("2823164", "3324826", "3339580"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id=c("4812069","4405051","4804230","3892617"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id=c("5304250","4415024","4804230"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#'
#' @import XML stringr stats
#'
ReadMetaDataFromPmcidEfetchDoc <- function(doc) {
  retrivePMID <- function(article){
    output <- unique(RetriveXmlNodeValuefromDoc(article,  "//article-id[@pub-id-type='pmid']") )
    if(length(output) >0 ) return(output[[1]]) else return(output)  }
  retriveJournal <- function(article){
    output <- unique(RetriveXmlNodeValuefromDoc(article,  "//journal-title"))
    if(length(output > 0)) return(output[[1]]) else return(output) }
  retriveJournalLocation <- function(article){
    output <- unique(RetriveXmlNodeValuefromDoc(article,  "//publisher"))
    if(length(output) >0 ) return(output[[1]]) else return(output) }
  retriveEmails <- function(article){
    emailList <- unique(stringr::str_extract_all(RetriveXmlNodeValuefromDoc(article,  "//email"), "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+[.][a-zA-Z]{2,}", simplify = T))
    validIndex <- which(!is.na(emailList) & emailList != "")
    if(length(validIndex) > 0)    emails <- paste0(emailList[validIndex], collapse = "||") else emails <- NA
    return(emails)
    }
  retriveAffliation <- function(article){
    # paste0(gsub("^[0-9]+", "", stats::na.omit( unique(RetriveXmlNodeValuefromDoc(article,  "//aff")))), collapse = "||")
      nodes <-XML::xpathApply(article,  paste0("//aff"))
      if(length(nodes) == 0 ) return(NA)

      affList <- sapply(nodes, function(node){
        childValues <- sapply(XML::xmlChildren(node), XML::xmlValue)
        index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues))
        if(length(index) > 0)  return(paste(childValues[index], collapse = ", ")) else return(NA)
      })

      validIndex <- which(!is.na(affList) & affList != "")
      if(length(validIndex) > 0)    correspondingAuthorAffs <- paste0(affList[validIndex], collapse = "||") else correspondingAuthorAffs <- NA

    if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "")correspondingAuthorAffs <- NA
    return(correspondingAuthorAffs)
  }
  retriveAuthor <- function(article){
    authorsNode <- XML::xpathApply(article,  "//contrib[@contrib-type='author']//name")
    if (is.null(authorsNode)| length(authorsNode) == 0)  return(NA)
    authors <-
      do.call(
        rbind,
        XML::xpathApply(article,  "//contrib[@contrib-type='author']//name", function(author) {
          forename <- XML::xmlValue(author[["given-names"]])
          lastname <- XML::xmlValue(author[["surname"]])
          return(paste(stats::na.omit(forename), stats::na.omit(lastname)))
        })
      )

    authors <- paste0(stats::na.omit( unique(authors)), collapse = "||")
    if(authors == "") return(NA)
    return(authors)
  }
  retriveCorrespondingAuthor <- function(article){
    correspondingAuthorsNode1 <- XML::xpathApply(article, "//contrib[@corresp='yes']")    #schema 1:"3324826"
    correspondingAuthorsNode2 <- XML::xpathApply(article, "//xref[@ref-type='corresp']")     #schema 2:"4405051"
    if (!is.null(correspondingAuthorsNode1) && length(correspondingAuthorsNode1) > 0) {correspondingAuthorsParentNodes <- lapply(correspondingAuthorsNode1, function(x)x)
    } else if (!is.null(correspondingAuthorsNode2) && length(correspondingAuthorsNode2) > 0) {correspondingAuthorsParentNodes <- lapply(correspondingAuthorsNode2, XML::xmlParent)
    }    else     return(list(name = NA, affIds = NA))

    correspondingAuthorsList <- lapply(correspondingAuthorsParentNodes, function(correspondingAuthorsParentNode){
      name <- paste(stats::na.omit( XML::xmlValue(correspondingAuthorsParentNode[["name"]][["given-names"]])), stats::na.omit( XML::xmlValue(correspondingAuthorsParentNode[["name"]][["surname"]])))

      affNodes <- XML::xmlElementsByTagName(correspondingAuthorsParentNode, "xref", recursive = F)
      if(length(affNodes) == 0)  return(c(name = name, affIds = NA))

      index <- which(sapply(affNodes, function(x)  XML::xmlAttrs(x)["ref-type"] == "aff") == T)
      if(length(index) == 0)  return(c(name = name, affIds = NA))

      affNodes <- affNodes[index]
      affIds <- lapply(affNodes,  function(y) XML::xmlAttrs(y)["rid"])

      if(is.na(name) || is.null(name) || length(name) ==0 || name =="")name <- NA
      if(is.na(affIds) || is.null(affIds) || length(affIds) ==0 || affIds =="")affIds <- NA

      return(c(name = name, affIds = affIds))
    })

    correspondingAuthors <- paste0(unique(sapply(correspondingAuthorsList, function(x) x["name"])), collapse = "||")
    if(length(correspondingAuthors) == 0 || is.null(correspondingAuthors) || is.na(correspondingAuthors) || correspondingAuthors == "") correspondingAuthors <- NA

    corespondingAuthorAffIds <- unique(unlist(sapply(correspondingAuthorsList, function(x) x[grep("affIds", names(x))])))
    if(length(corespondingAuthorAffIds) == 0 || is.null(corespondingAuthorAffIds) || is.na(corespondingAuthorAffIds) || corespondingAuthorAffIds == "") corespondingAuthorAffIds <- NA

    return(list(name = correspondingAuthors, affIds = corespondingAuthorAffIds))
  }
  retriveCorrespondindAuthorAffliation <- function(article,  correspondingAuthorAffIds){
    if(is.na(correspondingAuthorAffIds)) return(NA) else correspondingAuthorAffIds <- unlist(correspondingAuthorAffIds)
    correspondingAuthorAffs <-
      paste(stats::na.omit(sapply(correspondingAuthorAffIds, function(x) {
        node <-XML::xpathApply(article,  paste0("//aff[@id='", x, "']"))
        if(length(node) > 0 ) node <- node[[1]] else return(NA)
        childValues <- sapply(XML::xmlChildren(node), XML::xmlValue)
        index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues))
        if(length(index) > 0)  return(paste(childValues[index], collapse = ", ")) else return(NA)
      })),
      collapse = "||")

    if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "")correspondingAuthorAffs <- NA
    return(correspondingAuthorAffs)
  }
  retriveCorrespondindAuthorAffliationSchema3 <- function(article,  correspondingAuthorAffIds){
    if(is.na(correspondingAuthorAffIds)) return(NA) else correspondingAuthorAffIds <- unlist(correspondingAuthorAffIds)
    nodes <-XML::xpathApply(article,  paste0("//aff"))
    correspondingAuthorAffs <-
      paste(stats::na.omit(sapply(nodes, function(node) {
        childrenNodes <- XML::xmlChildren(node)

        index <- which(sapply(childrenNodes, function(x) {
          XML::xmlGetAttr(x, "id") %in% correspondingAuthorAffIds
        }) == T)

        childValues <- sapply(childrenNodes[index+1], XML::xmlValue)
        index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues))
        if(length(index) > 0)  return(paste(childValues[index], collapse = "||")) else return(NA)
      })),
      collapse = "||")

    if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "")correspondingAuthorAffs <- NA
    return(correspondingAuthorAffs)
  }

  results <-
    do.call(rbind, XML::xpathApply(doc, "//article", function(x) {
      article <- XML::xmlDoc(x)

      pmid <- retrivePMID (article)
      journal <- retriveJournal(article)
      journalLocation <-retriveJournalLocation(article)
      publicationDate <-GetEpubDateFromPmcidEfetchDoc(article)
      authors <- retriveAuthor(article)
      emails <-retriveEmails(article)
      affiliations <- retriveAffliation(article)
# There are hierarchy of the existance of the properties
      if(is.na(authors)) temp <- NA else temp <- retriveCorrespondingAuthor(article)
        correspondingAuthors <- temp["name"]
        correspondingAuthorAffIds <- temp["affIds"]
if(is.na(affiliations) || is.na(correspondingAuthorAffIds)) correspondingAuthorAffs <- NA else  correspondingAuthorAffs <- retriveCorrespondindAuthorAffliation(article, correspondingAuthorAffIds)

# if the corresponding aff ids exists but correspondingAuthorAffs
      if (is.na(correspondingAuthorAffs) && !is.na(correspondingAuthorAffIds) && !is.na(affiliations)) correspondingAuthorAffs <- retriveCorrespondindAuthorAffliation(article,strsplit(as.character(correspondingAuthorAffIds), " "))

      if (is.na(correspondingAuthorAffs) && !is.na(correspondingAuthorAffIds) && !is.na(affiliations)) correspondingAuthorAffs <- retriveCorrespondindAuthorAffliationSchema3(article,correspondingAuthorAffIds)

# sometimes, there is no marking on corresponding author because all authors are corresponding author
      if (is.na(correspondingAuthors) && !is.na(authors)) correspondingAuthors <- authors
      if (is.na(correspondingAuthorAffIds) && !is.na(affiliations)) correspondingAuthorAffs <- affiliations

      return(cbind(
        pmid,
        journal,
        journalLocation,
        publicationDate,
        authors,
        emails,
        affiliations,
        correspondingAuthors,
        correspondingAuthorAffs)
      )
    }))

  rownames(results) <- NULL
  return(results)
}

#' GetMetaDataFromPmcid
#'
#' @param pmcid a string of character. PubMed Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param writeFileName a string of characters. The file name you would like to download to.
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples GetMetaDataFromPmcid(c("5575286", "4804230"))
#'
#' @import XML
#'
GetMetaDataFromPmcid <-
  function(pmcid,
           apiKey = "",
           email = "",
           writeFileName = "") {
    doc <-
      GetDoc(id=pmcid, db="pmc", endpoint = "efetch", apiKey=apiKey, email=email)
    if (writeFileName != "")
      XML::saveXML(doc, file = writeFileName)
    result <- ReadMetaDataFromPmcidEfetchDoc(doc)
    return(result)
  }

#' GetMetaDataFromPmcidBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param apiKey a string of characters. The API Key obtained through NCBI account
#' @param email a string of characters. Your email address
#' @param writeFileName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples GetMetaDataFromPmcidBatch(c("5575286", "4804230"))
#'
#' @import XML
#'
GetMetaDataFromPmcidBatch <-
  function(pmcids,
           apiKey = "",
           email = "",
           writeFileName = "") {
    nids <- length(pmcids)
    grid <- 500
    nloop <- ceiling(nids / grid)
    results <- as.data.frame(matrix(nrow = nids, ncol = 10, ""), stringsAsFactors = F)
    # colnames(results) <- c("pmcid", "pmid", "doi")
    for (iloop in 1:nloop) {
      iindex <-
        (((iloop - 1) * grid) + 1):ifelse(iloop * grid > nids, nids, iloop * grid)
      results[, 1] <- pmcids[iindex]
      temp <-
        GetMetaDataFromPmcid(
          pmcids[iindex],
          apiKey = apiKey,
          email = email,
          writeFileName = writeFileName
        )
      temp <- as.data.frame(temp, stringsAsFactors = F)
      results[,-1] <- temp
    }
    colnames(results) <- c("pmcid", names(temp))

    return(results)
  }

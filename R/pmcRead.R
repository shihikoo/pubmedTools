#' ReadPmidDoiFromPmcidEsummaryDoc
#'
#' @param doc a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return extract pmid and doi for all articles in the doc
#' @export
#'
#' @examples
#' doc <- GetDoc(id=c("5575286", "4804230"), db="pmc", endpoint="esummary")
#' ReadPmidDoiFromPmcidEsummaryDoc(doc)
#'
#' @import xml2
#'
ReadPmidDoiFromPmcidEsummaryDoc <- function(doc) {
  nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//DocSum"))

  results <- as.data.frame(t(sapply(nodesetList, function(x) {
    x <- xml2::as_xml_document(list(x))
    pmid <-
      RetriveXmlNodeValuefromDoc(x,  "Item[@Name='pmid']")
    doi <-
      RetriveXmlNodeValuefromDoc(x,  "Item[@Name='doi']")

    return(cbind(pmid, doi))
  })), stringsAsFactors = F, drop = F, colnames = c("pmid","doi"))
  # print(results)
  names(results) <- c("pmid","doi")
  return(results)
}

#' GetPmidDoiFromPmcid
#'
#' @param pmcid a string of character. PubMed central Id
#' @param writeFileName a string of characters.
#'
#' @return a string: pmid
#' @export
#'
#' @examples
#' GetPmidDoiFromPmcid(c("5575286", "4804230"))
#'
#' @import xml2
#'
GetPmidDoiFromPmcid <-
  function(pmcid,
           writeFileName = "") {
    doc <-
      GetDoc(id=pmcid,db= "pmc", endpoint="esummary")
    if (writeFileName != "")
      xml2::write_xml(doc, file = writeFileName)
    result <- ReadPmidDoiFromPmcidEsummaryDoc(doc)
    return(result)
  }

#' GetPmidDoiFromPmcidBatch
#'
#' @param pmcids a string of character. PubMed central Id
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples
#' GetPmidDoiFromPmcidBatch(c("5575286", "4804230"))
#'
#'
GetPmidDoiFromPmcidBatch <-
  function(pmcids) {
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
        GetPmidDoiFromPmcid(pmcids[iindex])
    }
    return(results)
  }

#' DownloadMetaDataWithPmcidsBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param fileBaseName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples DownloadMetaDataWithPmcidsBatch(c("5575286", "4804230"), fileBaseName="test.xml")
#'
#' @import xml2
#'
DownloadMetaDataWithPmcidsBatch <-
  function(pmcids,
           fileBaseName = "") {
    nids <- length(pmcids)
    grid <- 500
    nloop <- ceiling(nids / grid)
    for (iloop in 1:nloop) {
      iindex <- (((iloop - 1) * grid) + 1) : ifelse(iloop * grid > nids, nids, iloop * grid)
      doc <-
        GetDoc(id = pmcids[iindex], db="pmc", endpoint = "efetch")
      outputFile <-
        xml2::write_xml(doc, file = paste0(
          gsub("[.]xml", "", fileBaseName),
          min(iindex),
          "_",
          max(iindex),
          ".xml"
        ))
    }
    return(nloop)
  }

#' extractEpubDateFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string: publicationDate
#'
#' @examples
#' doc <- GetDoc(id="5304250",db= "pmc", endpoint="efetch")
#' extractEpubDateFromPmcidEfetchDoc(doc)
#' doc <- GetDoc(id="4415024",db= "pmc", endpoint="efetch")
#' extractEpubDateFromPmcidEfetchDoc(doc)
#' doc <- GetDoc(id="4804230",db= "pmc", endpoint="efetch")
#' extractEpubDateFromPmcidEfetchDoc(doc)
#'
#' @import xml2
#' @export
#'
extractEpubDateFromPmcidEfetchDoc <- function(article){
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


#' extractPmidFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' extractPmidFromPmcidEfetchDoc(doc)
#'
extractPmidFromPmcidEfetchDoc <- function(article){
  output <- unique(RetriveXmlNodeValuefromDoc(article,  "//article-id[@pub-id-type='pmid']"))
  if(length(output) >0 ) return(output[[1]]) else return(output)
}

#' extractJournalFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' extractJournalFromPmcidEfetchDoc(doc)
#'
extractJournalFromPmcidEfetchDoc <- function(article){
  output <- unique(RetriveXmlNodeValuefromDoc(article,  "//journal-title"))
  if(length(output > 0)) return(output[[1]]) else return(output) }


#' extractPublisherFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' extractPublisherFromPmcidEfetchDoc(doc)
#'
extractPublisherFromPmcidEfetchDoc <- function(article){
  output <- unique(RetriveXmlNodeValuefromDoc(article,  "//publisher"))
  if(length(output) >0 ) return(output[[1]]) else return(output)
  }

#' extractEmailsFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' extractEmailsFromPmcidEfetchDoc(doc)
#'
extractEmailsFromPmcidEfetchDoc <- function(article){
  emailList <- unique(stringr::str_extract_all(RetriveXmlNodeValuefromDoc(article,  "//email"), "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+[.][a-zA-Z]{2,}", simplify = T))
  validIndex <- which(!is.na(emailList) & emailList != "")
  if(length(validIndex) > 0)    emails <- paste0(emailList[validIndex], collapse = "; ") else emails <- NA
  return(emails)
}

#' extractAffliationFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#' @param affIds affiliation id to extract. Default is "all_affiliatins", which gives all the affiliations
#'
#' @return a string of characters.
#' @export
#'
#' @import data.table
#'
#' @examples
#' doc <- GetDoc(id=c("2823164"), db="pmc", endpoint="efetch")
#' extractAffliationFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3339580", db="pmc", endpoint="efetch")
#' extractAffliationFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3324826", db="pmc", endpoint="efetch")
#' extractAffliationFromPmcidEfetchDoc(doc)
#'
extractAffliationFromPmcidEfetchDoc <- function(article, affIds = "all_affiliations"){
  
  if(affIds[[1]] == "all_affiliations") {
    nodes <- xml2::xml_find_all(article,  "//aff") 
    } else{
      nodes <- lapply(affIds, function(x)
    xml2::xml_find_all(article,  paste0("//aff[@id='", x, "']"))
    # xml2::xml_find_all(article,  paste0("//aff[@id='", affIds, "']"))
      )
      }
  
  if(length(nodes) == 0 ) return(NA)

  affList <- lapply(nodes, function(node){
    # affid <- xml2::xml_attr(node, "id")
    # if(is.na(affid)) affid <- sapply(xml2::xml_children(node), xml2::xml_attr, "id")

    childValues <- sapply(xml2::xml_children(node), xml2::xml_text)
    index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues) & lapply(childValues,nchar) > 1)
    # if(length(index) > 0)  return(list(affiliation = paste(childValues[index], collapse = ", "), affid = affid))
    if(length(index) > 0)  return(paste(childValues[index], collapse = ", "))

    # affiliationCandidates <-
    affiliationCandidates <- sapply(stringi::stri_split(xml2::xml_text(node), regex = "[;] (and)?"), trimws)
    # index <- which(!is.na(affiliationCandidates) & affiliationCandidates != "" & !grepl("^[0-9]+$", affiliationCandidates) & lapply(affiliationCandidates, nchar) > 1)
    affiliationCandidates <- affiliationCandidates[!is.na(affiliationCandidates) & affiliationCandidates != "" & !grepl("^[0-9]+$", affiliationCandidates) & lapply(affiliationCandidates, nchar) > 1]

    # affiliationCandidates <- gsub("^[0-9]","", affiliationCandidates[index])
    index <- !grepl("^[a-zA-Z]", affiliationCandidates)
    if(length(index) > 0) affiliationCandidates[index] <- sapply(affiliationCandidates[index], function(x) substr(x,start=2,nchar(x)))

    # index <- which(!is.na(affiliation) & affiliation != "" & lapply(affiliation,nchar) > 1)
    # if(length(index) > 0)  return(list(affiliation = affiliation, affid = affid))
    # return(list(aff = NA, affid=NA))
    # if(length(index) > 0)  return(list(affiliation = affiliation, affid = affid))
return(affiliationCandidates[!is.na(affiliationCandidates) & affiliationCandidates != "" & lapply(affiliationCandidates,nchar) > 1])
    # return(NA)
  })

  affiliations <- paste(unlist(affList), collapse = "; ")
  # options(warn = -1)
  return(affiliations)
}

#' extractAuthorsFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' extractAuthorsFromPmcidEfetchDoc(doc)
#'
extractAuthorsFromPmcidEfetchDoc <- function(article){
  authorsNode <- xml2::xml_find_all(article, "//contrib[@contrib-type='author']//name")
  if (is.null(authorsNode)| length(authorsNode) == 0)  return(NA)

  authorsNodeList <- xml2::as_list(authorsNode)
  authors <- sapply(authorsNodeList, function(author){
    forename <- author[["given-names"]]
    lastname <- author[["surname"]]
    return(paste(stats::na.omit(forename), stats::na.omit(lastname)))
  } )

  authors <- paste0(stats::na.omit( unique(authors)), collapse = "; ")
  if(authors == "") return(NA)
  return(authors)
}

#' extractCorrespondingAuthorFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4415024", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3339580", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorFromPmcidEfetchDoc(doc)

extractCorrespondingAuthorFromPmcidEfetchDoc <- function(article){
  extractCorrespondingAuthorNodesFromPmcidEfetchDoc <- function(article){
    correspondingAuthorsNode_schema1 <- xml2::xml_find_all(article, "//contrib[@corresp='yes']")    #schema 1:"3324826"
    if (!is.null(correspondingAuthorsNode_schema1) && length(correspondingAuthorsNode_schema1) > 0) return(lapply(correspondingAuthorsNode_schema1, function(x)x))

    correspondingAuthorsNode_schema2 <- xml2::xml_find_all(article, "//xref[@ref-type='corresp']")     #schema 2:"4405051"
    if (!is.null(correspondingAuthorsNode_schema2) && length(correspondingAuthorsNode_schema2) > 0) return(lapply(correspondingAuthorsNode_schema2, function(x) xml2::xml_parent(x)))
    return(list(name = NA, affIds = NA))
  }

    correspondingAuthorsParentNodes <- extractCorrespondingAuthorNodesFromPmcidEfetchDoc(article)

    correspondingAuthorsList <- sapply(correspondingAuthorsParentNodes, function(correspondingAuthorsParentNode){
      correspondingAuthorsParentNode <- xml2::as_xml_document(correspondingAuthorsParentNode)
      name <- paste(stats::na.omit(xml2::xml_text(xml2::xml_find_all(correspondingAuthorsParentNode, "//name//given-names"))), stats::na.omit( xml2::xml_text(xml2::xml_find_all(correspondingAuthorsParentNode, "//name//surname"))))

      if(is.na(name) || is.null(name) || length(name) == 0 || name =="") name <- NA
      return(name)
    })

    correspondingAuthors <- paste0(unlist(correspondingAuthorsList), collapse = "; ")

    # First we extract corresponding authors. if Corresponding author can't be found, we put all authors to be corresponding author
    if(correspondingAuthors == "" | is.na(correspondingAuthors)) correspondingAuthors <- extractAuthorsFromPmcidEfetchDoc(article)

    return(correspondingAuthors)
}

#' extractCorrespondingAuthorIdFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="3339580", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3892617", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4415024", db="pmc", endpoint="efetch")
#' extractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
extractCorrespondingAuthorIdFromPmcidEfetchDoc <- function(article){
  extractCorrespondingAuthorNodesFromPmcidEfetchDoc <- function(article){
    correspondingAuthorsNode_schema1 <- xml2::xml_find_all(article, "//contrib[@corresp='yes']")    #schema 1:"3324826"
    if (!is.null(correspondingAuthorsNode_schema1) && length(correspondingAuthorsNode_schema1) > 0) return(lapply(correspondingAuthorsNode_schema1, function(x)x))

    correspondingAuthorsNode_schema2 <- xml2::xml_find_all(article, "//xref[@ref-type='corresp']")     #schema 2:"4405051"
    if (!is.null(correspondingAuthorsNode_schema2) && length(correspondingAuthorsNode_schema2) > 0) return(lapply(correspondingAuthorsNode_schema2, function(x) xml2::xml_parent(x)))
    return(list(name = NA, affIds = NA))
  }

  correspondingAuthorsParentNodes <- extractCorrespondingAuthorNodesFromPmcidEfetchDoc(article)

  correspondingAuthorIdsList <- sapply(correspondingAuthorsParentNodes, function(correspondingAuthorsParentNode){
    correspondingAuthorsParentNode <- xml2::as_xml_document(correspondingAuthorsParentNode)

    affNodes <- xml2::xml_find_all(correspondingAuthorsParentNode, "xref")
    if(length(affNodes) == 0)  return(NA)

    index <- which(sapply(affNodes, function(x)  xml2::xml_attr(x,"ref-type") == "aff") == T)
    if(length(index) == 0)  return(NA) else affNodes <- affNodes[index]

    affIds <- unique(stats::na.omit(sapply(affNodes,  function(y) xml2::xml_attr(y, "rid"))))
    if(length(affIds) ==0  || all(affIds =="")) return(NA)

    affIds <- strsplit(affIds, " ")

    return(affIds)
  })

  corespondingAuthorAffIds <- unique(stats::na.omit(c(unlist(correspondingAuthorIdsList))))

  if(any(is.null(corespondingAuthorAffIds), length(corespondingAuthorAffIds) == 0,corespondingAuthorAffIds == "")) corespondingAuthorAffIds <- NA
  return(corespondingAuthorAffIds)
}

#' extractCorrespondindAuthorAffliationFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractCorrespondindAuthorAffliationFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4415024", db="pmc", endpoint="efetch")
#' extractCorrespondindAuthorAffliationFromPmcidEfetchDoc(doc)
#'
#'
extractCorrespondindAuthorAffliationFromPmcidEfetchDoc <- function(article){
  correspondingAuthorAffIds <- extractCorrespondingAuthorIdFromPmcidEfetchDoc(article)
  if(is.na(correspondingAuthorAffIds[[1]]))  return(extractAffliationFromPmcidEfetchDoc(article))

  extractCorrespondindAuthorAffliationSchema3 <- function(article,  correspondingAuthorAffIds){
    if(is.na(correspondingAuthorAffIds)) return(NA) else correspondingAuthorAffIds <- unlist(correspondingAuthorAffIds)
    nodes <-xml2::xml_find_all(article,  paste0("//aff"))
    correspondingAuthorAffs <-
      paste(stats::na.omit(sapply(nodes, function(node) {
        childrenNodes <- xml2::xml_children(node)

        index <- which(sapply(childrenNodes, function(x) {
          xml2::xml_attr(x, "id") %in% correspondingAuthorAffIds
        }) == T)

        childValues <- sapply(childrenNodes[index+1], xml2::xml_text)
        index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues) & !length(childValues) ==1 )
        if(length(index) > 0)  return(paste(childValues[index], collapse = "; ")) else return(NA)
      })),
      collapse = "; ")

    if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "")correspondingAuthorAffs <- NA
    return(correspondingAuthorAffs)
  }

  correspondingAuthorAffs <- extractAffliationFromPmcidEfetchDoc(article, affIds = correspondingAuthorAffIds)
    # paste(unique(stats::na.omit(sapply(correspondingAuthorAffIds, function(x) {
    #   node <-xml2::xml_find_all(article,  paste0("//aff[@id='", x, "']"))
    #   if(length(node) > 0 ) node <- node[[1]] else return(NA)
    #   childValues <- sapply(xml2::xml_children(node), xml2::xml_text)
    #   index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues) & !nchar(childValues) ==1 )
    #   if(length(index) > 0)  return(paste(childValues[index], collapse = ", ")) else return(NA)
    # })),
    # collapse = "; "))

  if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "") correspondingAuthorAffs <- extractCorrespondindAuthorAffliationSchema3(article,  correspondingAuthorAffIds)

  if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "") correspondingAuthorAffs <- extractAffliationFromPmcidEfetchDoc(article)

  return(correspondingAuthorAffs)
}

#' extractTitleFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. title
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractTitleFromPmcidEfetchDoc(doc)
#'
extractTitleFromPmcidEfetchDoc <- function(article){
  return(RetriveXmlNodeValuefromDoc(article, "//title-group//article-title")[[1]])
}

#' extractAbstractFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. abstract
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractAbstractFromPmcidEfetchDoc(doc)
#'
extractAbstractFromPmcidEfetchDoc <- function(article){
  return(paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article, "//abstract"))), collapse = " "))
}

#' extractFullTextFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. fulltext
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractFullTextFromPmcidEfetchDoc(doc)
#'
extractFullTextFromPmcidEfetchDoc <- function(article){
  return(paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article, "//body"))), collapse = " "))
}

#' extractStudyTypeFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. study type
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' extractStudyTypeFromPmcidEfetchDoc(doc)
#'
extractStudyTypeFromPmcidEfetchDoc <- function(article){
  return(paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article ,"//article-categories//subj-group//subject"))), collapse = "; "))
}

#' ReadMetaDataFromPMCnxml
#'
#' @param link a XMLInternalDocument, a XMLAbstractDocument
#' @param columns list of output columns
#'
#' @return a 1x3 data frame. With three columns: pmcid, pmid, doi
#' @export
#' @import xml2
#' 
ReadMetaDataFromPMCnxml <- function(link, columns = c("pmid", "title", "abstract","fulltext","studytype", "journal", "publisher", "publicationDate","authors", "emails","affiliations", "correspondingAuthors","correspondingAuthorAffs")) {
  doc <- xml2::read_xml(link)
  nodeset <- (xml2::as_list(xml2::xml_find_all(doc, "//article")))[[1]]
  nodeset <- nodeset[which(names(nodeset) != "")]
  article <- xml2::as_xml_document(list(nodeset))
    
    pmid <- extractPmidFromPmcidEfetchDoc(article)
    title <- extractTitleFromPmcidEfetchDoc(article)
    abstract <- extractAbstractFromPmcidEfetchDoc(article)
    fulltext <- extractFullTextFromPmcidEfetchDoc(article)
    studytype <- extractStudyTypeFromPmcidEfetchDoc(article)
    journal <- extractJournalFromPmcidEfetchDoc(article)
    publisher <-extractPublisherFromPmcidEfetchDoc(article)
    publicationDate <- extractEpubDateFromPmcidEfetchDoc(article)
    authors <- extractAuthorsFromPmcidEfetchDoc(article)
    emails <- extractEmailsFromPmcidEfetchDoc(article)
    affiliations <- extractAffliationFromPmcidEfetchDoc(article)
    correspondingAuthors <- extractCorrespondingAuthorFromPmcidEfetchDoc(article)
    correspondingAuthorAffs <- extractCorrespondindAuthorAffliationFromPmcidEfetchDoc(article)
    
    results <- cbind(
      pmid,
      title,
      abstract,
      fulltext,
      studytype,
      journal,
      publisher,
      publicationDate,
      authors,
      emails,
      affiliations,
      correspondingAuthors,
      correspondingAuthorAffs)
  
  colnames(results) <- c("pmid", "title", "abstract","fulltext","studytype", "journal", "publisher", "publicationDate","authors", "emails","affiliations", "correspondingAuthors","correspondingAuthorAffs")
  rownames(results) <- NULL
  return(results[,columns])
}

#' ReadMetaDataFromPmcidEfetchDoc
#'
#' @param doc a XMLInternalDocument, a XMLAbstractDocument
#' @param columns list of output columns
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples
#'
#' doc <- GetDoc(id=c("2823164"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#' 
#' doc <- GetDoc(id=c("2823164", "3324826", "3339580"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id=c("4812069","3892617"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id=c("4405051","4415024","4804230"), db="pmc", endpoint="efetch")
#' ReadMetaDataFromPmcidEfetchDoc(doc, c("pmid","title"))
#'
#' @import xml2 
#'
ReadMetaDataFromPmcidEfetchDoc <- function(doc, columns = c("pmid", "title", "abstract","fulltext","studytype", "journal", "publisher", "publicationDate","authors", "emails","affiliations", "correspondingAuthors","correspondingAuthorAffs")) {
  nodesetList <- xml2::as_list(xml2::xml_find_all(doc, "//article"))

  results <- as.data.frame(t(sapply(nodesetList, function(x) {
    article <- xml2::as_xml_document(list(x[c("front" ,"body",  "back")]))

    pmid <- extractPmidFromPmcidEfetchDoc(article)
    title <- extractTitleFromPmcidEfetchDoc(article)
    abstract <- extractAbstractFromPmcidEfetchDoc(article)
    fulltext <- extractFullTextFromPmcidEfetchDoc(article)
    studytype <- extractStudyTypeFromPmcidEfetchDoc(article)
    journal <- extractJournalFromPmcidEfetchDoc(article)
    publisher <-extractPublisherFromPmcidEfetchDoc(article)
    publicationDate <- extractEpubDateFromPmcidEfetchDoc(article)
    authors <- extractAuthorsFromPmcidEfetchDoc(article)
    emails <- extractEmailsFromPmcidEfetchDoc(article)
    affiliations <- extractAffliationFromPmcidEfetchDoc(article)
    correspondingAuthors <- extractCorrespondingAuthorFromPmcidEfetchDoc(article)
    correspondingAuthorAffs <- extractCorrespondindAuthorAffliationFromPmcidEfetchDoc(article)

    return(cbind(
      pmid,
      title,
      abstract,
      fulltext,
      studytype,
      journal,
      publisher,
      publicationDate,
      authors,
      emails,
      affiliations,
      correspondingAuthors,
      correspondingAuthorAffs)
    )
  })))
  colnames(results) <- c("pmid", "title", "abstract","fulltext","studytype", "journal", "publisher", "publicationDate","authors", "emails","affiliations", "correspondingAuthors","correspondingAuthorAffs")
  rownames(results) <- NULL
  return(results[,columns])
}

#' GetMetaDataFromPmcid
#'
#' @description The number of pmcids sent should be less than 500.
#' @param pmcid a string of character. PubMed Id
#' @param writeFileName a string of characters. The file name you would like to download to.
#'
#' @return a list of metaDatarmation retrived from PubMed
#' @export
#'
#' @examples mydata <- GetMetaDataFromPmcid(c("5575286", "4804230"))
#'
#' @import xml2
#'
GetMetaDataFromPmcid <-
  function(pmcid,
           writeFileName = "") {
    doc <-
      GetDoc(id=pmcid, db="pmc", endpoint = "efetch")
    if (writeFileName != "")
      xml2::write_xml(doc, file = writeFileName)
    result <- ReadMetaDataFromPmcidEfetchDoc(doc)
    return(result)
  }

#' GetMetaDataFromPmcidBatch
#'
#' @param pmcids a string of character. PubMed central Id
#' @param writeFileName a string of character. The base name of the to be saved xml files
#'
#' @return a nx3 data frame. With three columns: pmcid, pmid, doi
#' @export
#'
#' @examples 
#' GetMetaDataFromPmcidBatch(c("5575286", "4804230"))
#'
#' @import xml2
#'
GetMetaDataFromPmcidBatch <-
  function(pmcids,
           writeFileName = "") {
    nids <- length(pmcids)
    grid <- 500
    nloop <- ceiling(nids / grid)
    results <- as.data.frame(matrix(nrow = nids, ncol = 14, ""), stringsAsFactors = F)
    for (iloop in 1:nloop) {
      iindex <-
        (((iloop - 1) * grid) + 1):ifelse(iloop * grid > nids, nids, iloop * grid)
      results[, 1] <- pmcids[iindex]
      temp <-
        GetMetaDataFromPmcid(
          pmcids[iindex],
          writeFileName = writeFileName
        )
      temp <- as.data.frame(temp, stringsAsFactors = F)
      results[,-1] <- temp
    }
    colnames(results) <- c("pmcid", names(temp))

    return(results)
  }

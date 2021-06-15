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

#' ExtractEpubDateFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string: publicationDate
#'
#' @examples
#' doc <- GetDoc(id="5304250",db= "pmc", endpoint="efetch")
#' ExtractEpubDateFromPmcidEfetchDoc(doc)
#' doc <- GetDoc(id="4415024",db= "pmc", endpoint="efetch")
#' ExtractEpubDateFromPmcidEfetchDoc(doc)
#' doc <- GetDoc(id="4804230",db= "pmc", endpoint="efetch")
#' ExtractEpubDateFromPmcidEfetchDoc(doc)
#'
#' @import xml2
#' @export
#'
ExtractEpubDateFromPmcidEfetchDoc <- function(article){
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


#' ExtractPmidFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' ExtractPmidFromPmcidEfetchDoc(doc)
#'
ExtractPmidFromPmcidEfetchDoc <- function(article){
  output <- unique(RetriveXmlNodeValuefromDoc(article,  "//article-id[@pub-id-type='pmid']"))
  if(length(output) >0 ) return(output[[1]]) else return(output)
}

#' ExtractJournalFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' ExtractJournalFromPmcidEfetchDoc(doc)
#'
ExtractJournalFromPmcidEfetchDoc <- function(article){
  output <- unique(RetriveXmlNodeValuefromDoc(article,  "//journal-title"))
  if(length(output > 0)) return(output[[1]]) else return(output) }


#' ExtractPublisherFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' ExtractPublisherFromPmcidEfetchDoc(doc)
#'
ExtractPublisherFromPmcidEfetchDoc <- function(article){
  output <- unique(RetriveXmlNodeValuefromDoc(article,  "//publisher"))
  if(length(output) >0 ) return(output[[1]]) else return(output)
  }

#' ExtractEmailsFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' ExtractEmailsFromPmcidEfetchDoc(doc)
#'
ExtractEmailsFromPmcidEfetchDoc <- function(article){
  emailList <- unique(stringr::str_extract_all(RetriveXmlNodeValuefromDoc(article,  "//email"), "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+[.][a-zA-Z]{2,}", simplify = T))
  validIndex <- which(!is.na(emailList) & emailList != "")
  if(length(validIndex) > 0)    emails <- paste0(emailList[validIndex], collapse = "; ") else emails <- NA
  return(emails)
}

#' ExtractAffliationFromPmcidEfetchDoc
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
#' ExtractAffliationFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3339580", db="pmc", endpoint="efetch")
#' ExtractAffliationFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3324826", db="pmc", endpoint="efetch")
#' ExtractAffliationFromPmcidEfetchDoc(doc)
#'
ExtractAffliationFromPmcidEfetchDoc <- function(article, affIds = "all_affiliations"){
  if(all(is.na(affIds))) return(NA)
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

#' ExtractAuthorsFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="2823164", db="pmc", endpoint="efetch")
#' ExtractAuthorsFromPmcidEfetchDoc(doc)
#'
ExtractAuthorsFromPmcidEfetchDoc <- function(article){
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

#' ExtractCorrespondingAuthorFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4415024", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3339580", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorFromPmcidEfetchDoc(doc)

ExtractCorrespondingAuthorFromPmcidEfetchDoc <- function(article){
  ExtractCorrespondingAuthorNodesFromPmcidEfetchDoc <- function(article){
    correspondingAuthorsNode_schema1 <- xml2::xml_find_all(article, "//contrib[@corresp='yes']")    #schema 1:"3324826"
    if (!is.null(correspondingAuthorsNode_schema1) && length(correspondingAuthorsNode_schema1) > 0) return(lapply(correspondingAuthorsNode_schema1, function(x)x))

    correspondingAuthorsNode_schema2 <- xml2::xml_find_all(article, "//xref[@ref-type='corresp']")     #schema 2:"4405051"
    if (!is.null(correspondingAuthorsNode_schema2) && length(correspondingAuthorsNode_schema2) > 0) return(lapply(correspondingAuthorsNode_schema2, function(x) xml2::xml_parent(x)))
    return(NA)
  }

    correspondingAuthorsParentNodes <- ExtractCorrespondingAuthorNodesFromPmcidEfetchDoc(article)

    if(is.na(correspondingAuthorsParentNodes)) return(ExtractAuthorsFromPmcidEfetchDoc(article))
    
    correspondingAuthorsList <- sapply(correspondingAuthorsParentNodes, function(correspondingAuthorsParentNode){
      correspondingAuthorsParentNode <- xml2::as_xml_document(correspondingAuthorsParentNode)
      name <- paste(stats::na.omit(xml2::xml_text(xml2::xml_find_all(correspondingAuthorsParentNode, "//name//given-names"))), stats::na.omit( xml2::xml_text(xml2::xml_find_all(correspondingAuthorsParentNode, "//name//surname"))))

      if(is.na(name) || is.null(name) || length(name) == 0 || name =="") name <- NA
      return(name)
    })

    correspondingAuthors <- paste0(unlist(correspondingAuthorsList), collapse = "; ")

    # First we extract corresponding authors. if Corresponding author can't be found, we put all authors to be corresponding author
    if(correspondingAuthors == "" | is.na(correspondingAuthors)) correspondingAuthors <- ExtractAuthorsFromPmcidEfetchDoc(article)

    return(correspondingAuthors)
}

#' ExtractCorrespondingAuthorIdFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="3339580", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="3892617", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4415024", db="pmc", endpoint="efetch")
#' ExtractCorrespondingAuthorIdFromPmcidEfetchDoc(doc)
#'
ExtractCorrespondingAuthorIdFromPmcidEfetchDoc <- function(article){
  ExtractCorrespondingAuthorNodesFromPmcidEfetchDoc <- function(article){
    correspondingAuthorsNode_schema1 <- xml2::xml_find_all(article, "//contrib[@corresp='yes']")    #schema 1:"3324826"
    if (!is.null(correspondingAuthorsNode_schema1) && length(correspondingAuthorsNode_schema1) > 0) return(lapply(correspondingAuthorsNode_schema1, function(x)x))

    correspondingAuthorsNode_schema2 <- xml2::xml_find_all(article, "//xref[@ref-type='corresp']")     #schema 2:"4405051"
    if (!is.null(correspondingAuthorsNode_schema2) && length(correspondingAuthorsNode_schema2) > 0) return(lapply(correspondingAuthorsNode_schema2, function(x) xml2::xml_parent(x)))
    return(NA)
  }

  correspondingAuthorsParentNodes <- ExtractCorrespondingAuthorNodesFromPmcidEfetchDoc(article)
  if(is.na(correspondingAuthorsParentNodes[[1]])) return(NA)
  
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

#' ExtractCorrespondindAuthorAffliationFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters.
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractCorrespondindAuthorAffliationFromPmcidEfetchDoc(doc)
#'
#' doc <- GetDoc(id="4415024", db="pmc", endpoint="efetch")
#' ExtractCorrespondindAuthorAffliationFromPmcidEfetchDoc(doc)
#'
ExtractCorrespondindAuthorAffliationFromPmcidEfetchDoc <- function(article){
  
  correspondingAuthorAffIds <- ExtractCorrespondingAuthorIdFromPmcidEfetchDoc(article)
  if(all(is.na(correspondingAuthorAffIds)))  return(ExtractAffliationFromPmcidEfetchDoc(article))

  ExtractCorrespondindAuthorAffliationSchema2 <- function(article,  correspondingAuthorAffIds){
    if(all(is.na(correspondingAuthorAffIds))) return(NA) else correspondingAuthorAffIds <- unlist(correspondingAuthorAffIds)
    nodes <-xml2::xml_find_all(article,  paste0("//aff"))
    correspondingAuthorAffs <-
      paste(stats::na.omit(sapply(nodes, function(node) {
        childrenNodes <- xml2::xml_children(node)

        index <- which(sapply(childrenNodes, function(x) {
          xml2::xml_attr(x, "id") %in% correspondingAuthorAffIds
        }) == T)
  
        if(max(index) == length(childrenNodes)) return(NA)
        
        childValues <- sapply(childrenNodes[index+1], xml2::xml_text)
        index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues) & !length(childValues) ==1 )
        if(length(index) > 0)  return(paste(childValues[index], collapse = "; ")) else return(NA)
      })),
      collapse = "; ")

    if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "")correspondingAuthorAffs <- NA
    return(correspondingAuthorAffs)
  }

  # for PMC6813468
  # ExtractCorrespondindAuthorAffliationSchema3 <- function(article,  correspondingAuthorAffIds){
  #   if(all(is.na(correspondingAuthorAffIds))) return(NA) else correspondingAuthorAffIds <- unlist(correspondingAuthorAffIds)
  #   nodes <-xml2::xml_find_all(article,  paste0("//aff"))
  #   correspondingAuthorAffs <-
  #     paste(stats::na.omit(sapply(nodes, function(node) {
  #       childrenNodes <- xml2::xml_children(node)
  #       
  #       index <- which(sapply(childrenNodes, function(x) {
  #         xml2::xml_attr(x, "id") %in% correspondingAuthorAffIds
  #       }) == T)
  #       
  #       childValues <- sapply(childrenNodes[index+1], xml2::xml_text)
  #       index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues) & !length(childValues) ==1 )
  #       if(length(index) > 0)  return(paste(childValues[index], collapse = "; ")) else return(NA)
  #     })),
  #     collapse = "; ")
  #   
  #   if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "")correspondingAuthorAffs <- NA
  #   return(correspondingAuthorAffs)
  # }
  
  correspondingAuthorAffs <- ExtractAffliationFromPmcidEfetchDoc(article, affIds = correspondingAuthorAffIds)
    # paste(unique(stats::na.omit(sapply(correspondingAuthorAffIds, function(x) {
    #   node <-xml2::xml_find_all(article,  paste0("//aff[@id='", x, "']"))
    #   if(length(node) > 0 ) node <- node[[1]] else return(NA)
    #   childValues <- sapply(xml2::xml_children(node), xml2::xml_text)
    #   index <- which(!is.na(childValues) & childValues != "" & !grepl("^[0-9]+$", childValues) & !nchar(childValues) ==1 )
    #   if(length(index) > 0)  return(paste(childValues[index], collapse = ", ")) else return(NA)
    # })),
    # collapse = "; "))

  if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "") correspondingAuthorAffs <- ExtractCorrespondindAuthorAffliationSchema2(article,  correspondingAuthorAffIds)
  
  # if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "") correspondingAuthorAffs <- ExtractCorrespondindAuthorAffliationSchema3(article,  correspondingAuthorAffIds)
  tryCatch(
    if(is.null(correspondingAuthorAffs) || is.na(correspondingAuthorAffs) || length(correspondingAuthorAffs) == 0 || correspondingAuthorAffs == "") correspondingAuthorAffs <- ExtractAffliationFromPmcidEfetchDoc(article)
  )


  return(correspondingAuthorAffs)
}

#' ExtractTitleFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. title
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractTitleFromPmcidEfetchDoc(doc)
#'
ExtractTitleFromPmcidEfetchDoc <- function(article){
  return(RetriveXmlNodeValuefromDoc(article, "//title-group//article-title")[[1]])
}

#' ExtractAbstractFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. abstract
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractAbstractFromPmcidEfetchDoc(doc)
#'
ExtractAbstractFromPmcidEfetchDoc <- function(article){
  return(paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article, "//abstract"))), collapse = " "))
}

#' ExtractFullTextFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. fulltext
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractFullTextFromPmcidEfetchDoc(doc)
#'
ExtractFullTextFromPmcidEfetchDoc <- function(article){
  nodes <- xml2::xml_find_all(article, "//ref-list")
  xml2::xml_remove(nodes)
  
  body <- paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article, "//body", onlyChildren = T))), collapse = " ")
  back <- paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article, "//back", onlyChildren = T))), collapse = " ")
  
  return(paste(body, back, sep=" "))
}

#' ExtractFullTextToFile
#'
#' @param xmlFilePathFull xml file full path 
#' @param overwirte whether or not overwrite the text file if already exists
#'
#' @return textfilename, a string
#' @export
#'
ExtractFullTextToFile <- function(xmlFilePathFull, overwirte = F){
  textfilename <- gsub(".nxml", ".txt", xmlFilePathFull, fixed = T)
  if(file.exists(textfilename) & overwirte == F)  return(textfilename)
  
  article <- xml2::read_xml(xmlFilePathFull)
  fullText <- ExtractFullTextFromPmcidEfetchDoc(article)
  if(is.na(fullText)) return(NA)

  stringi::stri_write_lines(fullText, textfilename)
  return(textfilename)
}

#' ExtractStudyTypeFromPmcidEfetchDoc
#'
#' @param article a XMLInternalDocument, a XMLAbstractDocument
#'
#' @return a string of characters. study type
#' @export
#'
#' @examples
#' doc <- GetDoc(id="4405051", db="pmc", endpoint="efetch")
#' ExtractStudyTypeFromPmcidEfetchDoc(doc)
#'
ExtractStudyTypeFromPmcidEfetchDoc <- function(article){
  return(paste0(stats::na.omit(unique(RetriveXmlNodeValuefromDoc(article ,"//article-categories//subj-group//subject"))), collapse = "; "))
}

#' ReadMetaDataFromPMCnxml
#'
#' @param xmlFilePathFull a xml file path
#' @param columns list of output columns
#'
#' @return a 1x3 data frame. With three columns: pmcid, pmid, doi
#' @export
#' @import xml2
#' 
ReadMetaDataFromPMCnxml <- function(xmlFilePathFull, columns = c("pmid", "title", "abstract","fulltext","studytype", "journal", "publisher", "publicationDate","authors", "emails","affiliations", "correspondingAuthors","correspondingAuthorAffs")) {
  doc <- xml2::read_xml(xmlFilePathFull)
  nodeset <- (xml2::as_list(xml2::xml_find_all(doc, "//article")))[[1]]
  nodeset <- nodeset[which(names(nodeset) != "")]
  article <- xml2::as_xml_document(list(nodeset))
    
  if("pmid" %in% columns) pmid <- ExtractPmidFromPmcidEfetchDoc(article) else pmid <- NA
  if("title" %in% columns) title <- ExtractTitleFromPmcidEfetchDoc(article) else title <- NA
  if("abstract" %in% columns) abstract <- ExtractAbstractFromPmcidEfetchDoc(article) else abstract <- NA
  if("fulltext" %in% columns) fulltext <- ExtractFullTextFromPmcidEfetchDoc(article) else fulltext <- NA
  if("studytype" %in% columns) studytype <- ExtractStudyTypeFromPmcidEfetchDoc(article) else studytype <- NA
  if("journal" %in% columns)  journal <- ExtractJournalFromPmcidEfetchDoc(article) else journal <- NA
  if("publisher" %in% columns)  publisher <-ExtractPublisherFromPmcidEfetchDoc(article) else publisher <- NA
  if("publicationDate" %in% columns)  publicationDate <- ExtractEpubDateFromPmcidEfetchDoc(article) else publicationDate <- NA
  if("authors" %in% columns)  authors <- ExtractAuthorsFromPmcidEfetchDoc(article) else authors <- NA
  if("emails" %in% columns)   emails <- ExtractEmailsFromPmcidEfetchDoc(article) else emails <- NA
  if("affiliations" %in% columns)   affiliations <- ExtractAffliationFromPmcidEfetchDoc(article) else affiliations <- NA
  if("correspondingAuthors" %in% columns)   correspondingAuthors <- ExtractCorrespondingAuthorFromPmcidEfetchDoc(article) else correspondingAuthors <- NA
  if("correspondingAuthorAffs" %in% columns)   correspondingAuthorAffs <- ExtractCorrespondindAuthorAffliationFromPmcidEfetchDoc(article) else correspondingAuthorAffs <- NA
    
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
    x <- x[which(names(x) != "")]
    
    article <- xml2::as_xml_document(list(x))

    if("pmid" %in% columns) pmid <- ExtractPmidFromPmcidEfetchDoc(article) else pmid <- NA
    if("title" %in% columns) title <- ExtractTitleFromPmcidEfetchDoc(article) else title <- NA
    if("abstract" %in% columns) abstract <- ExtractAbstractFromPmcidEfetchDoc(article) else abstract <- NA
    if("fulltext" %in% columns) fulltext <- ExtractFullTextFromPmcidEfetchDoc(article) else fulltext <- NA
    if("studytype" %in% columns) studytype <- ExtractStudyTypeFromPmcidEfetchDoc(article) else studytype <- NA
    if("journal" %in% columns)  journal <- ExtractJournalFromPmcidEfetchDoc(article) else journal <- NA
    if("publisher" %in% columns)  publisher <-ExtractPublisherFromPmcidEfetchDoc(article) else publisher <- NA
    if("publicationDate" %in% columns)  publicationDate <- ExtractEpubDateFromPmcidEfetchDoc(article) else publicationDate <- NA
    if("authors" %in% columns)  authors <- ExtractAuthorsFromPmcidEfetchDoc(article) else authors <- NA
    if("emails" %in% columns)   emails <- ExtractEmailsFromPmcidEfetchDoc(article) else emails <- NA
    if("affiliations" %in% columns)   affiliations <- ExtractAffliationFromPmcidEfetchDoc(article) else affiliations <- NA
    if("correspondingAuthors" %in% columns)   correspondingAuthors <- ExtractCorrespondingAuthorFromPmcidEfetchDoc(article) else correspondingAuthors <- NA
    if("correspondingAuthorAffs" %in% columns)   correspondingAuthorAffs <- ExtractCorrespondindAuthorAffliationFromPmcidEfetchDoc(article) else correspondingAuthorAffs <- NA

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

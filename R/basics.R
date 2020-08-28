#' GetAPIlink
#' Return E-utility links with parameters
#'
#' @param baseUrl a string of characters, default is "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "eserach", "efetch","elink", "esummary"
#' @param db a string of characters. Target database about which to gather statistics. Values must match https://www.ncbi.nlm.nih.gov/books/n/helpeutils/chapter2/#chapter2.chapter2_table1
#' @param id a list of strings. UID list. Either a single UID or a UID list
#' @param apiKey a string of characters. API Key of the user. NCBI will begin enforcing the practice of using an API key for sites that post more than 3 requests per second.
#' @param email a string of characters. E-mail address of the E-utility user.
#' @param retmode a string of characters. Retrieval type. Default is "XML"
#' @param term a string of characters. Entrez text query.
#' @param reldate a string of characters, When reldate is set to an integer n, the search returns only those items that have a date specified by datetype within the last n days.
#' @param datetype a string of characters. Type of date used to limit a search. The allowed values vary between Entrez databases, but common values are 'mdat' (modification date), 'pdat' (publication date) and 'edat' (Entrez date).
#' @param retmax a string of characters. Total number of UIDs from the retrieved set to be shown in the XML output (default=20). By default, ESearch only includes the first 20 UIDs retrieved in the XML output.
#' @param usehistory a string of characters. When usehistory is set to 'y', ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent E-utility call. Also, usehistory must be set to 'y' for ESearch to interpret query key values included in term or to accept a WebEnv as input.
#' @param retstart a string of characters.Sequential index of the first UID in the retrieved set to be shown in the XML output. This parameter can be used in conjunction with retmax to download an arbitrary subset of UIDs retrieved from a search.
#' @param tool a string of characters. Name of application making the E-utility call.
#' @param WebEnv a string of characters. Web environment string returned from a previous ESearch, EPost or ELink call. When provided, ESearch will post the results of the search operation to this pre-existing
#' @param cmd a string of characters. ELink command mode. The command mode specified which function ELink will perform. Some optional parameters only function for certain values of &cmd
#'
#' @return link
#' @export
#'
#' @examples GetAPIlink(endpoint = "elink",id="12", cmd = "llinks")
#' GetAPIlink(endpoint = "esearch",term="stroke", retmax=5)
#' GetAPIlink(endpoint = "esummary",id="5575286")
#' GetAPIlink(endpoint = "efetch",id="5575286")
#'
GetAPIlink <- function(baseUrl = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
                        endpoint = "esearch",
                        db = "pubmed",
                        id = "",
                        apiKey = "",
                        email = "",
                        retmode = "XML",
                        term = "",
                        reldate = "",
                        datetype = "",
                        retmax = 1000,
                        usehistory ="y",
                        retstart = "",
                        tool = "pubmedTools",
                        WebEnv = "",
                        cmd = ""
                        ) {
  baseUrl <- paste0(baseUrl, endpoint, ".fcgi?")

  db <- ifelse(db != "", ifelse(endpoint == "elink",  paste0("dbfrom=",db),  paste0("db=",db)),NA)
  id <- ifelse(length(id) > 0,  paste0("id=",paste0(id, collapse = ",")),NA)
  apiKey <- ifelse(apiKey != "", paste0("api_key=",apiKey),NA)
  email <- ifelse(email != "",  paste0("email=",email),NA)
  retmode <- ifelse(retmode != "", paste0("retmode=",retmode),NA)
  term <- ifelse(term != "", paste0("term=",term),NA)
  reldate <- ifelse(reldate != "", paste0("reldate=",reldate),NA)
  datetype <- ifelse(datetype != "",  paste0("datetype=",datetype),NA)
  retmax <- ifelse(retmax != "", paste0("retmax=",retmax),NA)
  usehistory <- ifelse(usehistory != "", paste0("usehistory=",usehistory),NA)
  retstart <- ifelse(retstart != "", paste0("retstart=",retstart),NA)
  tool <- ifelse(tool != "",  paste0("tool=",tool),NA)
  WebEnv <- ifelse(WebEnv != "",  paste0("WebEnv=",WebEnv),NA)
  cmd <- ifelse(cmd != "",  paste0("cmd=",cmd),NA)

  paras <- paste0(na.omit(c(db, id, apiKey,email,retmode,term,reldate,datetype,retmax,usehistory,retstart,tool,WebEnv,cmd)), collapse = "&")

  link <- paste0(baseUrl, paras)

  return(link)
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
GetContentWithLink <- function(link, waitTime) {
  httr::set_config(httr::config(http_version = 0))
  content = NULL
  while (is.null(content)) {
    tryCatch({
      Sys.sleep(waitTime)
      r0 <- httr::POST(as.character(link))
      content <- httr::content(r0, "text")
    }, error = function(e) {
      print(e)
    })
  }
  return(content)
}

#' GetJson
#'
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "eserach", "efetch","elink", "esummary"
#' @param db a string of characters. Target database about which to gather statistics. Values must match https://www.ncbi.nlm.nih.gov/books/n/helpeutils/chapter2/#chapter2.chapter2_table1
#' @param id a list of strings. UID list. Either a single UID or a UID list
#' @param apiKey a string of characters. API Key of the user. NCBI will begin enforcing the practice of using an API key for sites that post more than 3 requests per second.
#' @param email a string of characters. E-mail address of the E-utility user.
#' @param term a string of characters. Entrez text query.
#' @param reldate a string of characters, When reldate is set to an integer n, the search returns only those items that have a date specified by datetype within the last n days.
#' @param datetype a string of characters. Type of date used to limit a search. The allowed values vary between Entrez databases, but common values are 'mdat' (modification date), 'pdat' (publication date) and 'edat' (Entrez date).
#' @param retmax a string of characters. Total number of UIDs from the retrieved set to be shown in the XML output (default=20). By default, ESearch only includes the first 20 UIDs retrieved in the XML output.
#' @param usehistory a string of characters. When usehistory is set to 'y', ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent E-utility call. Also, usehistory must be set to 'y' for ESearch to interpret query key values included in term or to accept a WebEnv as input.
#' @param retstart a string of characters.Sequential index of the first UID in the retrieved set to be shown in the XML output. This parameter can be used in conjunction with retmax to download an arbitrary subset of UIDs retrieved from a search.
#' @param WebEnv a string of characters. Web environment string returned from a previous ESearch, EPost or ELink call. When provided, ESearch will post the results of the search operation to this pre-existing
#' @param cmd a string of characters. ELink command mode. The command mode specified which function ELink will perform. Some optional parameters only function for certain values of &cmd
#'
#' @return a XMLInternalDocument
#' @export
#'
#' @import jsonlite
#'
#' @examples GetJson(db = "pubmed", endpoint = "esearch", term="pinkeye", retmax=5)
#'
#'
GetJson <-
  function( endpoint = "",
            db = "",
            id = "",
            apiKey = "",
            email = "",
            term = "",
            reldate = "",
            datetype = "",
            retmax = 1000,
            usehistory ="y",
            retstart = "",
            WebEnv = "",
            cmd = "") {
    if(endpoint == "efetch")retmode = "" else retmode = "json"
    link <- GetAPIlink(db = db, endpoint = endpoint, id = id,  apiKey = apiKey, email = email, term =term, reldate =reldate, retmode = retmode, datetype = datetype, retmax = retmax, usehistory = usehistory,retstart=retstart,WebEnv=WebEnv,cmd=cmd)
    # The waiting time to retrive data from the API. Default is set to 0.4 to ensure less than 3 API calling.
    if(apiKey != "") waitTime = 0 else waitTime = 0.4
    
    content <- GetContentWithLink(link, waitTime)

    result_json <- jsonlite::parse_json(content)
    
    return(result_json)
  }

#' GetDoc
#'
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "eserach", "efetch","elink", "esummary"
#' @param db a string of characters. Target database about which to gather statistics. Values must match https://www.ncbi.nlm.nih.gov/books/n/helpeutils/chapter2/#chapter2.chapter2_table1
#' @param id a list of strings. UID list. Either a single UID or a UID list
#' @param apiKey a string of characters. API Key of the user. NCBI will begin enforcing the practice of using an API key for sites that post more than 3 requests per second.
#' @param email a string of characters. E-mail address of the E-utility user.
#' @param term a string of characters. Entrez text query.
#' @param reldate a string of characters, When reldate is set to an integer n, the search returns only those items that have a date specified by datetype within the last n days.
#' @param datetype a string of characters. Type of date used to limit a search. The allowed values vary between Entrez databases, but common values are 'mdat' (modification date), 'pdat' (publication date) and 'edat' (Entrez date).
#' @param retmax a string of characters. Total number of UIDs from the retrieved set to be shown in the XML output (default=20). By default, ESearch only includes the first 20 UIDs retrieved in the XML output.
#' @param usehistory a string of characters. When usehistory is set to 'y', ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent E-utility call. Also, usehistory must be set to 'y' for ESearch to interpret query key values included in term or to accept a WebEnv as input.
#' @param retstart a string of characters.Sequential index of the first UID in the retrieved set to be shown in the XML output. This parameter can be used in conjunction with retmax to download an arbitrary subset of UIDs retrieved from a search.
#' @param WebEnv a string of characters. Web environment string returned from a previous ESearch, EPost or ELink call. When provided, ESearch will post the results of the search operation to this pre-existing
#' @param cmd a string of characters. ELink command mode. The command mode specified which function ELink will perform. Some optional parameters only function for certain values of &cmd
#'
#' @return a XMLInternalDocument
#' @export
#'
#' @import xml2
#'
#' @examples GetDoc(db = "pubmed", endpoint = "esearch", term="stroke", retmax=5)
#' GetDoc(id = "5575286", db = "pmc", endpoint = "efetch")
#'
GetDoc <-
  function( endpoint = "",
    db = "",
    id = "",
    apiKey = "",
    email = "",
    term = "",
    reldate = "",
    datetype = "",
    retmax = 1000,
    usehistory ="y",
    retstart = "",
    WebEnv = "",
    cmd = "") {
    retmode <- "XML"
    link <- GetAPIlink(db = db, endpoint = endpoint, id = id,  apiKey = apiKey, email = email, term =term, reldate =reldate, datetype = datetype, retmax = retmax, usehistory = usehistory,retstart=retstart,WebEnv=WebEnv,cmd=cmd,retmode = retmode)
    # The waiting time to retrive data from the API. Default is set to 0.4 to ensure less than 3 API calling.
    if(apiKey != "") waitTime = 0 else waitTime = 0.4

    content <- GetContentWithLink(link, waitTime)
    doc <- xml2::read_xml(content, encoding = "UTF-8", useInternalNodes = TRUE, trim = FALSE)

    return(doc)
  }

#' RetriveXmlNodeValuefromDoc
#'
#' @param doc the parsed XML file
#' @param nodePosition the node position of the xml file
#'
#' @return the values of the node
#' @export
#' @examples  doc <- GetDoc(id = c("5575286", "4804230"),db= "pmc", endpoint="esummary")
#' RetriveXmlNodeValuefromDoc(doc, "//Id")
#'
#' @import xml2
#'
RetriveXmlNodeValuefromDoc <- function(doc, nodePosition) {
  nodes <- xml2::xml_find_all(doc, nodePosition)
  if (length(nodes) == 0) return(NA)
  results <- gsub("\t"," ",gsub("\n","",sapply(nodes, xml2::xml_text), fixed = T), fixed = T)
  results[which(results == "NA")] <- NA

  return(results)
}

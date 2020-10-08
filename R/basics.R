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
#' @param queryKey  a string of characters. an integer label called a query key
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
                        apiKey = "2ab938c0c3cfbfe6446b544c8e37c5a4e609",
                        email = "shihikoo@gmail.com",
                        retmode = "XML",
                        term = "",
                        reldate = "",
                        datetype = "",
                        retmax = 1000,
                        usehistory ="y",
                        retstart = "",
                        tool = "pubmedTools",
                        WebEnv = "",
                        cmd = "",
                       queryKey = ""
                        ) {
  baseUrl <- paste0(baseUrl, endpoint, ".fcgi?")

  db <- ifelse(db != "", ifelse(endpoint == "elink",  paste0("dbfrom=",db),  paste0("db=",db)),NA)
  id <- ifelse(length(id) > 0 & all(id  != ""),  paste0("id=", paste0(id, collapse = ",") ),NA)
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
  queryKey <- ifelse(queryKey != "",  paste0("query_key=",queryKey),NA)

  paras <- paste0(na.omit(c(db, id, apiKey,email,retmode,term,reldate,datetype,retmax,usehistory,retstart,tool,WebEnv,cmd,queryKey )), collapse = "&")

  link <- paste0(baseUrl, paras)
  return(link)
}

#' GetContentByPostLink
#'
#' @param link a string of characters
#' @param waitTime a number. Waiting of the program
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples baselink <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
#' link <- paste0(baselink, "efetch.fcgi?db=pubmed&rettype=xml&id=26502666")
#' content <- GetContentByPostLink(link, 0.3)
#'
#' @import httr stringr
#'
GetContentByPostLink <- function(link, waitTime = 0.3) {
  # httr::set_config(httr::config(http_version = 0))
  content <- NULL
  attampt <- 0
  while (is.null(content) & attampt < 5) {
    tryCatch({
      baselink <- stringr::str_split(link,"[?]")[[1]][1]
      paras <- stringr::str_split(link,"[?]")[[1]][2]
      paralist <- stringr::str_split(stringr::str_split(paras,"[&]")[[1]], "=")
      fineParalist <- lapply(paralist, function(x) x[[2]])
      names(fineParalist) <- lapply(paralist, function(x) x[[1]])
      # print("Send Post request")
      # print(fineParalist)
      Sys.sleep(waitTime*attampt)
      r0 <- httr::POST(as.character(baselink), body = fineParalist)
      # print("Receive Post request")

      content <- httr::content(r0, "text")
    }, error = function(e) {
      print(e)
    })
    attampt <- attampt + 1
  }
  return(content)
}

#' GetContentByGetLink
#'
#' @param link a string of characters
#' @param waitTime a number. Waiting of the program
#'
#' @return a string of characters of the returned content
#' @export
#'
#' @examples baselink <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
#' link <- paste0(baselink, "efetch.fcgi?db=pubmed&rettype=xml&id=26502666")
#' content <- GetContentByGetLink(link, 0.3)
#'
#' @import httr
#'
GetContentByGetLink <- function(link, waitTime = 0.3) {
  # httr::set_config(httr::config(http_version = 0))
  content <- NULL
  attampt <- 0

  link <- gsub("#","%23", gsub("\"","%22", gsub(" ", "+", link)))

  while (is.null(content) & attampt < 10) {
    tryCatch({
      # print("Send GET request")
      iwaitTime <- waitTime*(attampt+1)
      Sys.sleep(iwaitTime)
      r0 <- httr::GET(link)
      # print("Receive GET request")
      content <- httr::content(r0, "text")
      # print(content)
    }, error = function(e) {
      print(e)
    })
    attampt <- attampt + 1
  }
  return(content)
}

#' GetJson
#'
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "eserach", "efetch","elink", "esummary"
#' @param db a string of characters. Target database about which to gather statistics. Values must match https://www.ncbi.nlm.nih.gov/books/n/helpeutils/chapter2/#chapter2.chapter2_table1
#' @param id a list of strings. UID list. Either a single UID or a UID list
#' @param apiKey a string of characters. API Key of the user. NCBI will begin enforcing the practice of using an API key for sites that post more than 3 requests per second.
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
            term = "",
            reldate = "",
            datetype = "",
            retmax = 1000,
            usehistory ="y",
            retstart = "",
            WebEnv = "",
            cmd = "") {
    if(endpoint == "efetch") retmode = "" else retmode = "json"
    link <- GetAPIlink(db = db, endpoint = endpoint, id = id,  apiKey = apiKey, term =term, reldate =reldate, retmode = retmode, datetype = datetype, retmax = retmax, usehistory = usehistory,retstart=retstart,WebEnv=WebEnv,cmd=cmd)
    # The waiting time to retrive data from the API. Default is set to 0.4 to ensure less than 3 API calling.
    if(apiKey != "") waitTime = 0.3 else waitTime = 0.4
    # print(link)

    if(nchar(link) > 500) content <- GetContentByPostLink(link, waitTime) else content <-  GetContentByGetLink(link, waitTime)
    if(is.null(content)) return(NULL)
    if(jsonlite::validate(content) == FALSE) return(NULL)

    result_json <- jsonlite::parse_json(content)

    return(result_json)
  }

#' GetDoc
#'
#' @param endpoint a string of characters, valid endpoint of NCBI Utilities, e.g. "eserach", "efetch","elink", "esummary"
#' @param db a string of characters. Target database about which to gather statistics. Values must match https://www.ncbi.nlm.nih.gov/books/n/helpeutils/chapter2/#chapter2.chapter2_table1
#' @param id a list of strings. UID list. Either a single UID or a UID list
#' @param apiKey a string of characters. API Key of the user. NCBI will begin enforcing the practice of using an API key for sites that post more than 3 requests per second.
#' @param term a string of characters. Entrez text query.
#' @param reldate a string of characters, When reldate is set to an integer n, the search returns only those items that have a date specified by datetype within the last n days.
#' @param datetype a string of characters. Type of date used to limit a search. The allowed values vary between Entrez databases, but common values are 'mdat' (modification date), 'pdat' (publication date) and 'edat' (Entrez date).
#' @param retmax a string of characters. Total number of UIDs from the retrieved set to be shown in the XML output (default=20). By default, ESearch only includes the first 20 UIDs retrieved in the XML output.
#' @param usehistory a string of characters. When usehistory is set to 'y', ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent E-utility call. Also, usehistory must be set to 'y' for ESearch to interpret query key values included in term or to accept a WebEnv as input.
#' @param retstart a string of characters.Sequential index of the first UID in the retrieved set to be shown in the XML output. This parameter can be used in conjunction with retmax to download an arbitrary subset of UIDs retrieved from a search.
#' @param WebEnv a string of characters. Web environment string returned from a previous ESearch, EPost or ELink call. When provided, ESearch will post the results of the search operation to this pre-existing
#' @param cmd a string of characters. ELink command mode. The command mode specified which function ELink will perform. Some optional parameters only function for certain values of &cmd
#' @param queryKey  a string of characters. an integer label called a query key

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
    term = "",
    reldate = "",
    datetype = "",
    retmax = 1000,
    usehistory ="y",
    retstart = "",
    WebEnv = "",
    cmd = "",
    queryKey = "") {
    retmode <- "XML"
    link <- GetAPIlink(db = db, endpoint = endpoint, id = id,  apiKey = apiKey, term =term, reldate =reldate, datetype = datetype, retmax = retmax, usehistory = usehistory,retstart=retstart,WebEnv=WebEnv,cmd=cmd,retmode = retmode,queryKey=queryKey)
    # The waiting time to retrive data from the API. Default is set to 0.4 to ensure less than 3 API calling.
    if(apiKey != "")  waitTime = 0.3 else waitTime = 0.4
    # print(link)

    if(nchar(link) > 500) content <- GetContentByPostLink(link, waitTime) else content <- GetContentByGetLink(link, waitTime)

    if(is.null(content)) return(NULL)

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

#' clean_doi
#'
#' @param doi a doi string
#'
#' @return the values of the node
#' @export
#' @examples
#' clean_doi("https://doi.org/10.1212/01.wnl.0000260060.60870.89")
#'
clean_doi <-function(doi ){
  doi <- tolower(doi)
  doi <- gsub("doi:", "", doi)
  doi <- gsub("https://", "", doi)
  doi <- gsub("http://", "", doi)
  doi <- gsub("dx.doi.org/","", doi)
  doi <- gsub("doi.org/","", doi)
  return(doi)
}


#' clean_title
#' clean a title, preparing other process
#'
#' @param x a string
#'
#' @import textclean
#' @return the distance between two strings
#'
#' @export
#'
#' @examples
#' clean_title("I&rsquo;m testing.")
#'
clean_title <- function(x){
  x <- textclean::replace_html(x)
  x <- tolower(x)
  # x <- gsub("&rsquo;","'",x)
  return(x)
}

#' calculate_distance
#' Calculate distance between two strings
#'
#' @param x a string
#' @param y a string
#'
#' @return the distance between two strings
#'
#' @import stringdist
#'
#' @export
#'
#' @examples
#' calculate_distance("I am testing.","Testing is ")
#'
calculate_distance <- function(x, y){
  title1 = clean_title(x)
  title2 = clean_title(y)
  return(1-stringdist::stringdist(title1,title2,method='jw',p=0.1))
}


#' compare_year
#' compare two publication years is the same. Sometimes, the year has one year different for the same publication in two different databases
#'
#' @param year1 a number
#' @param year2 a number
#'
#' @return the distance between two strings
#'
#' @export
#'
#' @examples
#' compare_year(2001,2000)
#'
compare_year <- function(year1, year2){
  issame <- abs(year1 - year2) < 2
  if(is.na(issame)) return(FALSE)
  return(issame)
}




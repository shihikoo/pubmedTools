# library(httr)
# library(XML)
# library(foreach)
# library(doParallel)
# library(parallel)
# library(rjson)
# source('R/pubmedRead.R')
options("download.file.method" = "libcurl")
devtools::install_github("shihikoo/pubmedTools")

result <- fromJSON(file = "configure.json")
apiKey <- result$APIKey
pmids <- c("28852052", "29041955")
pmid <- pmids[1]
pmcid <- "4804230"

urls <-  pubmedTools::RetriveUrlsFromPmids(pmids, apiKey)
print(urls)

metaData <-  pubmedTools::RetriveMetaDataFromPmids(pmids, apiKey)
print(metaData)

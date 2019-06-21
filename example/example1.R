# library(httr)
# library(XML)
# library(foreach)
# library(doParallel)
# library(parallel)
# library(rjson)
# source('R/pubmedRead.R')

options("download.file.method" = "libcurl")
devtools::install_github("shihikoo/pubmedTools")
# library(pubmedTools)

result <- fromJSON(file = "configure.json")
apiKey <- result$APIKey
email <- "shihikoo@gmail.com"
pmids <- c("28852052", "29041955")
pmid <- pmids[1]
pmcid <- "4804230"
email <- "shihikoo@gmail.com"

urls <-  pubmedTools::RetriveUrlsFromPmids(pmids, apiKey,email)
print(urls)

metaData <-  pubmedTools::RetriveMetaDataFromPmids(pmids, apiKey,email)
print(metaData)


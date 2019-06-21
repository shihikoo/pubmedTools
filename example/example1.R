# library(httr)
# library(XML)
# library(foreach)
# library(doParallel)
# library(parallel)
# library(rjson)

result <- rjson::fromJSON(file = "configure.json")
apiKey <- result$APIKey
email <- "shihikoo@gmail.com"
pmids <- c("28852052", "29041955")
pmid <- pmids[1]
pmcid <- "4804230"
email <- "shihikoo@gmail.com"

# source('R/pubmedRead.R')
# print(GetPmidDoiFromPmcid(pmcid, apiKey,email))
# print(RetriveUrlsFromPmids(pmids, apiKey,email))
# print(RetriveMetaDataFromPmids(pmids, apiKey,email))

options("download.file.method" = "libcurl")
devtools::install_github("shihikoo/pubmedTools", force = T)
print(pubmedTools::GetPmidDoiFromPmcid(pmcid, apiKey,email))
print(pubmedTools::RetriveUrlsFromPmids(pmids, apiKey,email))
print(pubmedTools::RetriveMetaDataFromPmids(pmids, apiKey,email))


library(rjson)

apiKey <- rjson::fromJSON(file = "configure.json")$APIKey
email <- "shihikoo@gmail.com"
pmids <- c("28852052", "29041955")
pmid <- pmids[1]
pmcids <- c("5575286", "4804230")
pmcid <- pmcids[1]
email <- "shihikoo@gmail.com"
waitTime <- 0
# source('R/pubmedRead.R')
# print(GetPmidDoiFromPmcid(pmcid, apiKey,email))
# print(RetriveUrlsFromPmids(pmids, apiKey,email))
# print(RetriveMetaDataFromPmids(pmids, apiKey,email))

options("download.file.method" = "libcurl")
devtools::install_github("shihikoo/pubmedTools")
library(pubmedTools)

print(GetPmidDoiFromPmcid(pmcid, apiKey, email, waitTime = 0))
print(GetPmidDoiFromPmcidBatch(pmcids, apiKey, email, waitTime = 0))
print(RetriveUrlsFromPmids(pmids, apiKey, email, waitTime = 0, fulltext = T))
print(RetriveMetaDataFromPmids(pmids, apiKey, email, waitTime = 0))



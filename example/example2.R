library(rjson)
# options("download.file.method" = "libcurl")
# devtools::install_github("shihikoo/pubmedTools")
library(pubmedTools)
library(XML)
library(tidyr)
library(data.table)

apiKey <- rjson::fromJSON(file = "configure.json")$APIKey
email <- "shihikoo@gmail.com"

# print(GetPmidDoiFromPmcid(pmcid, apiKey, email))
# print(GetPmidDoiFromPmcidBatch(pmcids, apiKey, email))
# print(RetriveUrlsFromPmids(pmids, apiKey, email))
# print(GetMetaDataFromPmcid(pmcids, apiKey, email))
# print(GetMetaDataFromPmcidBatch(pmcids, apiKey, email))

doc <- XML::xmlParse(file = "example/pubmed_result.xml")
csvdf <- data.table::fread("example/pubmed_result.csv")

library(httr)
library(XML)
library(foreach)
library(doParallel)
library(parallel)
library(rjson)

# library(devtools)
install_github("shihikoo/pubmedTools")
# library(pubmedTools)
source('R/pubmedRead.R')

result <- fromJSON(file = "configure.json")
apiKey <- result$APIKey
print(apiKey)

pmids <- c("11748933", "29041955")
pmid <- pmids[1]
pmcid <- "4804230"

urls <-  RetriveUrlsFromPmids(pmids, apiKey)
print(urls)

info <-  RetriveInfoFromPmids(pmids, apiKey)
print(info)

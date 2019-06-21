library(httr)
library(XML)
library(foreach)
library(doParallel)
# install.packages("devtools")
# library(devtools)
# install_github("shihikoo/pubmedTools")
# library(pubmedTools)
library(rjson)
source('R/pubmedRead.R')


pmids <- c("11748933", "29041955")
urls <-  RetriveUrlsFromPmids(pmids)
print(urls)

result <- fromJSON(file = "configure.json")
print(result$APIKey)

apiKey <- result$APIKey

pmcid <- "4804230"

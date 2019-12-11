# pubmedTools

'pubmedTools' is an R package helping researchers extracting data from PubMed using Entrez Programming Utilities (E-utilities) https://www.ncbi.nlm.nih.gov/books/NBK25501/ 

It is highly recommonded to register a NCBI account and created an API Key. https://www.ncbi.nlm.nih.gov/books/NBK25497/

The tool contains different functions to retrive meta-data (such as DOI, pdf link, journal, year, author, corresponding author, affiliation etc.) using provided PubMed Id or PubMed Central Id. 

Example:

```r
devtools::install_github("shihikoo/pubmedTools")
library(pubmedTools)
pmids <- read.csv(file="example/pmid.csv")
pmids <- rbind(pmids,pmids)
output <- RetriveMetaDataFromPmidsBatch(pmids$x)
```


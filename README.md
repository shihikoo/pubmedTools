# pubmedTools

'pubmedTools' is an R package helping researchers extracting data from PubMed using Entrez Programming Utilities (E-utilities) https://www.ncbi.nlm.nih.gov/books/NBK25501/ 

It is highly recommonded to register a NCBI account and created an API Key. https://www.ncbi.nlm.nih.gov/books/NBK25497/

The tool contains different functions to retrive meta-data (such as DOI, pdf link, journal, year, author, corresponding author, affiliation etc.) using provided PubMed Id or PubMed Central Id. 

Example:

```r
install.packages("devtools")
devtools::install_github("shihikoo/pubmedTools")
library(pubmedTools)
searchTerm = "pinkeye"
pmids <- GetPmidsWithSearch(searchTerm)
output <- RetriveMetaDataFromPmidsBatch(pmids, columns = c("pmid","pmcid", "journal",
                       "journalCountry",
                       "publicationYear",
                       "funders",
                       "authors",
                       "affiliations",
                       "title",
                       "abstract","isbn",
                       "volume",
                       "issue",
                       "pages", "keywords","doi"))
```

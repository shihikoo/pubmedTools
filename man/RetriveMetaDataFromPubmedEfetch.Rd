% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubmedRead.R
\name{RetriveMetaDataFromPubmedEfetch}
\alias{RetriveMetaDataFromPubmedEfetch}
\title{RetriveMetaDataFromPubmedEfetch}
\usage{
RetriveMetaDataFromPubmedEfetch(doc, columns = "")
}
\arguments{
\item{doc}{an XMLInternalDocument class file read from a pubmed efetch xml file}

\item{columns}{a list of string of requested column names: "pmid","pmcid", "journal", "journalCountry","publicationYear","funders","authors","affiliations","title","abstract","isbn","volume","issue","pages", "keywords","doi"}
}
\value{
a list of metaDatarmation retrived from PubMed
}
\description{
RetriveMetaDataFromPubmedEfetch
}
\examples{
doc <- GetDoc(id = c("28852052", "29041955","31230181"), db = "pubmed", endpoint = "efetch")
RetriveMetaDataFromPubmedEfetch(doc, columns = c("pmid","pmcid","title","doi"))

doc <- GetDoc(id = c("31524133"), db = "pubmed", endpoint = "efetch")
RetriveMetaDataFromPubmedEfetch(doc, columns = c("pmid","pmcid","title","doi"))

doc <- GetDoc(id = c("20704052"), db = "pubmed", endpoint = "efetch")
RetriveMetaDataFromPubmedEfetch(doc, columns = c("pmid","pmcid","title","doi"))

}

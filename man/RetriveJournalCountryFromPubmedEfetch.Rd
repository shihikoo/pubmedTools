% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pubmedRead.R
\name{RetriveJournalCountryFromPubmedEfetch}
\alias{RetriveJournalCountryFromPubmedEfetch}
\title{RetriveJournalCountryFromPubmedEfetch}
\usage{
RetriveJournalCountryFromPubmedEfetch(doc)
}
\arguments{
\item{doc}{an XMLInternalDocument class file read from a pubmed efetch xml file}
}
\value{
a string
}
\description{
RetriveJournalCountryFromPubmedEfetch
}
\examples{
 doc <- GetDoc(id = "28852052", db = "pubmed", endpoint = "efetch")
 RetriveJournalCountryFromPubmedEfetch(doc)
}

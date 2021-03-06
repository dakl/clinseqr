% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getQDNAseq.R
\name{getQDNAseq}
\alias{getQDNAseq}
\title{Get segmented CNV data}
\usage{
getQDNAseq(reports, columnToUse = "segmented")
}
\arguments{
\item{reports}{data frame with reports}

\item{columnToUse}{column to use for getting values}
}
\value{
A data frame with segmented value, or other value
}
\description{
Get segmented CNV data
}
\examples{
#dat <- getReadcountPerChr(reports)
#dat <- getReadcountPerChr(reports, columnToUse="segmented")
}


% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/genomeplot.R
\name{genomeplot}
\alias{genomeplot}
\title{plot a copy number profile}
\usage{
genomeplot(segments, chrsizes, chrs_to_use)
}
\arguments{
\item{segments}{Data frame with segments from qdnaseq. Must include column chr, start, end and segmented, and *prob.}

\item{chrsizes}{Data frame with chromosome sizes.}

\item{chr_to_use}{Vector of chromosome names to use. If not used, intersection of segments and chrsizes will be used.}
}
\value{
A ggplot object
}
\description{
plot a copy number profile
}
\examples{
# genomeplot(segments, chrsizes)
}


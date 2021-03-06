% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{cleanphe}
\alias{cleanphe}
\title{Clean the phenotype data from TCGA}
\usage{
cleanphe(phe)
}
\arguments{
\item{phe}{data.frame, the raw expression matrix with colname of molecular id and individual ids.}
}
\value{
data.frame, the clean expression matrix with colname of molecular id and individual ids.
}
\description{
Given a phenotype data frame, delete the individual with NA .
}
\details{
nothing
}
\examples{
data(phe);phe = cleanphe(phe)
}

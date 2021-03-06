% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{status.table}
\alias{status.table}
\title{Table of stock status metrics}
\usage{
status.table(rep.list, rep.names = NULL)
}
\arguments{
\item{rep.list}{A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.}

\item{rep.names}{A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.}
}
\description{
Returns a data.frame of terminal year stock status metrics for each model in the \code{rep.list}.
Metrics include SBSBF0latest, MSY, BSMY and FMSY.
The output can be processed with the \code{xtable} package to make a table for RMarkdown or Latex reports.
}

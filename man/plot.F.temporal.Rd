% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.F.temporal.r
\name{plot.F.temporal}
\alias{plot.F.temporal}
\title{Compare the fishing mortality across different models.}
\usage{
plot.F.temporal(
  rep.list,
  par.list = NULL,
  rep.names = NULL,
  agg.years = TRUE,
  agg.regions = TRUE,
  agg.ages = NULL,
  yaxis.free = FALSE,
  palette.func = default.model.colours,
  save.dir,
  save.name,
  ...
)
}
\arguments{
\item{rep.list}{A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.}

\item{par.list}{Optional. A list of MFCLPar objects or a single MFCLPar object. Used for plotting juvenile and adult fishing mortality. If specified then agg.ages is ignored}

\item{rep.names}{A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.}

\item{agg.years}{TRUE or FALSE. Should model outputs be aggregated to an annual time step.}

\item{agg.regions}{TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.}

\item{agg.ages}{A vector of age classes to average F over. Default is all age classes.}

\item{yaxis.free}{TRUE or FALSE. Default is FALSE. If TRUE and agg.regions is also TRUE than the y-axis scales will be independent across regions, otherwise they will be shared so regional scaling will be apparent.}

\item{palette.func}{A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().}

\item{save.dir}{Path to the directory where the outputs will be saved}

\item{save.name}{Name stem for the output, useful when saving many model outputs in the same directory}

\item{...}{Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.}
}
\description{
Compare the fishing mortality across different models.
}

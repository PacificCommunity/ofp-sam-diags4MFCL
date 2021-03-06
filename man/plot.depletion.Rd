% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.depletion.r
\name{plot.depletion}
\alias{plot.depletion}
\title{Compare the estimated depletion across different models.}
\usage{
plot.depletion(
  rep.list,
  rep.names = NULL,
  agg.years = TRUE,
  agg.regions = TRUE,
  biomass.type = "SSB",
  LRP = NULL,
  TRP = NULL,
  palette.func = default.model.colours,
  save.dir,
  save.name,
  ...
)
}
\arguments{
\item{rep.list}{A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.}

\item{rep.names}{A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.}

\item{agg.years}{TRUE or FALSE. Should model outputs be aggregated to an annual time step.}

\item{agg.regions}{TRUE or FALSE. Should model outputs be aggregated across all regions are kept separate.}

\item{biomass.type}{Character string denoting the type of biomass plotted, 'SSB' or 'Total'}

\item{LRP}{Limit reference point. If not specified it is not plotted.}

\item{TRP}{Target reference point. If not specified it is not plotted.}

\item{palette.func}{A function to determine the colours of the models. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().}

\item{save.dir}{Path to the directory where the outputs will be saved}

\item{save.name}{Name stem for the output, useful when saving many model outputs in the same directory}

\item{...}{Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.}
}
\description{
Compare the estimated depletion across different models.
}

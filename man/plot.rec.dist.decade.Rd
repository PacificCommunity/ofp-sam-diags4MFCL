% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recruitment.plots.r
\name{plot.rec.dist.decade}
\alias{plot.rec.dist.decade}
\title{Plot recruitment distribution proportions by decade for a single model}
\usage{
plot.rec.dist.decade(
  rep,
  plot_type = "violin",
  overlay_data = FALSE,
  palette.func = default.model.colours,
  save.dir,
  save.name,
  ...
)
}
\arguments{
\item{rep}{A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.}

\item{plot_type}{Can be "violin" for a violin plot or "box" for a boxplot. The default is boxplot.}

\item{overlay_data}{Do you want to overlay the original data on the distribution? TRUE or FALSE (default).}

\item{palette.func}{A function to determine the colours of each area. The default palette has the reference model in black. It is possible to determine your own palette function. Two functions currently exist: default.model.colours() and colourblind.model.colours().}

\item{save.dir}{Path to the directory where the outputs will be saved}

\item{save.name}{Name stem for the output, useful when saving many model outputs in the same directory}

\item{...}{Passes extra arguments to the palette function. Use the argument all.model.colours to ensure consistency of model colours when plotting a subset of models.}
}
\description{
Plot distribution of proportion of total recruitment (averaged over year range) between quarters and regions across models. If only a single repfile is entered then plot the distribution of proportions across time
}

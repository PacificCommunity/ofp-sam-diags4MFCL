% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.effort.dev.penalties.R
\name{plot.effort.dev.penalties}
\alias{plot.effort.dev.penalties}
\title{Plot effort deviation penalties over time by fishery and region}
\usage{
plot.effort.dev.penalties(
  frqreal,
  par,
  fishery_map,
  fisheries = unique(fishery_map$fishery),
  save.dir,
  save.name
)
}
\arguments{
\item{frqreal}{A data.frame created by running realisations() on an object of type MFCLFrq.}

\item{par}{An object of MFCLPar that contains the effort deviations.}

\item{fishery_map}{A data.frame that describes which fishery is fishing in which region and with which gear. The columns are: fishery_name, region, gear and fishery (the reference number). Only fisheries in the fishery_map will be plotted.}

\item{fisheries}{Which fisheries to plot. Default is all of them.}

\item{save.dir}{Path to the directory where the outputs will be saved}

\item{save.name}{Name stem for the output, useful when saving many model outputs in the same directory}
}
\description{
If time varying effort weights are active (fish flag 66), the effort deviation penalties are taken from the 'penalty' column of the 'freq' object.
Otherwise, if the penalty for effort deviations flag (fish flag 13) is less than 0, the penalty is calculated as the square root of the normalised effort multiplied by the absolute value of the effort deviations flag, by fishery.
If the penalty for effort deviations flag (fish flag 13) is greater than or equal to 0, the penalty is set to the value of the effort deviations flag, by fishery.
Observations with missing effort have no penalty.
}

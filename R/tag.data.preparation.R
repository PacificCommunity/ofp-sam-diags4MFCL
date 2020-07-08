# Tag data preparation for plotting
# The tag plots need to combine data from different sources.
# It is easier to do that before we call the function, rather than doing it in the
# function.

#' Prepare data for tagging plots
#'
#' The data required for the tagging plots are in different sources (temporary_tag_report, tag files, par files).
#' This function pulls the data together into a single data.frame to be passed to the tag plotting functions.
#' @param tagrep A data.frame of tag returns from the temporary_tag_returns file. The output from the FLR4MFCL function \code{read.temporary_tag_report()}.
#' @param tagobs An object of type MFCLTag.
#' @param par An object of MFCLPar that contains the effort deviations.
#' @param fishery.map A data.frame that describes which fishery is fishing in which region and with which gear. The columns are: fishery_name, region, gear, fishery (the reference number)and optionally tag_recapture_name, if not provided will default to the gear and region. The map should contain entries for all fisheries to avoid data being inadvertently dropped.
#' @export
#' @import FLR4MFCL
tag.data.preparation <- function(tagrep, tagobs, par, fishery.map){
  if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }
  if (class(tagobs) != "MFCLTag"){
    stop("obs argument must of type of type 'MFCLTag'.")
  }

  # Need the par to get mixing periods by release group
  # Or get number of releases from the tag file
  tag_releases <- data.frame(rel.group = sort(unique(tagrep$rel.group)))
  # Get the mixing periods from the tag flags
  tag_releases$mixing_period <- flagval(par, (-10000 - tag_releases$rel.group + 1),1)$value
  tagrep$mixing_period <- tag_releases$mixing_period[tagrep$rel.group]

  # Also need the release dates and numbers
  # Could sum over length to get total release numbers by release event
  releases <- aggregate(list(rel.obs = releases(tagobs)$lendist),
    list(program=releases(tagobs)$program, rel.group=releases(tagobs)$rel.group, rel.region=releases(tagobs)$region, rel.year=releases(tagobs)$year, rel.month=releases(tagobs)$month),
  sum, na.rm=TRUE)
  tagdat <- merge(tagrep, releases, all=TRUE)

  # Sort out the release and recapture timestep
  no_seasons <- length(unique(tagdat$recap.month)) # this is unsafe, how best to get no seasons?
  tagdat$rel.ts <- tagdat$rel.year + (tagdat$rel.month-1)/12 + 1/24
  tagdat$recap.ts <- tagdat$recap.year + (tagdat$recap.month-1)/12 + 1/24
  tagdat$mixing_period_quarters <- tagdat$mixing_period / no_seasons
  # Period at liberty
  tagdat$period_at_liberty <- ((tagdat$recap.ts - tagdat$rel.ts) * no_seasons) + 1

  # Check to make sure that the fishery.map has the necessary columns
  if (is.null(fishery.map$gear)) stop("The fishery.map that you provided is missing a column called gear.")
  if (is.null(fishery.map$region)) stop("The fishery.map that you provided is missing a column called region.")
  if (is.null(fishery.map$fishery)) stop("The fishery.map that you provided is missing a column called fishery that must include all fishery numbers.")

  # fishery map here is the FULL set of tag recapture groups - not the subset
  # Argument to recovery plot is therefore vector of groups
  # Fill in the tag_recapture_group and if not present the tag_recapture_name
  if(any(duplicated(fishery.map))) stop("There are duplicates in your fishery map this should not happen")
  fishery.map = fishery.map[sort(fishery.map$fishery),]
  fishery.map$tag_recapture_group <- flagval(par, -1:-max(fishery.map$fishery),32)$value
  if (is.null(fishery.map$tag_recapture_name)) fishery.map$tag_recapture_name <- paste(fishery.map$gear,fishery.map$region)
  if (is.null(fishery.map$fishery_name)) fishery.map$fishery_name <- paste("Fishery",fishery.map$fishery)

  # Bring in recapture groups
  colnames(fishery.map)[colnames(fishery.map) == "fishery"] <- "recap.fishery"
  colnames(fishery.map)[colnames(fishery.map) == "region"] <- "recap.region"
  tagdat <- merge(tagdat, fishery.map[,c("recap.fishery", "recap.region", "tag_recapture_group", "tag_recapture_name")])

  # Reorder for friendliness back in calling function
  tagdat <- tagdat[order(tagdat$rel.group, tagdat$rel.ts),]

  return(tagdat)
}

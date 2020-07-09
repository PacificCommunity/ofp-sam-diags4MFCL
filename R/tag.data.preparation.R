# Tag data preparation for plotting
# The tag plots need to combine data from different sources.
# It is easier to do that before we call the function, rather than doing it in the plot function.



#' Prepare tag release data for tagging plots
#' 
#' The data required for the tagging plots are in different sources (temporary_tag_report, tag files, par files).
#' This function pulls the data together the release data from the tag file and the par file (for the mixing period).
#' This can be passed, along with the recapture data, into the tag plot functions
#' @param tagobs An object of type MFCLTag.
#' @param par An object of MFCLPar that contains the effort deviations.
#' @export
#' @import FLR4MFCL
tag.release.data.preparation <- function(tagobs, par){
  if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }
  if (class(tagobs) != "MFCLTag"){
    stop("obs argument must of type of type 'MFCLTag'.")
  }
  
  # Get total release numbers by release event
  tag_releases <- aggregate(list(rel.obs = releases(tagobs)$lendist),
    list(program=releases(tagobs)$program, rel.group=releases(tagobs)$rel.group, rel.region=releases(tagobs)$region, rel.year=releases(tagobs)$year, rel.month=releases(tagobs)$month),
  sum, na.rm=TRUE)
  # Bring in the mixing period
  tag_releases$mixing_period <- flagval(par, (-10000 - tag_releases$rel.group + 1),1)$value
  # What is the mixing period in terms of years?
  no_seasons <- dimensions(par)["seasons"]
  tag_releases$mixing_period_years <- tag_releases$mixing_period / no_seasons
  tag_releases$rel.ts <- tag_releases$rel.year + (tag_releases$rel.month-1)/12 + 1/24
  
  # Reorder for friendliness back in calling function
  tag_releases <- tag_releases[order(tag_releases$rel.group, tag_releases$rel.ts),]
  
  return(tag_releases)
}



#' Prepare data for tagging plots
#' 
#' The data required for the tagging plots are in different sources (temporary_tag_report, tag files, par files).
#' This function pulls the data together into a single data.frame to be passed to the tag plotting functions.
#' @param tagrep A data.frame of tag returns from the temporary_tag_returns file. The output from the FLR4MFCL function \code{read.temporary_tag_report()}.
#' @param tagobs An object of type MFCLTag.
#' @param par An object of MFCLPar that contains the effort deviations.
#' @param fishery.map A data.frame that describes which fishery is fishing in which region and with which gear. The columns are: fishery_name, region, gear, fishery (the reference number), tag_recapture_group and tag_recapture_name. The map should contain entries for all fisheries to avoid data being inadvertently dropped.
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
  
  # fishery map here is the FULL set of tag recapture groups - not the subset
  # Argument to recovery plot is therefore vector of groups
  # Bring in recapture groups
  colnames(fishery.map)[colnames(fishery.map) == "fishery"] <- "recap.fishery"
  colnames(fishery.map)[colnames(fishery.map) == "region"] <- "recap.region"
  tagdat <- merge(tagdat, fishery.map[,c("recap.fishery", "recap.region", "tag_recapture_group", "tag_recapture_name")])
  
  # Reorder for friendliness back in calling function
  tagdat <- tagdat[order(tagdat$rel.group, tagdat$rel.ts),]
  
  return(tagdat)
}

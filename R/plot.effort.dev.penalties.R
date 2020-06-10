# Effort deviation penalties
# Adapted from code from Matt: plot.effort.deviation.penalties.r and get.fish.grp.r


#' Plot effort deviation penalties over time by fishery and region
#' 
#' If time varying effort weights are active (fish flag 66), the effort deviation penalties are taken from the 'penalty' column of the 'freq' object.
#' Otherwise, if the penalty for effort deviations flag (fish flag 13) is less than 0, the penalty is calculated as the square root of the normalised effort multiplied by the absolute value of the effort deviations flag, by fishery.
#' If the penalty for effort deviations flag (fish flag 13) is greater than or equal to 0, the penalty is set to the value of the effort deviations flag, by fishery.
#' Observations with missing effort have no penalty.
#' @param frq An object of type MFCLFrq that contains the observed effort data.
#' @param par An object of MFCLPar that contains the effort deviations.
#' @param fishery_map A data.frame that describes which fishery is fishing in which region and with which gear. The columns are: fishery_name, region, gear and fishery (the reference number). Only fisheries in the fishery_map will be plotted.
#' @param save.dir Path to the directory where the outputs will be saved
#' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
#' @export
#' @import FLR4MFCL
#' @import magrittr
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_color_gradient
#' @importFrom ggplot2 scale_y_continuous
#' 
plot.effort.dev.penalties <- function(frq, par, fishery_map, save.dir, save.name){
  if (class(par) != "MFCLPar"){
    stop("par argument must of type of type 'MFCLPar'.")
  }
  if (class(frq) != "MFCLFrq"){
    stop("frq argument must of type of type 'MFCLFrq'.")
  }
  # Get the fishery realisations and tidy up
  frqreal <- realisations(frq)
  # Add timestep column for plotting - ignoring week
  frqreal$ts <- frqreal$year + (frqreal$month-1)/12 + 1/24  # month is mid-month
  # Tidy up missing values
  frqreal$effort[frqreal$effort < 0] <- NA
  frqreal$catch[frqreal$catch< 0] <- NA
  frqreal$penalty[frqreal$penalty< 0] <- NA
  # Add in the  map of what fishery is where, and what gear type to colour it
  # Just include the fisheries in the fishery_map - merge should handle that
  pdat <- merge(frqreal, fishery_map)
  
  # Bits from Matt's code
  # Pull out fishflag 13 (penalty of effort devs)
  # Pull out fishflag 66 (time varying effort wt)
  #tmp <- ifelse(parfl$ffl[, 13] == 0, 10, parfl$ffl[, 13])
  #tmp1 <- parfl$ffl[, 66]
  no_fisheries <- n_fisheries(frq)
  ff13 <- flagval(par, -(1:no_fisheries), 13)$value
  # Replace 0 penalty with penalty of 10 - why?
  ff13[ff13==0] <- 10
  ff66 <- flagval(par, -(1:no_fisheries), 66)$value
  
  # Add these penalties to the freq data
  #  basepen = tmp[fishery],
  #  timpen = tmp1[fishery]
  pdat$basepen <- ff13[pdat$fishery]
  pdat$timpen <- ff66[pdat$fishery]
  
  # From Matt's code - reproduce in base R
  #mu.eff <- pldat %>% group_by(fishery) %>% summarise(mu = mean(effort)) %>% as.data.frame
  #pldat$timpen[pldat$fishery==8] <- 1
  mean_effort <- aggregate(list(mean_effort = pdat$effort), list(fishery=pdat$fishery), mean, na.rm=TRUE)
  pdat <- merge(pdat, mean_effort)
  #pldat %<>% mutate(obs.sq.eff = effort / mu.eff$mu[fishery], sq.ratio = sqrt(0.1 + obs.sq.eff))
  pdat$obs_sq_eff <- pdat$effort / pdat$mean_effort
  pdat$sq_ratio <- sqrt(pdat$obs_sq_eff + 0.1)
  pdat$effpen <- ifelse(pdat$basepen < 0, pdat$sq_ratio * abs(pdat$basepen), pdat$basepen)
  # What is se column? Penalty?
  #pdat$effpen <- ifelse(pdat$timpen == 1, pdat$se, pdat$effpen)
  # If fishflag 66 is active use the penalty column, otherwise use the calcualted penalty
  pdat$effpen <- ifelse(pdat$timpen == 1, pdat$penalty, pdat$effpen)
  # Set penalty to NA where effort is NA
  pdat$effpen <- ifelse(is.na(pdat$effort), NA, pdat$effpen)
  
  # Plot away
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x=ts, y=effpen))
  p <- p + ggplot2::geom_line(aes(colour=gear), na.rm=TRUE)
  p <- p + ggplot2::facet_wrap(~region, scales="free")
  p <- p + ggplot2::labs(colour = "Fishery")
  p <- p + ggplot2::xlab("Time") + ggplot2::ylab("Effort deviation penalty")
  p <- p + ggthemes::theme_few()
  
  save_plot(save.dir, save.name, plot=p)
  
  return(p)
}
#' Compare Fishing Mortality Across Different Models
#'
#' @param x A list of \code{MFCLRep} objects or a single \code{MFCLRep} object.
#'        The reference model should be listed first.
#' @param par.list Optional list of \code{MFCLPar} objects or a single
#'        \code{MFCLPar} object. Used for plotting juvenile and adult fishing
#'        mortality. If specified then \code{agg.ages} is ignored.
#' @param rep.names A vector of character strings naming the models for plotting
#'        purposes. If not supplied, model names will be taken from the names in
#'        the \code{x} list (if available) or generated automatically.
#' @param agg.years \code{TRUE} or \code{FALSE}. Should model outputs be
#'        aggregated to an annual time step.
#' @param agg.regions \code{TRUE} or \code{FALSE}. Should model outputs be
#'        aggregated across all regions or kept separate.
#' @param agg.ages A vector of age classes to average F over. Default is all age
#'        classes.
#' @param yaxis.free \code{TRUE} or \code{FALSE}. If \code{TRUE} and
#'        \code{agg.regions} is also \code{TRUE} then the y-axis scales will be
#'        independent across regions, otherwise they will be shared to show
#'        regional scaling.
#' @param palette.func A function to determine the colours of the models. The
#'        default palette has the reference model in black. It is possible to
#'        determine your own palette function. Two functions currently exist:
#'        \code{default.model.colours} and \code{colourblind.model.colours}.
#' @param save.dir Path to the directory where the outputs will be saved.
#' @param save.name Name stem for the output, useful when saving many model
#'        outputs in the same directory.
#' @param \dots Extra arguments passed to \code{palette.func}. Use the argument
#'        \code{all.model.colours} to ensure consistency of model colours when
#'        plotting a subset of models.
#'
#' @importFrom FLR4MFCL dimensions fm m_at_age mat popN
#' @import magrittr
#' @importFrom data.table as.data.table rbindlist setnames
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_line ggplot ggsave ggtitle
#'             scale_color_manual xlab ylab
#'
#' @export

plot.F.temporal <- function(x, par.list=NULL, rep.names=NULL, agg.years=TRUE,
                            agg.regions=TRUE, agg.ages=NULL, yaxis.free=FALSE,
                            palette.func=default.model.colours, save.dir,
                            save.name, ...)
{
  # Global variables for R CMD check
  . <- ":=" <- adult <- age <- area <- dead <- dead.adult <- dead.juv <- NULL
  f <- F.adult <- F.juv <- juv <- model <- N <- region <- season <- time <- NULL
  year <- NULL

  # Check and sanitise input MFCLRep arguments and names
  x <- check.rep.args(rep=x, rep.names=rep.names)
  rep.names <- names(x)

  dt.list <- as.list(rep(NA, length(x)))
  names(dt.list) <- names(x)

  if(is.null(par.list))
  {
    if(is.null(agg.ages))
    {
      agg.ages <- 1:length(m_at_age(x[[1]]))
    }

    for(i in 1:length(dt.list))
    {
      tmp.rep <- x[[i]]
      tmp.name <- names(x)[i]
      if(agg.years)
      {
        mlab <- "Annual"
        if(agg.regions)
        {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,dead:=f*N] %>%
            .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season)] %>% .[,F:=(dead/N)] %>% .[,.(F=mean(F)*4),by=year] %>%
            setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
        } else {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,dead:=f*N] %>%
            .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season,area)] %>% .[,F:=(dead/N)] %>% .[,.(F=mean(F)*4),by=.(year,area)] %>%
            setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
        }
      } else {
        mlab <- "Seasonal"
        if(agg.regions)
        {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,dead:=f*N] %>%
            .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season)] %>% .[,F:=(dead/N)] %>%
            .[,model:=tmp.name] %>% .[,region:="All regions"] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
        } else {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,dead:=f*N] %>%
            .[age %in% agg.ages,.(dead=sum(dead),N=sum(N)),by=.(year,season,area)] %>% .[,F:=(dead/N)] %>%
            setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
        }
      }

      # clean up
      rm(list=c("tmp.rep", "tmp.name"))
    }

    # combine into single data.table
    plot.dt <- rbindlist(dt.list) %>%
      .[,model:=factor(as.character(model),levels=rep.names)]

    # make plot
    # Get colours - pass ... to palette.func
    colour_values <- palette.func(selected.model.names=names(x), ...)
    g <- plot.dt %>%
      ggplot() + theme_few() +
      geom_hline(yintercept=0) +
      xlab("Year") + ylab("Fishing mortality") +
      ggtitle(paste0(mlab, " average F, age classes: ",
                     paste0(range(agg.ages), collapse="-"))) +
      geom_line(aes(x=time, y=F, color=model), size=1) +
      scale_color_manual("Model", values=colour_values)
    if(yaxis.free)
    {
      g <- g + facet_wrap(~region, scales="free_y")
    } else {
      g <- g + facet_wrap(~region)
    }
  } else {
    if(!is.null(agg.ages))
    {
      message("You have supplied the par file so agg.ages will be ignored.\n",
              "Juvenile and adult F will be plotted instead.")
    }

    par.list <- check.par.args(par=par.list, par.names=rep.names)
    par.names <- names(x)

    for(i in 1:length(dt.list))
    {
      tmp.rep <- x[[i]]
      tmp.par <- par.list[[i]]
      tmp.name <- names(x)[i]

      prop.adult <- mat(tmp.par)
      # find the maximum and make the rest also = 1
      prop.adult[rev(order(prop.adult))[1]:length(prop.adult)] <- 1
      prop.juv <- 1 - prop.adult

      if(agg.years)
      {
        mlab <- "Annual"
        if(agg.regions)
        {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
            .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season)] %>%
            .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>% .[,.(F.juv=mean(F.juv)*4,F.adult=mean(F.adult)*4),by=year] %>%
            setnames(.,c("year"),c("time")) %>% .[,model:=tmp.name] %>% .[,region:="All regions"]
        } else {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
            .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season,area)] %>%
            .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>% .[,.(F.juv=mean(F.juv)*4,F.adult=mean(F.adult)*4),by=.(year,area)] %>%
            setnames(.,c("year","area"),c("time","region")) %>% .[,model:=tmp.name]
        }
      } else {
        mlab <- "Seasonal"
        if(agg.regions)
        {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
            .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season)] %>%
            .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>%
            .[,model:=tmp.name] %>% .[,region:="All regions"] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
        } else {
          dt.list[[i]] <- as.data.table(fm(tmp.rep)) %>% merge(., as.data.table(popN(tmp.rep)),by=c("age","year","unit","season","area","iter")) %>%
            setnames(.,c("value.x","value.y"),c("f","N")) %>%
            .[,age:=as.numeric(age)] %>% .[,year:=as.numeric(year)] %>% .[,season:=as.numeric(season)] %>% .[,area:=as.numeric(area)] %>%
            .[,juv:=prop.juv[age]*N] %>% .[,adult:=prop.adult[age]*N] %>% .[,dead.juv:=f*juv] %>% .[,dead.adult:=f*adult] %>%
            .[,.(dead.juv=sum(dead.juv),juv=sum(juv),dead.adult=sum(dead.adult),adult=sum(adult)),by=.(year,season,area)] %>%
            .[,F.juv:=(dead.juv/juv)] %>% .[,F.adult:=(dead.adult/adult)] %>%
            setnames(.,c("area"),c("region")) %>% .[,model:=tmp.name] %>% .[,time:=year+(season-1)/dimensions(tmp.rep)["seasons"]]
        }
      }

      # clean-up
      rm(list=c("tmp.rep", "tmp.name", "tmp.par", "prop.adult", "prop.juv"))
    }

    # combine into single data.table
    plot.dt <- rbindlist(dt.list) %>%
      .[,model:=factor(as.character(model),levels=rep.names)]

    # make plot
    # Get colours - pass ... to palette.func
    colour_values <- palette.func(selected.model.names=names(x), ...)
    g <- plot.dt %>%
      ggplot() + theme_few() +
      geom_hline(yintercept=0) +
      xlab("Year") + ylab("Fishing mortality") +
      ggtitle(paste0(mlab, " average adult (solid) and juvenile (dashed) F")) +
      geom_line(aes(x=time, y=F.adult, color=model), size=1) +
      geom_line(aes(x=time, y=F.juv, color=model), size=1,
                linetype="longdash") +
      scale_color_manual("Model", values=colour_values)
    if(yaxis.free)
    {
      g <- g + facet_wrap(~region, scales="free_y")
    } else {
      g <- g + facet_wrap(~region)
    }
  }

  # write out
  if(!missing(save.dir))
  {
    if(missing(save.name))
    {
      stop("please specify save.dir")
    } else {
      if(!dir.exists(save.dir)) dir.create(save.dir, recursive=TRUE)
      ggsave(paste0(save.name, ".png"), plot=g, device="png", path=save.dir,
             scale=1, width=9, height=9, units="in")
    }
  }

  return(g)
}

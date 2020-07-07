## Function to take a directory and create a subfolder that will contain all of the plots that can be made for a single model. Within the directory there must be a .frq, .par, and .rep file and optionally a .tag and a temporary_tag_report

#' Create all of the diagnostic plots from a given directory
#'
#' Give a directory that contains the output of an MFCL run and the function will automatically find the largest par file (or if is exists the out.par) to create all the figures currently available and save in a subdirectory called DiagnosticPlots
#' @param rundir A directory string that tells where to get the files
#' @param spp The species abbreviation that comes before
#' @param fdescloc optional location of fdesc.txt file that contains information for labels of figures if not included within the folder. the fdesc.txt is a file with 7 columns and an entry for each fishery in the model. The columns should be named 1. num 2. gear_long  3. method  4. code  5. gear 6. flag 7. region. This file creates all of the labels for the fisheries.
#' @param par optional name of par file you you want plotted otherwise finds and uses largest number value
#' @param outdir An optional directory path that tells where the figures are to be saved. The function will automatically create a directory called DiagnosticPlots within dir if not provided an outdir.
#' @param ... Additional parameters that can be fed to the palette? Not functional
#' @export
#' @import FLR4MFCL
#' @import magrittr

plot.everything <- function(rundir,spp='skj',parname=NULL,fdescloc=NULL,outdir=NULL,...){
    ## check if last value in string is '/' if is not add it in
    if (substr(rundir,nchar(rundir),nchar(rundir))!='/') rundir=paste0(rundir,'/')
    olddir=getwd()
    ## Set working directory and load some files
    setwd(rundir)
    if (is.null(outdir)) outdir = paste0(rundir,'DiagnosticPlots/')
    if (! dir.exists(outdir)) dir.create(outdir)
    ## Since not recursive do a check to make sure it exists
    if (! dir.exists(outdir)) stop("The outdir that you specified does not exist and cannot be created because the parent directory does not exist")

    ## Keep out.par or find the largest par file and keep that
    if (is.null(parname)){
        if('out.par' %in% list.files(rundir)) {
            parname='out'
        }else{
            numparf=list.files(path=rundir,patt='[0-9]{1,2}\\.par$')
            parname= numparf %>% gsub('.par','', .) %>% as.numeric %>% sort(decreasing=TRUE) %>% '['(1)
            if (parname <10) parname=paste0('0',parname)
        }
    }

    ## Check to make sure that the directory exists
    if (! dir.exists(rundir)) stop("You fool. You've given a directory that does not exist!")

    ## check if files exist and if they do then read them in
    if (file.exists(paste0(spp,'.frq'))){ #.frq
        frq=read.MFCLFrq(paste0(spp,'.frq'))
    } else {stop(paste0("Error: The rundir supplied does not contain a ",spp,".frq file. The rundir supplied was:\n",rundir,"\n"))}
    if (file.exists(paste0(spp,'.ini'))){ #.ini
        ini=read.MFCLIni(paste0(spp,'.ini'))
    } else {stop(paste0("Error: The rundir supplied does not contain a ",spp,".ini file. The rundir supplied was:\n",rundir,"\n"))}
    firstyear <- as.numeric(range(frq)["minyear"])
    Nfish <- n_fisheries(frq)
    binwd=lf_range(frq)["LFWidth"]
    nbins=lf_range(frq)["LFIntervals"]
    bin1=lf_range(frq)["LFFirst"]
    LnBins = seq(bin1,by=binwd,length.out=nbins)

    par = read.MFCLPar(paste0(parname,".par"),firstyear)
    rep = read.MFCLRep(paste0("plot-",parname,".par.rep"))
    Nages=dimensions(rep)["agecls"]
    Nseasons=dimensions(rep)["seasons"]

    if (!is.null(fdescloc)){            #fdesc
        labels <- read.table(fdescloc, header=TRUE)
    } else if (!file.exists("fdesc.txt")){ stop("Error: You did not supply a location for the fdesc.txt and the rundir supplied does not contain a fdesc.txt file. You need to create a file with 7 columns and an entry for each fishery in the model. The columns should be named 1. num 2. gear_long  3. method  4. code  5. gear 6. flag 7. region. This file creates all of the labels for the fisheries.  The rundir supplied was:\n",rundir,"\n")
    } else {
        labels <- read.table( paste0(rundir,"fdesc.txt"), header=TRUE)
    }

    ## Make the tag related plotting functions all at once
    if (file.exists(paste0(spp,'.tag'))) {
        tag=read.MFCLTag(paste0(spp,".tag"))
        tagrep=read.temporary_tag_report("temporary_tag_report")
        fishmaptag=data.frame(fishery_name=labels$code,region=labels$region,gear=labels$method,fishery=1:Nfish)
        prepTag=tag.data.preparation(tagrep,tag,par,fishmaptag)
        plot.tag.attrition(prepTag,facet="none",plot.diff=TRUE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagAllDiffScaled")
        plot.tag.attrition(prepTag,facet="program",plot.diff=TRUE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagProgramDiffScaled")
        plot.tag.attrition(prepTag,facet="region",plot.diff=TRUE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagRegionDiffScaled")

        plot.tag.attrition(prepTag,facet="none",plot.diff=TRUE,scale.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagAllDiff")
        plot.tag.attrition(prepTag,facet="program",plot.diff=TRUE,scale.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagProgramDiff")
        plot.tag.attrition(prepTag,facet="region",plot.diff=TRUE,scale.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagRegionDiff")

        plot.tag.attrition(prepTag,facet="none",plot.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagAllLiberty")

        plot.tag.attrition(prepTag,facet="program",plot.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagProgramLiberty")

        plot.tag.attrition(prepTag,facet="region",plot.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagRegionLiberty")


        plot.tag.returns.time(prepTag,recapture.groups=1:max(prepTag$tag_recapture_group),plot.diff=TRUE,scale.diff=TRUE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagReturnsScaledDiff")

        plot.tag.returns.time(prepTag,recapture.groups=1:max(prepTag$tag_recapture_group),plot.diff=TRUE,scale.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagReturnsDiff")
        plot.tag.returns.time(prepTag,recapture.groups=1:max(prepTag$tag_recapture_group),plot.diff=FALSE,scale.diff=FALSE,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="TagReturnsTime")

        plot.tag.return.proportion(prepTag,plot.type="point",save.dir=outdir,save.name="TagPropRegionPoint")
        plot.tag.return.proportion(prepTag,plot.type="point",save.dir=outdir,save.name="TagPropRegionBar")

    } else {
        print('There is no tag file in the rundir supplied')
    }
    ## Natural mortality
    plot.nat.mort(rep,show.legend=FALSE,save.dir=outdir,save.name="NatMort")

    ## Fits to length and weight composition data
    if ( file.exists('length.fit')) {
        prepLen=length.fit.preparation('length.fit')
        tmpfsh=unique(prepLen$fishery[prepLen$obs>0])
        plot.overall.composition.fit(prepLen,tmpfsh,labels$code[tmpfsh],outdir,"LengthCompFit")
        rm(tmpfsh)
    }

    if ( file.exists('weight.fit')) {
        prepWt=length.fit.preparation('weight.fit')
        tmpfsh=unique(prepWt$fishery[prepWt$obs>0])
        plot.overall.composition.fit(prepWt,tmpfsh,labels$code[tmpfsh],outdir,"WeightCompFit")
        rm(tmpfsh)
    }

    ## calculate range of ages to plot one for each year
    tmpage=seq(1,Nages,Nseasons)
    plot.age.dist(rep,tmpage,save.dir=outdir,save.name="RegionalAgeDistribution")

    ## Plot the biomass and depletion seasonally and regionally for the adult population and the total population
    plot.biomass.contributions(par,rep,save.dir=outdir,save.name="BiomassContributions")
    plot.biomass(rep,agg.years=FALSE,agg.regions=TRUE,save.dir=outdir,save.name="SeasonalAdultBiomass")
    plot.biomass(rep,agg.years=TRUE,agg.regions=FALSE,save.dir=outdir,save.name="RegionalAdultBiomass")
    plot.depletion(rep,agg.years=FALSE,agg.regions=TRUE,save.dir=outdir,save.name="SeasonalAdultDepletion")
    plot.depletion(rep,agg.years=TRUE,agg.regions=FALSE,save.dir=outdir,save.name="RegionalAdultDepletion")
    plot.biomass(rep,biomass.type="Total",agg.years=FALSE,agg.regions=TRUE,save.dir=outdir,save.name="SeasonalTotalBiomass")
    plot.biomass(rep,biomass.type="Total",agg.years=TRUE,agg.regions=FALSE,save.dir=outdir,save.name="RegionalTotalBiomass")
    plot.depletion(rep,biomass.type="Total",agg.years=FALSE,agg.regions=TRUE,save.dir=outdir,save.name="SeasonalTotalDepletion")
    plot.depletion(rep,biomass.type="Total",agg.years=TRUE,agg.regions=FALSE,save.dir=outdir,save.name="RegionalTotalDepletion")

    ## Plot the effort penalty specified within the frq file
    fisheryMap = data.frame(fishery=1:Nfish,fishery_name=labels$code,region=labels$region,gear=labels$gear)
    plot.effort.dev.penalties(frq,par,fisheryMap,outdir,"EffortDevPenalties")
    tmpfsh=which(flagval(par, -(1:Nfish), 66)$value > 0)

    ## Plot the effort deviates for the standardized fisheries
    plot.effort.devs(frq,par,fisheries=tmpfsh,fishery.names=labels$code[tmpfsh],show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="EffortDevs")
    ##plot.frqit
    ## Growth curve
    plot.growth(rep,save.dir=outdir,save.name="MeanLengthAge")

    ## Various maturity ogives
    plot.maturity(par,show.legend=FALSE,save.dir=outdir,save.name="MaturityAge")
    plot.maturity(par,Length=TRUE,LnBins=LnBins,show.legend=FALSE,save.dir=outdir,save.name="MaturityLength")

    ## Movement averages and by season
    plot.movement.matrix(par,save.dir=outdir,save.name="MovementAverage")
    plot.movement.matrix(par,season.vec=1:Nseasons,save.dir=outdir,save.name="MovementSeasonal")

    ## Observed Vs predicted Catch
    plot.pred.obs.catch(rep,fisheries=1:Nfish,fishery_names=labels$code,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="CatchDiff")

    ## Observed VS predicted CPUE
    plot.pred.obs.cpue(frq,par,fisheries=tmpfsh,fishery_names=labels$code[tmpfsh],save.dir=outdir,save.name="CPUEDiff")

    ## plot recruitment deviations
    plot.rec.devs(par,show.legend=FALSE,show.points=TRUE,save.dir=outdir,save.name="RecruitmentDevs")

    ## Plot decadal recruitment distribution
    plot.rec.dist(rep,year_range=range(frq)["minyear"]:range(frq)["maxyear"],save.dir=outdir,save.name="RegionalRecuitment")
    plot.rec.dist.decade(rep,save.dir=outdir,save.name="DecadalRecruitmentDist")

    ## Various plots of selectivity
    fshtmp=which(!duplicated(flagval(par, -(1:Nfish), 24)$value))
    plot.selectivity(rep,sel.basis="AGE",fisheries=fshtmp,save.dir=outdir,save.name="SelectivityAgeGroups")
    plot.selectivity(rep,sel.basis="LENGTH",fisheries=fshtmp,save.dir=outdir,save.name="SelectivityLengthGroups")
    plot.selectivity(rep,sel.basis="AGE",fisheries=1:Nfish,save.dir=outdir,save.name="SelectivityAge")
    plot.selectivity(rep,sel.basis="LENGTH",fisheries=1:Nfish,save.dir=outdir,save.name="SelectivityLength")

    ## Plot Stock recruitment relationship
    plot.srr(rep,show.legend=FALSE,save.dir=outdir,save.name="SRR")
    ## status.table(rep)




    setwd(olddir)
}

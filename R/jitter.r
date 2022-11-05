
## Code written by Matthew Vincent
#' jitter
#'
#' Function to take a par file and change the values within based on an input SD to allow to perform a jittering analysis for a model in MFCL
#' This probably only works for a single species model and has not been coded to be useable by the multispecies model
#'
#' This function is still preliminary and in development. Currently things that are not completed includin those below
#' new orthogonal coefficients
#' extra fisheries pars except for 1,2,4
#' length based selectivity parameters
#' selectivity deviates
#' mean natural mortality and deviates/ functional forms
#' growth curve deviates ??? different from von Bertalannfy growth deviates which is accounted for
#' Seasonal griwth parameter
#' age dependent movement coefficients
#' nonlinear movement coefficients
#'
#' Probably others that I didn't notice or don't have any idea about such as the Lagrangian
#'
#' @param object: An object of class MFCLPar
#'
#' @return An object of class MFCLPar
#'
#'
#' @export
#' @docType methods
#' @rdname par-methods
setGeneric('jitter',function(par,sd,seed) standardGeneric('jitter'))
setMethod("jitter", signature(par='MFCLPar',sd='numeric',seed='numeric'),function(par,sd,seed) {
            set.seed(seed)
            nFish=dimensions(par)["fisheries"]
            nAge=dimensions(par)["agecls"]
            nSeasons=dimensions(par)["seasons"]
            nYears=dimensions(par)["years"]
            nAgeYr=nAge/nSeasons
            nReg=dim(region_pars(par))[2]

            ## tag fish rep
            if (flagval(par,2,198)$value==1){
                nRRpars=max(tag_fish_rep_grp(par))
                maxRR=flagval(par,1,33)$value/100
                ## Randomly draw reporting rates from uniform distribution for each possibly reporting rate
                if(length(which(tag_fish_rep_flags(par)==0))>0)
                {
                  orig.rep.rate = tag_fish_rep_rate(par)
                  idx.fixed = which(tag_fish_rep_flags(par)==0)

                  x=runif(nRRpars,0,maxRR)
                  matcher=match(tag_fish_rep_grp(par),1:nRRpars)
                  tag_fish_rep_rate(par) <- matrix(x[matcher],dim(tag_fish_rep_grp(par)))
                  tag_fish_rep_rate(par)[idx.fixed] = orig.rep.rate[idx.fixed]

                } else {
                  x=runif(nRRpars,0,maxRR)
                  matcher=match(tag_fish_rep_grp(par),1:nRRpars)
                  tag_fish_rep_rate(par) <- matrix(x[matcher],dim(tag_fish_rep_grp(par)))
                }

            }

            ## percent maturity
            if (flagval(par,2,188)$value>0){ #If converting maturity at length to age using a spline
              mat(par) <- mat(par)*rnorm(length(mat(par)),1,sd)
              mat(par) <- mat(par)/max(mat(par))
            }

            ## total population scaling parameter
            if (flagval(par,2,31)$value==1){  #If totpop is estimated
              tot_pop(par) <- tot_pop(par)+rnorm(1,0,sd)
            }

            ## Recruitment deviates
            if (flagval(par,2,30)$value==1){
              rel_rec(par) <- rel_rec(par)*rnorm(length(rel_rec(par)),1,sd)
            }

            ## fishery selectivity
            uniqueSels=max(flagval(par,-1:-nFish,24)$value)
            for (i in 1:uniqueSels){         #loop over fisheries
              Selsfish=which(flagval(par,-1:-nFish,24)$value==i)
              if (flagval(par,-Selsfish[1],48)$value==1){ #If selectivity estimated
                ## ## Is selectivity non functional
                ## if (flagval(par,-Selsfish[1],57)$value==0) nsels=agecls
                ## ## Is selectivity logistic
                ## if (flagval(par,-Selsfish[1],57)$value==1) nsels=2
                ## ## Is selectivity double normal
                ##   if (flagval(par,-Selsfish[1],57)$value==2) stop("I don't know how many parameters are used by the double normal, so you will need to code this yourself")
                ## ## Is selectivity cubic spline
                ## if (flagval(par,-Selsfish[1],57)$value==3 & flagval(par,-Selsfish[1],61)$value>0) nsels=flagval(par,-Selsfish[1],61)$value
                NewSel=c(aperm(fishery_sel(par)[,,Selsfish[1]],c(4,1,2,3,5,6)))+rnorm(nAge,0,sd)
                fishery_sel(par)[,,Selsfish] <- aperm(array(NewSel,c(nSeasons,nAgeYr,1,length(Selsfish),1,1)),c(2,3,4,1,5,6))
              }
            }

            ## natural mortality

            ## average catchability coefficients
            if (any(flagval(par,-1:-nFish,1)$value==1)){
              for (i in 1:max(flagval(par,-1:-nFish,60)$value)){
                matcher=flagval(par,-1:-nFish,60)$value==i
                av_q_coffs(par)[,,matcher,,,] <- av_q_coffs(par)[,,matcher,,,]*rnorm(1,1,sd)
              }
            }


            ## movement parameters
            if (flagval(par,2,68)$value==1){
              diff_coffs(par) <- diff_coffs(par)*rnorm(length(diff_coffs(par)),1,sd)
              ## Check to make sure that all parameters are greater than 0 and less than 3
              if (any(diff_coffs(par)<=0))
                diff_coffs(par)[diff_coffs(par)<=0]=1e-16
              if (any(diff_coffs(par)>3))
                diff_coffs(par)[diff_coffs(par)>3]=2.9999999
            }

            ## xmovement coefficients
            if (flagval(par,2,184)$value==1){
              xdiff_coffs(par) <- xdiff_coffs(par)*rnorm(length(xdiff_coffs(par)),1,sd)
            }

            ## regional recruitment distribution
            if(sum(subset(flags(par),flagtype==-100000)$value>0) >0){
              # identify 'free' regions
              idx.free = which(subset(flags(par),flagtype==-100000)$value==1)
              must.sum = 1-sum(region_pars(par)[1,-idx.free])
              rand.dist = c(rmultinom(1,500,region_pars(par)[1,idx.free]))
              rand.dist = (rand.dist/sum(rand.dist))*must.sum # normalize and make sum to orginial proportion
              region_pars(par)[1,idx.free] = rand.dist
            }

            ## regional recruitment variation
            if (flagval(par,2,71)$value==1){
              region_rec_var(par)[] <- 0
            }

            ## Effort Devs
            if (flagval(par,2,34)$value==1){
              for (i in 1:nFish){
                if (flagval(par,-i,4)$value>1){
                  effort_dev_coffs(par)[[i]] <- effort_dev_coffs(par)[[i]]+rnorm(length(effort_dev_coffs(par)[[i]]),0,sd)
                  effort_dev_coffs(par)[[i]][effort_dev_coffs(par)[[i]]>flagval(par,2,35)$value]=flagval(par,2,35)$value-(1e-4)
                  effort_dev_coffs(par)[[i]][effort_dev_coffs(par)[[i]]<  -flagval(par,2,35)$value]= -flagval(par,2,35)$value+(1e-4)
                }
              }
            }

            ## extra fishery paramters

            for (i in 1:nFish){
              ## 1 and 2 seasonal catchability
              if (flagval(par,-i,27)$value==1){
                fish_params(par)[1:2,i]=fish_params(par)[1:2,i]*rnorm(2,1,sd)
              }

            }

            ## 4 variance for tag negative binomial
            if (any(flagval(par,-1:-nFish,43)$value==1)){
                nVars=max(flagval(par,-1:-nFish,44)$value)

                for (i in 1:nVars){
                    matcher=flagval(par,-1:-nFish,44)$value==i
                    if (all(flagval(par,-1:-nFish,43)$value[matcher]==1)){
                        fish_params(par)[4,matcher]=fish_params(par)[4,matcher]*rnorm(1,1,sd)
                    }
                }
            }

            ## Age Pars
            ## 2 M deviations

            ## 3 deviations from von Bertalanffy curve
            if (flagval(par,1,173)$value>1 & flagval(par,1,184)$value>0){
              growth_devs_age(par) <- growth_devs_age(par)*rnorm(nAge,1,sd)
            }

            ## 4 growth curve deviates ??? unused?
            ## 5 log natural mortality

            ## Region Pars
            if (any(flagval(par,-100000,1:nReg)$value==1)) {
              estRegs=flagval(par,-100000,1:nReg)$value==1
              region_pars(par)[1,estRegs]=region_pars(par)[1,estRegs]*rnorm(length(region_pars(par)[1,estRegs]),1,sd)
            }

            ## time varying catchability ???




            ## Growth Parameters
            ## L1
            if (flagval(par,1,12)$value==1){
              growth(par)[1] <- growth(par)[1]*rnorm(1,1,sd)
            }

            ## L2
            if (flagval(par,1,13)$value==1){
              growth(par)[2] <- growth(par)[2]*rnorm(1,1,sd)
            }

            ## k
            if (flagval(par,1,14)$value==1){
              growth(par)[3] <- growth(par)[3]*rnorm(1,1,sd)
            }

            ## Richards
            if (flagval(par,1,227)$value==1){
              richards(par) <- richards(par)+rnorm(1,0,sd)
            }

            ## Seasonal growth parameters

            ## Variance parameters
            if (flagval(par,1,15)$value==1){
              growth_var_pars(par)[1]=growth_var_pars(par)[1]*rnorm(1,1,sd)
              while(growth_var_pars(par)[1] <growth_var_pars(par)[1,2] | growth_var_pars(par)[1] > growth_var_pars(par)[1,3]) {
                growth_var_pars(par)[1]=growth_var_pars(par)[1]*rnorm(1,1,sd)
              }
            }
            if (flagval(par,1,16)$value==1){
              growth_var_pars(par)[2]=growth_var_pars(par)[2]*rnorm(1,1,sd)
              while(growth_var_pars(par)[2] <growth_var_pars(par)[2,2] | growth_var_pars(par)[2] > growth_var_pars(par)[2,3]) {
                growth_var_pars(par)[2]=growth_var_pars(par)[2]*rnorm(1,1,sd)
              }
            }

            ## new orthogonal coefficients

            ## grouped_catch_dev_coffs
            if (any(flagval(par,-1:-nFish,10)$value==1)){
              for (i in 1:max(flagval(par,-1:-nFish,29)$value)){
                matcher=which(flagval(par,-1:-nFish,29)$value==i)
                if (flagval(par,-matcher[1],10)$value==1){
                  catch_dev_coffs(par)[[i]]=catch_dev_coffs(par)[[i]]+rnorm(length(catch_dev_coffs(par)[[i]]),0,sd)
                }
              }
            }

            return(par)
})

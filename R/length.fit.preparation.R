# Function to read and process the length.fit file
# This basically all taken from Yukio's code plot_overall.composition.fit.gg.r
# Some small edits from me


#' Read in and process length.fit file
#' 
#' Hoovers out the observed ann predicted composite (i.e. summed over time) catches at length from the length.fit file.
#' It should also work with the weight.fit file but hasn't yet been tested.
#' Does not handle multispecies.
#' @param filname The filename of the length.fit file, including the location.
#' @export
#' @import FLR4MFCL

length.fit.preparation <- function(filename){
  
  # Read the whole dang thing - it's pretty huge
  dat <-readLines(filename)
  
  # This whole first section is sort of metadata stuff
  # Pulling out length vectors, number of fisheries etc
  # Get the version number - needed because older versions have different spacing
  version<-ifelse(strsplit(dat[1],split=" +")[[1]][1]=='#', strsplit(dat[1],split=" +")[[1]][3], 1)
  # Determine the number of fisheries from file header
  # Could possibly interrogate from dat but we'll use Yukio's code for now
  Nfsh <- scan(filename, nlines=1, skip=ifelse(version==1,2,3)) - 1
  # Determine the number of lines in the matrix for each fishery, from file header
  Nskips <- scan(filename, nlines=1, skip=ifelse(version==1,4,5))
  # Get parameters no. bins, first bin size, bin width
  size.pars <- scan(filename, nlines=1, skip=ifelse(version==1,1,2))  
  # Construct the size bins from the file header
  sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])
  # Figure the number of species in the length.fit and stop if more than 1
  fishSpPtr <- ifelse(version>=2, scan(filename,nlines=1,skip=7), NA)
  nsp <- ifelse(version>=2,length(unique(fishSpPtr)),1)
  # Change this to stop can only do with single species / sex model for now
  if(nsp>1){
    stop("More than 1 species/sex in the file. Not able to handle yet. Stopping.")
  }
  # Default is all of them, 1:31
  VecFsh <-1:Nfsh
  # Identify the lines of the observed size frequencies for the fisheries
  # These are lines under the # fishery totals bit at the bottom of the file
  LineKeep <- (VecFsh-1) * (Nskips + 6) + 1
  
  # Now we start processing the data
  # All the blocks in the file with # fishery 1 etc, are observed and predicted
  # numbers at length at age in each time step (see manual)
  # Only want the stuff at bottom of the file which has the data summed over time
  # Read in the file as text - run time could be reduced by only reading in from '# fishery totals' down but no skiplines argument in readLines - will have a hunt
  # Remove all unwanted data above the fishery totals
  dat <- dat[(grep("totals",dat)+4):length(dat)]

  # Get the observed data
  # This is the only observed data we want keep - pulls out vector for the fishery then skips down to the next fishery and grabs vector, etc. etc.
  # This should match the freq file? It does
  dat.obs <- dat[LineKeep]
  # Get it in the right format and transpose
  # To a matrix length class (rows), fishery (column)
  #dat.obs <- as.data.frame(t(read.table(text=dat.obs, nrows=length(LineKeep))))
  dat.obs <- t(read.table(text=dat.obs, nrows=length(LineKeep)))
  # Do the same for the predicted
  dat.pred <- dat[LineKeep+1]
  dat.pred <- t(read.table(text=dat.pred, nrows=length(LineKeep)))
  
  # Put together into data.frame
  out <- data.frame(fishery=rep(VecFsh,each=size.pars[1]),
                    length=rep(sizebins, Nfsh),
                    obs = c(dat.obs),
                    pred = c(dat.pred))
  return(out)
}


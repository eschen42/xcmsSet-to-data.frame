# progress bar for as.data.frame.xcmsSet
pctComplete <- function(
  progress = 0, last.progress = 0,
  total = 100, increment = 10,
  suffix = " ", method = cat, ...) 
{
  if (increment < 1)
    increment <- 1
  if (total < 0)
    total <- 100
  pct <- round(progress * 100 / total)
  if ( pct >= increment 
               + last.progress
               - last.progress%% increment )
  {
    method(paste("", pct, suffix, sep = ""),
           ...)
  }
  pct
}

# produce a data frame with attributes from an xcmsSet object that are interesting to me
as.data.frame.xcmsSet <- function(filled, output.method = cat, ...) {
  # extract the peak list
  peak.list <- filled@peaks
  # name the peak attributes that are to be extracted
  selected.column.names <- c('mz', 'rt', 'into', 'sample')
  # Make syntactically valid names out of character vectors.
  selected.columns <- make.names(colnames(peak.list)) %in% selected.column.names
  output.method("Now computing data.frame from peak list.  Please be patient. :)\n", ...)
  # create a big list with the selected attributes
  data.big.list <- peak.list[,selected.columns]
  # create a data frame from the big list
  ddf <- as.data.frame(data.big.list)
  # note that after retention time correction, ddf[,"rt"] in the extracted df has the corrected retention time
  # set the sample_name column
  ddf[,"sample_name"] <- sampnames(filled)[ddf$sample]
  # determine the number of groups
  groupidx.length <- length(filled@groupidx)
  # initialize group, feature, rtcor, and mzcor columns
  rep.times <- length(ddf$mz)
  ddf[,"group"] <- rep.int(x = 0, times = rep.times)
  ddf[,"feature"] <- rep.int(x = "", times = rep.times)
  ddf[,"rtcor"] <- rep.int(x = 0, times = rep.times)
  ddf[,"mzcor"] <- rep.int(x = 0, times = rep.times)
  # prepare to iterate over the groups
  output.method("% complete: ", ...)
  pct <- 0
  # I am arbitrarily using 0.1 minute resolution corrected retention time to distinguish features.
  #    If the correction was successful, this seems like it should work.
  mypaste <- function(...) { sprintf( fmt = "mz %.5f @ rt %.1f @ grp %d", ...) }
  for (i in 1:groupidx.length) {
    # this iterates at about 1 to 2 iterations per second for 96,000 groups and 440 samples.
    pct <- pctComplete(progress = i, last.progress = pct, increment = 5, total = groupidx.length)
    # extract the collection of group indexes for for this group
    myslice <- filled@groupidx[[i]]
    # assign the group index and median rt and mz for each sample-feature spanned by the group
    ddf$group[ myslice ] <- i
    ddf$rtcor[ myslice ] <- filled@groups[ i , "rtmed" ]
    ddf$mzcor[ myslice ] <- filled@groups[ i , "mzmed" ]
    # set the feature name for each sample-feature spanned by the group;
    #    one group may contain several features
    ddf$feature[ myslice ] <- do.call( mypaste, ddf[ myslice, c("mzcor", "rt", "group") ] )
  }
  # rename the intensity column
  colnames(ddf)[colnames(ddf) == "into"] <- "intensity"
  output.method(" done creating data.frame\n", ...)
  ddf
}


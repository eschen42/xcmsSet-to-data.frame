# construct a data.frame from an xcmsSet object that has undergone 'group'
# prerequisite: library(xcms)

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
as.data.frame.xcmsSet.peaks <- function(filled, output.method = cat, ...) {
  # extract the peak list
  peak.list <- filled@peaks
  # extract index mapping the group number to the sample numbers comprised by that group
  groupidx <- filled@groupidx
  # name the peak attributes that are to be extracted
  selected.column.names <- c('mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'intb', 'maxo', 'sn', 'sample')
  # Make syntactically valid names out of character vectors.
  selected.columns <- make.names(colnames(peak.list)) %in% selected.column.names
  output.method("Now computing data.frame from peak list.  Please be (very, very) patient. :)\n", ...)
  # create a big list with the selected attributes
  data.big.list <- peak.list[,selected.columns]
  # create a data frame from the big list
  ddf <- as.data.frame(data.big.list)
  # note that after retention time correction, ddf[,"rt"] in the extracted df has the corrected retention time
  # get the sample names from the xcmsSet object
  sampnames_df <- as.data.frame(sampnames(filled), stringsAsFactors = FALSE)
  # set the sample names in the data.frame as a new 'sample_name' column
  ddf[,"sample_name"] <- sampnames_df[ filled$sample, 1 ]
  # initialize the group_id column
  ddf[,"group_id"] <- rep( 0, length(filled$mz) )
  # prepare to iterate over the groups
  output.method("% complete: ", ...)
  # fill in ddf$group_id
  pct <- 0
  i <- 0
  groupidx.length <- length(groupidx)
  junk <-
    lapply(
      groupidx, 
      function(x) {
        i <<- 1 + i
        pct <<- pctComplete(progress = i, last.progress = pct, increment = 1, total = groupidx.length)
        lapply(
          x, 
          function(y) {
            ddf[y,"group_id"] <<- i
          }
        )
        NULL
      }
    )

  # rename the intensity column
  colnames(ddf)[colnames(ddf) == "intb"] <- "intensity_area"
  colnames(ddf)[colnames(ddf) == "maxo"] <- "intensity_peak"
  output.method(" done creating data.frame\n", ...)
  ddf
}


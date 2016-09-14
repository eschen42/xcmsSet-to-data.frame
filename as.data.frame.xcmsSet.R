# construct a data.frame from an xcmsSet object that has undergone 'group'
# prerequisite: library(xcms)

# # progress bar for as.data.frame.xcmsSet
# pctComplete <- function(
#   progress = 0, last.progress = 0,
#   total = 100, increment = 10,
#   suffix = " ", method = cat, ...) 
# {
#   if (increment < 1)
#     increment <- 1
#   if (total < 0)
#     total <- 100
#   pct <- round(progress * 100 / total)
#   if ( pct >= increment 
#                + last.progress
#                - last.progress%% increment )
#   {
#     method(paste("", pct, suffix, sep = ""),
#            ...)
#   }
#   pct
# }

as.data.frame.xcmsSet.peaks.make.peak2groupid.lut <- function(grouped) {
 # construct the peak-to-groupId lookup table
 # fetch the goupidx
 grouped_idx <- grouped@groupidx
 #message(format(Sys.time(), "%Y-%m-%d %X - entered as.data.frame.xcmsSet.peaks.make.peak2groupid.lut"))
 #message(format(Sys.time(), "%Y-%m-%d %X - creating groupidx_groupids ..."))
 # create the values for the lookup table (LUT)
 i <- 0 ; groupidx_groupids <- sapply( grouped_idx, function(x) { i <<- i + 1 ; rep(i, times=length(x)) } )
 #message(format(Sys.time(), "%Y-%m-%d %X - creating groupid_lut - this is SLOW ..."))
 # create the incomplete peak-to-group_id LUT; this is missing the sample peaks with no groups - SLOW
 #   This took 15 minutes for about 2,000,000 peaks and about 100,000 groups.
 groupid_lut <- data.frame( 
     peak  = Reduce("c", grouped_idx), 
     group = Reduce("c", groupidx_groupids)
   )
 # now find the peaks that are missing from groupid_lut
 #message(format(Sys.time(), "%Y-%m-%d %X - creating sampleid_count and sampleid_array ..."))
 sampleid_count <- length(grouped@peaks[,1])
 sampleid_array <- 1:sampleid_count
 # now make the LUT for the peaks not in groupid_lut - this is quick
 #message(format(Sys.time(), "%Y-%m-%d %X - creating nogroupid_lut ..."))
 nogroupid_lut <- data.frame(
     peak  = sampleid_array, 
     group = rep(0, times = sampleid_count)
   )[ !(sampleid_array %in% groupid_lut$peak),  ]
 # combine groupid_lut and nogroupid_lut - this is quick
 #message(format(Sys.time(), "%Y-%m-%d %X - creating peak_to_group_lut ..."))
 peak_to_group_lut <- data.frame(
     peak  = c(groupid_lut$peak,  nogroupid_lut$peak ),
     group = c(groupid_lut$group, nogroupid_lut$group)
   )
 #message(format(Sys.time(), "%Y-%m-%d %X - leaving as.data.frame.xcmsSet.peaks.make.peak2groupid.lut"))
 # sort by peak ID and return the result
 peak_to_group_lut[ with(peak_to_group_lut,order(peak)), ]
 # use this as follows:
 #   new_ddf_head[,"group_id"] <- peak_to_group_lut[ peak_to_group_lut$peak %in% 1:length(new_ddf_head$mz), 2]
}

# produce a data frame with attributes from an xcmsSet object that are interesting to me
as.data.frame.xcmsSet.peaks <- function(grouped, output.method = cat, include.group.id = FALSE, ...) {
  # extract the peak list
  peak.list <- grouped@peaks
  # extract index mapping the group number to the sample numbers comprised by that group
  groupidx <- grouped@groupidx
  # name the peak attributes that are to be extracted
  selected.column.names <- c('mz', 'mzmin', 'mzmax', 'rt', 'rtmin', 'rtmax', 'intb', 'maxo', 'sn', 'sample')
  # Make syntactically valid names out of character vectors.
  selected.columns <- make.names(colnames(peak.list)) %in% selected.column.names
  if ( include.group.id ) {
    output.method("Now computing data.frame from peak list.  Please be (very, very) patient. :)\n", ...)
  } else {
    output.method("Now computing data.frame from peak list ... \n", ...)
  }
  # create a big list with the selected attributes
  data.big.list <- peak.list[,selected.columns]
  # create a data frame from the big list
  ddf <- as.data.frame(data.big.list)
  # note that after retention time correction, ddf[,"rt"] in the extracted df has the corrected retention time
  # get the sample names from the xcmsSet object
  sampnames_df <- as.data.frame(sampnames(grouped), stringsAsFactors = FALSE)
  # get the sample IDs from the peak list
  sample_ids_df <- peak.list[,"sample"]
  # set the sample names in the data.frame as a new 'sample_name' column
  ddf[,"sample_name"] <- sampnames_df[ sample_ids_df, 1 ]
  if ( include.group.id ) {
    # make the table to look up group_id from peak sequence number
    peak_to_group_lut <- as.data.frame.xcmsSet.peaks.make.peak2groupid.lut(grouped)
    #message(length(peak_to_group_lut[,1]))
    #message(show(tail(peak_to_group_lut, n=1)))
    #message(length(ddf[,1]))
    #message(show(tail(ddf, n=1)))
    #message(show(head(peak_to_group_lut)))
    # set the group_id column
    ddf[,"peak_id"] <- 1:length(ddf$mz)
    ddf[,"group_id"] <- peak_to_group_lut[,2]
  }
  # rename the intensity column
  colnames(ddf)[colnames(ddf) == "intb"] <- "intensity_area"
  colnames(ddf)[colnames(ddf) == "maxo"] <- "intensity_peak"
  output.method(" done creating data.frame\n", ...)
  ddf
}
# vim: sw=2 ts=3 et ai :

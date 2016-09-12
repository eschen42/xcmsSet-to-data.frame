near <- function(
  dataframe, # may be specified as identifier or by name
  criterion, # may be specified as identifier or by name
  expected   # must be specified as identifier or literal; is.numeric(expected) must be true
){
  # Function 'near' returns rows from a data.frame that have values in the criterion column within a given tolerance
  #   For example, if the 'mzdel' option is set to 0.00005 with the statement 'options(mzdel=0.00005)',
  #     then the expression 'near(dataframe = unfilled.df, criterion = mz, expected = 431.09906)'
  #     will return a new data.frame whose rows are those rows of 'unfilled.df' where mz is within 0.00005 of 431.09906

  # allow dataframe to be specified as identifier or by name
  d_f <- if ("character" != typeof(substitute(dataframe))) dataframe else get(dataframe) 
  # return no rows if expected is not numeric
  if (!is.numeric(expected)) {
    warning("near: argument 'expected' is not numeric")
    return ( d_f[FALSE,] )
  }
  # allow criterion to be specified as identifier or by name
  cr <- paste0(substitute(criterion))
  # look up delta among options;
  #   note that to set an option 'foodel', the syntax is 'options(foodel = 1)'
  del_name <- paste0(cr,"del")
  del <- getOption(del_name)
  del <- if (is.null(del)) 0 else del
  # return a data.frame with rows where criterion
  return ( d_f[ abs(d_f[,cr] - expected) < del,] )
}

# > options(digits=9)
# > options(mzdel=0.00005)
# > near(unfilled.df, mz, 443.2581)
#                 mz        rt   intensity sample      sample_name group                             feature     rtcor      mzcor
# 1158477 443.258068 724.01978  511615.393    229 294_130325122658 22511 mz 443.25910 @ rt 724.0 @ grp 22511 724.33778 443.259104
# 1218170 443.258086 724.01870  300270.681    242              305 22511 mz 443.25910 @ rt 724.0 @ grp 22511 724.33778 443.259104
# 1232104 443.258057 724.01978  291080.080    246              309 22511 mz 443.25910 @ rt 724.0 @ grp 22511 724.33778 443.259104
# 1281283 443.258108 724.09378  540164.780    257 319_130326140622 22511 mz 443.25910 @ rt 724.1 @ grp 22511 724.33778 443.259104
# 1304499 443.258057 724.70590 4555307.967    261 322_130325133259 22511 mz 443.25910 @ rt 724.7 @ grp 22511 724.33778 443.259104
# 1346170 443.258112 725.09152  429855.640    271              331 22511 mz 443.25910 @ rt 725.1 @ grp 22511 724.33778 443.259104


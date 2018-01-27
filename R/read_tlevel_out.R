#' READ outputs of TLEVEL.OUT
#'
#' @param project.path
#' @param out.file
#' @param output
#' @param warn
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read.tlevel.out<- function(project.path, out.file = "T_Level.out", output = NULL,
         warn = FALSE, ...){
playone

   if(is.null(output) | missing(output)) {
            output = output = c("rTop", "rRoot", "vTop", "vRoot",
                                "vBot", "sum(rTop)", "sum(rRoot)",
                                "sum(vTop)", "sum(vRoot)", "sum(vBot)",
                                "hTop", "hRoot", "hBot", "RunOff",
                                "sum(Runoff)", "Volume", "sum(Infil)",
                                "sum(Evap)", "TLevel", "Cum(WTrans)",
                                "SnowLayer")

      }

    # tlevel_out<- read.table(file.path(project.path, out.file),
    #                        header = T, sep = "", dec = ".",
    #                        na.strings = "NA", colClasses = NA, as.is = TRUE,
    #                        skip = 6, check.names = FALSE, fill = T,
    #                        strip.white = FALSE, blank.lines.skip = TRUE,
    #                        comment.char = "#",
    #                        allowEscapes = FALSE, flush = FALSE,
    #                        stringsAsFactors = default.stringsAsFactors(),
    #                        fileEncoding = "", encoding = "unknown")

    options(warn = -1)
 tlevel_out = data.table::fread(input = file.path(project.path, out.file),
                  fill = TRUE, blank.lines.skip = T,  skip = 6, header = T)

 tlevel_out =  apply(tlevel_out, MARGIN = 2, FUN = as.numeric)
 tlevel_out = na.omit(tlevel_out)
 tlevel_out = data.frame(tlevel_out, check.names = FALSE, row.names = NULL)

### These steps are required to get a continuous sum(*) variables because
 ### for each run, these variables start at 0 at the begining of simulation

 tstart_ind = which(tlevel_out$TLevel == 1)

 sum_cols_ind = grep("sum", names(tlevel_out))
 sum_col_names = names(tlevel_out)[sum_cols_ind]

  for(i in 2: length(tstart_ind)){
        run1_totals = tlevel_out[(tstart_ind[i]-1), sum_cols_ind]

    if(i == length(tstart_ind)){
       run_i_ind = tstart_ind[i]:nrow(tlevel_out)
    } else {
          run_i_ind = tstart_ind[i]:(tstart_ind[i+1]-1)
    }
       tout_j = tlevel_out[run_i_ind, ]
   for(j in sum_col_names) {
         tlevel_out[run_i_ind, j] = tout_j[, j] + run1_totals[[j]]
   }

 }

optins(warn = 0)

  return(tlevel_out)

}

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
read.T_level.out<- function(project.path, out.file = "T_Level.out", output = NULL, warn = FALSE, ...){


   if(is.null(output) | missing(output)) {
            output = output = c("rTop", "rRoot", "vTop", "vRoot",
                                "vBot", "sum(rTop)", "sum(rRoot)",
                                "sum(vTop)", "sum(vRoot)", "sum(vBot)",
                                "hTop", "hRoot", "hBot", "RunOff",
                                "sum(Runoff)", "Volume", "sum(Infil)",
                                "sum(Evap)", "TLevel", "Cum(WTrans)",
                                "SnowLayer")

      }

    tlevel_out<- read.table(file.path(project.path, out.file),
                           header = T, sep = "", dec = ".",
                           na.strings = "NA", colClasses = NA, as.is = TRUE,
                           skip = 6, check.names = FALSE, fill = T,
                           strip.white = FALSE, blank.lines.skip = TRUE,
                           comment.char = "#",
                           allowEscapes = FALSE, flush = FALSE,
                           stringsAsFactors = default.stringsAsFactors(),
                           fileEncoding = "", encoding = "unknown")

  tlevel_out = tlevel_out[-c(1, nrow(tlevel_out)), ]

 tlevel_out =  apply(tlevel_out, MARGIN = 2, FUN = as.numeric)
 tlevel_out = data.frame(tlevel_out, check.names = FALSE, row.names = NULL)

  return(tlevel_out)

}

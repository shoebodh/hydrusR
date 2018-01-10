#' Read outputs of ALEVEL.OUT
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
read.A_level.out<- function(project.path,out.file = "A_Level.out", output = NULL, warn = FALSE, ...) {

   if(is.null(output) | missing(output)) {
      output = c("sum(rTop)", "sum(rRoot)", "sum(vTop)", "sum(vRoot)",
                                "sum(vBot)", "hTop", "hRoot", "hBot", "A-level")

      }

    alevel_out<- read.table(file.path(project.path, out.file),
                           header = T, sep = "", dec = ".",
                           na.strings = "NA", colClasses = NA, as.is = TRUE,
                           skip = 2, check.names = FALSE, fill = T,
                           strip.white = FALSE, blank.lines.skip = TRUE,
                           comment.char = "#",
                           allowEscapes = FALSE, flush = FALSE,
                           stringsAsFactors = default.stringsAsFactors(),
                           fileEncoding = "", encoding = "unknown")
    alevel_out = alevel_out[-c(1, nrow(alevel_out)), ]

    alevel_out =  apply(alevel_out, MARGIN = 2, FUN = as.numeric)
    alevel_out = data.frame(alevel_out, check.names = FALSE, row.names = NULL)

    return(alevel_out)

}

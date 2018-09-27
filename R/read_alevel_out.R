#' Read outputs of ALEVEL.OUT
#'
#' @param project.path path of hydrus project
#' @param out.file name of teh alevel file: "A_Level.out" is default output file.
#' @param output vector of output names that should be read
#' @param warn Logical for displaying/suppressing warnings producted by data.table::fread()
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read.alevel.out<- function(project.path, out.file = "A_Level.out", output = NULL, warn = FALSE, ...) {

  if(is.null(output) | missing(output)) {
    output = c("sum(rTop)", "sum(rRoot)", "sum(vTop)", "sum(vRoot)",
               "sum(vBot)", "hTop", "hRoot", "hBot", "A-level")

  }

  # alevel_out<- read.table(file.path(project.path, out.file),
  #                        header = T, sep = "", dec = ".",
  #                        na.strings = "NA", colClasses = NA, as.is = TRUE,
  #                        skip = 2, check.names = FALSE, fill = T,
  #                        strip.white = FALSE, blank.lines.skip = TRUE,
  #                        comment.char = "#",
  #                        allowEscapes = FALSE, flush = FALSE,
  #                        stringsAsFactors = default.stringsAsFactors(),
  #                        fileEncoding = "", encoding = "unknown")

  options(warn = -1)

  alevel_out = data.table::fread(input = file.path(project.path, out.file),
                                 fill = TRUE, blank.lines.skip = T,
                                 skip = 2, header = T)

  alevel_out =  apply(alevel_out, MARGIN = 2, FUN = as.numeric)
  alevel_out = na.omit(alevel_out)

  alevel_out = data.frame(alevel_out, check.names = FALSE, row.names = NULL)

  astart_ind = which(alevel_out$`A-level` == 1)

  sum_cols_ind = grep("sum", names(alevel_out))
  sum_col_names = names(alevel_out)[sum_cols_ind]

  if(length(astart_ind) == 1){

    alevel_out = alevel_out

  }else {

    for(i in 2: length(astart_ind)){

      # run1_totals = alevel_out[(astart_ind[i]-1), sum_cols_ind]

      if(i == length(astart_ind)){
        run_i_ind = astart_ind[i]:nrow(alevel_out)
      } else {

        run_i_ind = astart_ind[i]:(astart_ind[i+1]-1)
      }
      aout_j = alevel_out[run_i_ind, ]
      for(j in sum_col_names) {
        alevel_out[run_i_ind, j] = aout_j[, j] + run1_totals[[j]]
      }

    }
  }
  return(alevel_out)

}

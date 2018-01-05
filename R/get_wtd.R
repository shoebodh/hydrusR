#' Extracts water table depth at each time step
#'
#' @param tlevel.out
#' @param soil.depth
#'
#' @return
#' @export
#'
#' @examples
get.water.table<- function(tlevel.out, soil.depth) {
      Tlevel<- read.table(file = tlevel.out,
                          header = T, sep = "", dec = ".",
                          na.strings = "NA", colClasses = NA, as.is = TRUE, skip  = 6,
                          check.names = TRUE, fill = T,
                          strip.white = FALSE, blank.lines.skip = TRUE,
                          comment.char = "#",
                          allowEscapes = FALSE, flush = FALSE,
                          stringsAsFactors = default.stringsAsFactors(),
                          fileEncoding = "", encoding = "unknown")

      options(warn = -1)
      Tlevel.dat<- data.frame(Time = as.numeric(Tlevel$Time),
                              hTop = as.numeric(Tlevel$hTop),
                              hBot = as.numeric(Tlevel$hBot),
                              evap = as.numeric(Tlevel$vTop),
                              transp = as.numeric(Tlevel$vRoot),
                              stor = as.numeric(Tlevel$Volume))
      options(warn = 0)
      rm(Tlevel)

      Tdat = na.omit(Tlevel.dat)
      wtdepth = soil.depth - Tdat$hBot

      wtdata = data.frame(Time = Tdat$Time, wtdepth = wtdepth)
      return(wtdata)
}

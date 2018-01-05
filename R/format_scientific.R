#' Format number to scientific format according to HYDRUS inputs
#'
#' @param x
#' @param ndec
#' @param power.digits
#' @param ...
#'
#' @return
#' @export
#'
format.sci<- function(x, ndec, power.digits, ...) {

      dformat_sci = format(x, scientific = T)
      dnum = unlist(strsplit(dformat_sci, "e"))
      dnum_psign = substr(dnum[2], start = 1, stop = 1)
      dnuml = dnum[1]
      dnums = gsub(pattern = "\\+|\\-", replacement = "", x = dnum[2])
      dnuml = format(as.numeric(dnuml), nsmall = ndec)
      dnums = sprintf(fmt = paste0("e", dnum_psign, "%0",power.digits, "d"), as.numeric(dnums))
      dformat_new = paste0(dnuml, dnums)
      return(dformat_new)
}

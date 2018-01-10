#' Format number to scientific format according to HYDRUS inputs
#'
#' @param x  A numeric vector to be formatted
#' @param ndec Number of decimal places to apply (e.g, .000xx)
#' @param power.digits Number of power digits to apply (e.g., e+002)
#' @param ...
#'
#' @return
#' @export
#'
#'
format2sci<- function (x, ndec, power.digits, ...) {

      format.scalar<- function(x, ndec, power.digits) {

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

if(length(x) > 1000){
  ncores = (parallel::detectCores()) -1
  cl = parallel::makeCluster(ncores)
  clusterExport(cl, varlist = c("format.scalar", "ndec", "power.digits"), envir = environment())
  fmt_vec_out = parallel::parSapply(cl, X = x, FUN = format.scalar, ndec = ndec, power.digits = power.digits, simplify = TRUE)
 stopCluster(cl)
} else {
      fmt_vec_out = sapply(X = x, FUN = format.scalar, ndec, power.digits)
 }

return(fmt_vec_out)
}

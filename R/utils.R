#' Copy contents of a folder to another
#'
#' @param src.dir Source directory
#' @param dest.dir Destination directory
#' @param overwrite Logical, if true overwrites the existing directory, default = T
#' @param ask Logical, should ask whether to create/delete directories if required, default = FALSE
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dir.copy<- function(src.dir, dest.dir, overwrite = TRUE, ask = FALSE, ...){

      if(!file.exists(src.dir)){

            stop("Specified source directory is not valied...\n")

      }

      if(!file.exists(dest.dir) && ask == FALSE){
            stop("the destination folder specified is either invalid or doesn't exist...\n")
            return(FALSE)

      } else if(!file.exists(dest.dir) && ask == TRUE){
            cat("the destination folder specified is either invalid or doesn't exist...\n")

            askUser <- paste("would you like to create this directory?, y/n/c")

            userAnswer <- readline(askUser)
            userInput <- substr(toupper(userAnswer), start = 1, stop = 1)

            if(userInput == "N"){
                  return(FALSE)

            } else if(userInput == "Y"){

                  dir.create(dest.dir)
            }

      }

      all_files = dir(src.dir, full.names = TRUE)
      file.copy(from = all_files, to = dest.dir, recursive = TRUE)
}

#############
#' @description Get the number of decimal digits in a numeric value
#' @title get.decimalplaces
#' @param x A double numerical
#' @export
#' @return
#' @example
#' get.decimalplaces(10.343434)

get.decimalplaces <- function(x) {
        if ((x %% 1) != 0) {
                nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
        } else {
                return(0)
        }
}
############
#' Format number to scientific format according to HYDRUS inputs
#'
#' @param x  A numeric vector to be formatted
#' @param ndec Number of decimal places to apply (e.g, .000xx)
#' @param power.digits Number of power digits to apply (e.g., e+002)
#' @param ...
#' @return
#' @export
#' @example
#' format2sci(120.5, ndec = 5, power.digits = 2)

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
                parallel::clusterExport(cl, varlist = c("format.scalar", "ndec", "power.digits"), envir = environment())
                fmt_vec_out = parallel::parSapply(cl, X = x, FUN = format.scalar, ndec = ndec, power.digits = power.digits, simplify = TRUE)
                parallel::stopCluster(cl)
        } else {
                fmt_vec_out = sapply(X = x, FUN = format.scalar, ndec, power.digits)
        }

        return(fmt_vec_out)
}





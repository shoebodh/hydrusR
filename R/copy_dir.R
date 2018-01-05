#' Copy contents of a folder to another
#'
#' @param src.dir
#' @param dest.dir
#' @param overwrite
#' @param ask
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
        userInput <- substr(userAnswer, start = 1, stop = 1)

        if(userInput == "n"|| userInput == "N"){
            return(FALSE)

        } else if(userInput == "y" || userInput == "Y"){

            dir.create(dest.dir)
        }

    }

    all_files = dir(src.dir, full.names = TRUE)
    file.copy(from = all_files, to = dest.dir, recursive = TRUE)
}





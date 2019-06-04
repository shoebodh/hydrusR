#' Executes H1D_CALC.EXE from  Hydrus 1D directory
#'
#' @param project.path Path of the hydrus project
#' @param hydrus.path  Path of the Hydrus 1D executable
#' @param show.output Logical (Default = TRUE) whether the shell
#'                    output should be visible on R console (relevant only on windows)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

call.H1D<- function(project.path, hydrus.path, show.output = TRUE, ...){
      os.type = .Platform$OS.type

      hydrus.exe = "H1D_CALC.EXE"  #### Windows sepcific executable name

      oldwd = getwd()
      level_01 = file.path(hydrus.path, "LEVEL_01.DIR")
      Sys.chmod(level_01, "666")

      write(x = noquote(project.path), file = level_01, append = F)

      setwd(hydrus.path)

      if(os.type == "unix") {
            system(paste0("./", hydrus.exe))
      } else {
            system(hydrus.exe, show.output.on.console = show.output,
                   minimized = TRUE, invisible = TRUE)
      }

      setwd(oldwd)

}

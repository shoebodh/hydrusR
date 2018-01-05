#' Executes H1D_CALC.EXE from  Hydrus 1D directory
#'
#' @param project.path
#' @param hydrus.path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
call.H1D<- function(project.path, hydrus.path, output.on.console = TRUE, ...){
      os.type = .Platform$OS.type

      hydrus.exe = "H1D_CALC.exe"  #### Windows sepcific executable name

      oldwd = getwd()
      level_01 = file.path(hydrus.path, "LEVEL_01.DIR")
      write(x = noquote(project.path), file = level_01, append = F)


      setwd(hydrus.path)

      system(hydrus.exe, show.output.on.console = output.on.console,
             minimized = TRUE, invisible = TRUE)

      setwd(oldwd)

}

#' Executes H1D_CALC.EXE from  Hydrus 1D directory
#'
#' @param project.path
#' @param hydrus.path
#' @param show.output Logical (Default = TRUE) whether the shell output should be  visible on R console
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
call.H1D<- function(project.path, hydrus.path, show.output = TRUE, ...){
      os.type = .Platform$OS.type

      hydrus.exe = "H1D_CALC.exe"  #### Windows sepcific executable name

      oldwd = getwd()
      level_01 = file.path(hydrus.path, "LEVEL_01.DIR")
      write(x = noquote(project.path), file = level_01, append = F)


      setwd(hydrus.path)

      system(hydrus.exe, show.output.on.console = show.output,
             minimized = TRUE, invisible = TRUE)

      setwd(oldwd)

}

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

call.H1D<- function(project.path, hydrus.path = NULL, show.output = TRUE, ...){

   os.type = .Platform$OS.type

   if(is.null(hydrus.path)) {

      if(os.type == "windows"){
         win_def_hdir = "C:/Program Files (x86)/PC-Progress"
         h1d_version_dir = list.files(win_def_hdir, full.names = T, pattern = "Hydrus-1D")
         h1d_versions = sapply(basename(h1d_version_dir),
                                        function(x) {
                                          name_split = unlist(strsplit(x, " |\\."))
                                          return(as.numeric(name_split[2]))
                                          })

          hydrus.path = h1d_version_dir[which.max(h1d_versions)]

         }

   }

      hydrus.exe = "H1D_CALC.EXE"  #### Windows sepcific executable name


      oldwd = getwd()
      file_level01 = file.path(hydrus.path, "LEVEL_01.DIR")
      if(!file.exists(file_level01)) file(file_level01, "w+")


      Sys.chmod(file_level01, "0777")

      write(x = noquote(project.path), file = file_level01, append = F)

      setwd(hydrus.path)

      if(os.type == "unix") {
            system(paste0("./", hydrus.exe))
      } else {
            system(hydrus.exe, show.output.on.console = show.output,
                   minimized = TRUE, invisible = TRUE)
      }

      setwd(oldwd)

}

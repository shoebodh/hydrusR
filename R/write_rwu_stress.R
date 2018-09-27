write.rwu.stress<- function(project.path, model = 0, para, ...){

      input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                             n = -1L, encoding = "unknown")

   rwu_info_ind = grep("BLOCK G", input_data)


}

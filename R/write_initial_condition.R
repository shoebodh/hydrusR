#' Write root distribution in profile.dat
#'
#' @param project.path Path of HYDRUS1D project
#' @param profile.depth Depth of soil profile
#' @param pr.vec A vector of pressure head values (length = # of total nodes in the profile)
#' @param wt.depth Depth of water table (to assign hydrostatic initial condition)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.ini.cond<- function(project.path, profile.depth, pr.vec = NULL, wt.depth, ...) {
      file.profile.dat = file.path(project.path, "PROFILE.DAT")
      profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")
      node_ind = grep(pattern =  ("^[0-9]"), profile_dat)

      header_split = unlist(strsplit(profile_dat[5], split = " "))
      header_split2 = header_split[header_split != ""]

      if(length(node_ind) == 0) {
            profile_data = profile_dat[6:length(profile_dat)]
      } else {
            profile_data = profile_dat[6:(node_ind - 1)]
      }

      profile_data_split = strsplit(profile_data, split = " ")
      profile_data_split2 = sapply(profile_data_split, FUN = function(x) x[x!= ""])
      profile_data_new = t(profile_data_split2)

      deltaz = abs(as.numeric(profile_data_new[3, 2]) - as.numeric(profile_data_new[2, 2]))

      if(!is.null(pr.vec)){
            ini_pr_vec = pr.vec
      } else {
            ini_pr_vec = seq(0, profile.depth, by = deltaz) - wt.depth

      }



      pr_vec_fmt = mapply(FUN = format.scientific, ini_pr_vec, ndec = 6, power.digits = 3)

      profile_data_new[1:length(pr_vec_fmt), 3] = pr_vec_fmt

      fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15, 15, 15)
      fmt_vec = paste("%", fmt_space, "s", sep = "")
      fmt_vec = fmt_vec[1:ncol(profile_data_new)]

      profile_data_fmt = profile_data_new
      for(n in 1:nrow(profile_data_new)){

            profile_data_fmt[n, ] = sprintf(fmt_vec, profile_data_new[n, ])
      }

      tspace = sprintf("%13s", "")
      profile_data_fmt2 = apply(profile_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
      profile_data_fmt2 = paste(profile_data_fmt2, tspace)

      profile_data_new = c(profile_dat[1:5], profile_data_fmt2)

      write(profile_data_new, file.profile.dat, append = FALSE)

}

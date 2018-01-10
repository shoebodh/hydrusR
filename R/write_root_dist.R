#' Write root distribution in profile.dat
#'
#' @param project.path
#' @param rdepth
#' @param rbeta
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.root.dist<- function(project.path, rdepth, rbeta = 0.962, ...) {
      file.profile.dat = file.path(project.path, "PROFILE.DAT")
        profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")

       # profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")
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
      rdepth_coord = seq(0, rdepth, by = deltaz)

      rdist = 1 - rbeta^rdepth_coord

      rdist = rev(rdist)
      rdist = c(1, rdist)
      rdist_new = numeric(nrow(profile_data_new))
      rdist_new[1:length(rdist)] = rdist

      root_dist_fmt = mapply(FUN = format.scientific, rdist_new, ndec = 6, power.digits = 3)

      profile_data_new[1:length(root_dist_fmt), 6] = root_dist_fmt

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

# write.root.dist<- function(file.profile.dat, rdepth, rbeta = 0.962, n.nodes, ...) {
#
# profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")
#
#       # node_ind = grep(pattern = paste0(n.nodes, " "), profile_dat)
#       # diff(node_ind)
#
#       skip = 5
#
#
#       last_line = profile_dat[length(profile_dat)]
#       last_line_split = unlist(strsplit(last_line, split = " "))
#       last_line_split = last_line_split[last_line_split!= ""]
#
#       header_split = unlist(strsplit(profile_dat[5], split = " "))
#       header_split2 = header_split[header_split != ""]
#
#      node_data = profile_dat[(skip +1):(skip + n.nodes)]
#
#       # if(length(last_line_split) == 0) {
#       #       node_data = profile_dat[skip:length(profile_dat)]
#       # } else {
#       #       node_data = profile_dat[6:(length(profile_dat) - 1)]
#       # }
#
#       node_data_split = strsplit(node_data, split = " ")
#       node_data_split2 = lapply(node_data_split, FUN = function(x) x[x!= ""])
#       node_data_new = do.call("rbind", node_data_split2)
#
#       deltaz = abs(as.numeric(node_data_new[3, 2]) - as.numeric(node_data_new[2, 2]))
#       rdepth_coord = seq(0, rdepth, by = deltaz)
#
#       rdist = 1 - rbeta^rdepth_coord
#
#       rdist = rev(rdist)
#       rdist = c(1, rdist)
#       rdist_new = numeric(nrow(node_data_new))
#       rdist_new[1:length(rdist)] = rdist
#
#       root_dist_fmt = format(rdist_new, scientific = TRUE)
#
#       node_data_new[1:length(root_dist_fmt), 6] = root_dist_fmt
#
#       fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15)
#       fmt_vec = paste("%", fmt_space, "s", sep = "")
#
#       node_data_fmt = node_data_new
#       for(n in 1:nrow(node_data_new)){
#
#             node_data_fmt[n, ] = sprintf(fmt_vec, node_data_new[n, ])
#       }
#
#       tspace = sprintf("%13s", "")
#       node_data_fmt2 = apply(node_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
#       node_data_fmt2 = paste(node_data_fmt2, tspace)
#       profile_data_new = c(profile_dat[1:5], node_data_fmt2)
#
#       write(profile_data_new, file.profile.dat, append = FALSE)
#
# }

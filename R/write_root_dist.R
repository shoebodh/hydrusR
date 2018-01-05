#' Write root distribution in profile.dat
#'
#' @param file.profile.dat
#' @param rdepth
#' @param rbeta
#' @param n.nodes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.root.dist<- function(file.profile.dat, rdepth, rbeta = 0.962, n.nodes, ...) {
      profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")

      # node_ind = grep(pattern = paste0(n.nodes, " "), profile_dat)
      # diff(node_ind)

      skip = 5


      last_line = profile_dat[length(profile_dat)]
      last_line_split = unlist(strsplit(last_line, split = " "))
      last_line_split = last_line_split[last_line_split!= ""]

      header_split = unlist(strsplit(profile_dat[5], split = " "))
      header_split2 = header_split[header_split != ""]

     node_data = profile_dat[(skip +1):(skip + n.nodes)]

      # if(length(last_line_split) == 0) {
      #       node_data = profile_dat[skip:length(profile_dat)]
      # } else {
      #       node_data = profile_dat[6:(length(profile_dat) - 1)]
      # }

      node_data_split = strsplit(node_data, split = " ")
      node_data_split2 = lapply(node_data_split, FUN = function(x) x[x!= ""])
      node_data_new = do.call("rbind", node_data_split2)

      deltaz = abs(as.numeric(node_data_new[3, 2]) - as.numeric(node_data_new[2, 2]))
      rdepth_coord = seq(0, rdepth, by = deltaz)

      rdist = 1 - rbeta^rdepth_coord

      rdist = rev(rdist)
      rdist = c(1, rdist)
      rdist_new = numeric(nrow(node_data_new))
      rdist_new[1:length(rdist)] = rdist

      root_dist_fmt = format(rdist_new, scientific = TRUE)

      node_data_new[1:length(root_dist_fmt), 6] = root_dist_fmt

      fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15)
      fmt_vec = paste("%", fmt_space, "s", sep = "")

      node_data_fmt = node_data_new
      for(n in 1:nrow(node_data_new)){

            node_data_fmt[n, ] = sprintf(fmt_vec, node_data_new[n, ])
      }

      tspace = sprintf("%13s", "")
      node_data_fmt2 = apply(node_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
      node_data_fmt2 = paste(node_data_fmt2, tspace)
      profile_data_new = c(profile_dat[1:5], node_data_fmt2)

      write(profile_data_new, file.profile.dat, append = FALSE)

}

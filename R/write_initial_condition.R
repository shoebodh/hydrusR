#' Write initial conditions in profile.dat
#'
#' @param file.profile.dat
#' @param pr.vec
#' @param wt
#' @param soil.depth
#' @param n.nodes
#' @param ...
#'
#' @return
#' @export
#' @examples
write.ini.cond<- function(file.profile.dat, pr.vec = NULL, wt, soil.depth, n.nodes, ...) {
  profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")
  skip = 5

 node_data = profile_dat[(skip+1):(n.nodes+skip)]

  if(is.null(pr.vec)){

    hTop = -wt
    hBot = soil.depth - wt
    pr.vec = seq(hTop, hBot, length.out = n.nodes)


  }
  pr.vec = format(pr.vec, scientific = TRUE)

  last_line = profile_dat[length(profile_dat)]
  last_line_split = unlist(strsplit(last_line, split = " "))
  last_line_split = last_line_split[last_line_split!= ""]

  header_split = unlist(strsplit(profile_dat[5], split = " "))
  header_split2 = header_split[header_split != ""]

  node_data_split = strsplit(node_data, split = " ")
  node_data_split2 = lapply(node_data_split, FUN = function(x) x[x!= ""])
  node_data_new = do.call('rbind', node_data_split2)
  # node_data_new = t(node_data_split2)

    deltaz = abs(as.numeric(node_data_new[3, 2]) - as.numeric(node_data_new[2, 2]))
  fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15)
  fmt_vec = paste("%", fmt_space, "s", sep = "")

  pr_col = 3
  pr_vec_fmt = sprintf(fmt_vec[pr_col], pr.vec)
  node_data_new[, 3] = pr_vec_fmt

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

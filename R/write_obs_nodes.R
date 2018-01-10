#' Write observation nodes in profile.dat
#'
#' @param project.path
#' @param Z
#' @param dz
#' @param obs.nodes
#'
#' @return
#' @export
#'
#' @examples
#'
write.obs.nodes<- function(project.path, Z, dz, obs.nodes) {
      profile_data =   readLines(con = file.path(project.path, "PROFILE.DAT"),
                                 n = -1L, encoding = "unknown", warn = F)

      nodes_fmt = format(fmt = "%5.0f", obs.nodes)
      nodes_new = paste(nodes_fmt, collapse = "  ")

      node_ind = grep(pattern =  ("^[0-9]"), profile_data)

      total_nodes = length(seq(0, Z, by = dz))

      last_node_ind = grep(pattern = as.character(total_nodes), profile_data)
      last_node_ind = last_node_ind[length(last_node_ind)]

      profile_data = c(profile_data[1:last_node_ind],
                       as.character(length(obs.nodes)), nodes_new)

      write(profile_data, file = file.path(project.path, "PROFILE.DAT"),
            append = F)

}

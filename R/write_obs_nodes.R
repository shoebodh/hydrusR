#' Write observation nodes in profile.dat
#'
#' @param project.path
#' @param obs.nodes
#'
#' @export
#'
#' @examples
#'
write.obs.nodes<- function(project.path, obs.nodes, ...) {
      profile_data1 =   readLines(con = file.path(project.path, "PROFILE.DAT"),
                                 n = -1L, encoding = "unknown", warn = F)

      profile_summary = profile_data1[1:5]

      node_num_ind = grep(pattern =  ("^[0-9]"), profile_data1)

      if(length(node_num_ind) == 0) {
            profile_data = profile_data1[6:length(profile_data1)]
      } else {
            profile_data = profile_data1[6:(node_num_ind - 1)]
      }

      profile_summary_line = unlist(strsplit(profile_data1[4], split = " "))
      profile_summary_line = profile_summary_line[profile_summary_line!= ""]

      end_row = profile_data[length(profile_data)]
      end_row_split = unlist(strsplit(end_row, split = " "))
      end_row_split = end_row_split[end_row_split != ""]

      profile_depth = abs(as.numeric(end_row_split[2]))

      if(max(obs.nodes) > profile_depth){
           cat ("omitting observation noded deeper than  profile  depth ...\n" )
      obs.nodes = obs.nodes[obs.nodes <= profile_depth]

      }

      num_of_nodes = length(obs.nodes)

      nodes_fmt = format(fmt = "%5.0f", obs.nodes)
      nodes_new = paste(nodes_fmt, collapse = "  ")

      # last_node_ind = grep(pattern = as.character(total_nodes), profile_data)
      # last_node_ind = last_node_ind[length(last_node_ind)]


      profile_data = c(profile_summary, profile_data, as.character(num_of_nodes), nodes_new)

      write(profile_data, file = file.path(project.path, "PROFILE.DAT"),
            append = F)

}

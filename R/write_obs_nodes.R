#' Write observation nodes in profile.dat
#'
#' @param project.path path to the hydrus project
#' @param obs.nodes Observation nodes
#'
#' @export
#'
#' @examples \dontrun{
#' write.obs.nodes(project.path, obs.nodes = c(5, 10, 20, 30)) ## Writes at these depths
#' write.obs.nodes(project.path) ##
#' }
#'
write.obs.nodes<- function(project.path, obs.nodes = NULL) {
      def_profile_data =   readLines(con = file.path(project.path, "PROFILE.DAT"),
                                 n = -1L, encoding = "unknown", warn = F)

      profile_summary = def_profile_data[1:4]

      pr_header = trimws(def_profile_data[4])
      num_nodes = as.numeric(unlist(strsplit(pr_header, " "))[1])
      profile_depth = num_nodes - 1

       profile_body = def_profile_data[5:(5 + num_nodes - 1)]

       if(length(obs.nodes) == 1 && obs.nodes == 0) obs.nodes = NULL

      if(missing(obs.nodes)|is.null(obs.nodes)) {
         num_obs_nodes = sprintf("%5.0f", 0)

      } else {
         num_obs_nodes = sprintf("%5.0f", length(obs.nodes))
         if(max(obs.nodes) > profile_depth){
            cat ("omitting observation noded deeper than  profile  depth ...\n" )
            obs.nodes = obs.nodes[obs.nodes <= profile_depth]

         }
      }

      nodes_fmt = sprintf(fmt = "%5.0f", obs.nodes)
      nodes_new = paste(nodes_fmt, collapse = "")

      profile_data = c(profile_summary, profile_body, num_obs_nodes, nodes_new)

      write(profile_data, file = file.path(project.path, "PROFILE.DAT"),
            append = F)

}

#' Read observation point outputs from
#'
#' @param project.path
#' @param out.file
#' @param obs.output
#' @param obs.nodes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read.obs_node<- function(project.path, out.file = "Obs_Node.out", obs.output = c("h", "theta"), obs.nodes, ...) {

      # obs_node_out = read.table(file.path(simulation.path, "Obs_Node.out"),
      #                           header = F, sep = "", dec = ".",
      #                           na.strings = "NA", colClasses = NA, as.is = TRUE,
      #                           skip = 10, check.names = FALSE, fill = T,
      #                           strip.white = FALSE, blank.lines.skip = TRUE,
      #                           comment.char = "#",
      #                           allowEscapes = FALSE, flush = FALSE,
      #                           stringsAsFactors = default.stringsAsFactors())
      # #
      #   output_names = obs_node_out[1, ]
      #   obs_node_out = obs_node_out[-c(1, nrow(obs_node_out)), ]

      obs_node_out = data.table::fread(input = file.path(project.path, out.file),
                                       fill = TRUE, blank.lines.skip = FALSE)

      output_names = unlist(unclass(obs_node_out[10]))
      output_names = unique(output_names[!is.na(output_names)])
      output_names = output_names[2:length(output_names)]

      obs_node_out = obs_node_out[-c(1:10, nrow(obs_node_out)), ]
      obs_node_out = obs_node_out[, colnames(obs_node_out) := lapply(.SD, as.numeric), .SDcols = colnames(obs_node_out)]

      output_names_rep = rep(output_names, times = length(obs.nodes))
      obs_nodes_rep = rep(obs.nodes, each = length(output_names))
      output_names_all = paste(output_names_rep, obs_nodes_rep, sep = "_")
      colnames(obs_node_out) = c("Time", output_names_all)
      obs_node_out = data.frame(obs_node_out, row.names = NULL, check.names = F)

      if(is.null(obs.output) | missing(obs.output)) {
            obs_node_out = obs_node_out

      } else {
          output_cols = grepl(obs.output, names(obs_node_out))
            # output_ind = grepl(pattern = paste(c("Time", obs.output), collapse = "|"), x = names(obs_node_out))
            cols = c("Time", names(obs_node_out)[output_cols])
            obs_node_out =  obs_node_out[, cols]

      }

      t1 = obs_node_out[1, ]
      t1$Time = 0
      tstep = diff(obs_node_out$Time)
      tstep = tstep[length(tstep)]
      remainder = obs_node_out$Time%%tstep
      rem_ind = which(remainder == 0)

      obs_node_out = rbind(t1, obs_node_out[rem_ind, ])
      row.names(obs_node_out) = NULL

      return(obs_node_out)

}

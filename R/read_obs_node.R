#' @Description Reads observation point outputs of h, theta and flux, that is produced in the "Obs_Node.out" file
#' @title Read observation point outputs
#' @param project.path Directory of the model input/outputs
#' @param out.file   The output file name. Default is "Obs_Node.out", produced by the Hydrus 1D program
#' @param obs.output The output to be returned.Default = NULL, i.e., all the outputs are read
#'                   Other options are "h", "theta", "Flux". example, obs.output = c("h", "theta")
#' @param obs.nodes Numeric Vector of the observation nodes defined in the PROFILE.DAT
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read.obs_node<- function(project.path, out.file = "Obs_Node.out", obs.output = NULL,
            obs.nodes, ...) {

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
options(warn = -1)
      obs_node_out = data.table::fread(input = file.path(project.path, out.file),
                                       fill = TRUE, blank.lines.skip = FALSE)

      node_ind = grep("Node", data.frame(obs_node_out)[, 1])
      output_names = unlist(unclass(obs_node_out[node_ind + 2]))
      output_names = unique(output_names[!is.na(output_names)])
      output_names = output_names[output_names != ""]
      output_names = output_names[2:length(output_names)]

      obs_node_out = obs_node_out[-c(1:(node_ind + 2), nrow(obs_node_out)), ]
      obs_node_out = obs_node_out[, colnames(obs_node_out) := lapply(.SD, as.numeric), .SDcols = colnames(obs_node_out)]

    all_na_ind =   sapply(X = obs_node_out, function(x) !all(is.na(x)))
     obs_node_out = obs_node_out[, (all_na_ind), with = F]

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
            in_cols = c("Time", names(obs_node_out)[output_cols])
            obs_node_out =  obs_node_out[, in_cols]

      }

      t1 = obs_node_out[1, ]
      t1$Time = 0
      tstep = diff(obs_node_out$Time)
      tstep = tstep[length(tstep)]
      remainder = obs_node_out$Time%%tstep
      rem_ind = which(remainder == 0)

      obs_node_out = rbind(t1, obs_node_out[rem_ind, ])
      row.names(obs_node_out) = NULL

      options(warn = 0)

      return(obs_node_out)

}

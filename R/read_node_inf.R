#' READ outputs of NOD_INF.OUT
#'
#' @param project.path
#' @param out.file
#' @param output
#' @param warn
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read.nod_inf<- function(project.path, out.file = "Nod_Inf.out", output = NULL, warn = FALSE, ...){
      if(is.null(output) | missing(output)) {
            output = c("Head", "Moisture", "K", "C", "Flux",
                       "Sink", "Kappa", "v/KsTop", "Temp")
      }

      nod_inf = data.table::fread(input = file.path(project.path, out.file),
                                  fill = TRUE, blank.lines.skip = FALSE)

      colnames(nod_inf) = as.character(nod_inf[10, ])

      time_lines = nod_inf[grepl("Time:", nod_inf[, Node]), ]
      times = as.numeric(time_lines[, Depth])

      nod_inf = nod_inf[-(1:12), ]
      for (col in colnames(nod_inf)) set(nod_inf, j=col, value= as.numeric(nod_inf[[col]]))

      # nod_inf[, colnames(nod_inf) := lapply(.SD, as.numeric), .SDcols = colnames(nod_inf)]

      nod_inf = na.omit(nod_inf)

      options(warn = -1)
      if(warn == TRUE) options(warn = 0)

      nodes = unique(nod_inf[,Node])

      nod_inf[, Time:= rep(times, each = length(nodes))]

      output_names = intersect(output, colnames(nod_inf))
      output_names = c("Time", "Node", "Depth", output_names)
      not_needed = colnames(nod_inf)[!(colnames(nod_inf) %in% output_names)]

      nod_out = nod_inf[, .SD, .SDcols = output_names]

      # node_out<- data.frame(Node = nod_inf$Node,
      #                       Depth = as.numeric(nod_inf$Depth),
      #                       nod_inf[, output_names], check.names = FALSE)
      # names(node_out) = c("Node", "Depth", output_names)

      # node_out = as.data.frame(na.omit(node_out), row.names = NULL, check.names = FALSE)
      # nodes = unique(node_out$Node)
      # node_out = data.frame(Time = rep(times, each = length(nodes)), node_out,
      #                       row.names = NULL, check.names = FALSE)

      return(nod_out)

}

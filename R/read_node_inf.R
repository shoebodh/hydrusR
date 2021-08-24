#' READ outputs of NOD_INF.OUT
#'
#' @param project.path Path to the H1D project
#' @param out.file ## Name of the Nod_Inf.out file, in case saved to different name
#' @param output Vector of output types to be read (e.g., "Head", "Moisture", "Flux")
#' Default is NULL, meaning all the outputs is read.
#' @param warn Should the warning of coercion of character to NA be shown
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
read.nod_inf<- function(project.path, out.file = "Nod_Inf.out",
                        output = NULL, warn = FALSE, asDT = TRUE){

    if(is.null(output) | missing(output)) {
            output = c("Head", "Moisture", "K", "C", "Flux",
                       "Sink", "Kappa", "v/KsTop", "Temp")
      }

      options(warn = -1)
      if(warn == TRUE) options(warn = 0)

      nod_inf = data.table::fread(input = file.path(project.path, out.file),
                                  fill = TRUE, blank.lines.skip = FALSE, skip = 10)

      # colnames(nod_inf) = as.character(nod_inf[10, ])

      time_lines = nod_inf[grepl("Time:", nod_inf[["Node"]]), ]

       times = c(0, as.numeric(time_lines$Depth))

      for (col in colnames(nod_inf)) set(nod_inf, j=col, value= as.numeric(nod_inf[[col]]))

      # nod_inf[, colnames(nod_inf) := lapply(.SD, as.numeric), .SDcols = colnames(nod_inf)]

      nod_inf = na.omit(nod_inf)

      nodes = sort(unique(nod_inf[["Node"]]))

      nod_inf[, Time:= rep(times, each = length(nodes))]

      nod_split = split(nod_inf, f = nod_inf$Time)

      nrow_split = sapply(nod_split, nrow)

      extra_index = which(nrow_split > length(nodes))

      for(i in extra_index){
            nod_split[[i]] = nod_split[[i]][1:length(nodes), ]
      }


      nod_inf =  rbindlist(nod_split)

      output_names = intersect(output, colnames(nod_inf))
      output_names = c("Time", "Node", "Depth", output_names)
      # dropped_cols = colnames(nod_inf)[!(colnames(nod_inf) %in% output_names)]

      nod_out = nod_inf[, .SD, .SDcols = output_names]

      # node_out<- data.frame(Node = nod_inf$Node,
      #                       Depth = as.numeric(nod_inf$Depth),
      #                       nod_inf[, output_names], check.names = FALSE)
      # names(node_out) = c("Node", "Depth", output_names)

      # node_out = as.data.frame(na.omit(node_out), row.names = NULL, check.names = FALSE)
      # nodes = unique(node_out$Node)
      # node_out = data.frame(Time = rep(times, each = length(nodes)), node_out,
      #                       row.names = NULL, check.names = FALSE)
    options(warn = 0)

    if(!asDT) nod_out = data.frame(nod_out)

     return(nod_out)

}

#' Join the output files from several H1D simulations (for longer simulation times than allowed by H1D (1000))
#'
#' @param project.path path of hydrus project
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

join.output.files<- function(project.path, ...) {

      output_files = c("A_Level.out", "T_Level.out", "Nod_Inf.out", "Obs_Node.out")

      mapply(file.remove, file.path(project.path, output_files))

      sim_dirs = dir(project.path, pattern = "sim", full.names = TRUE)
      sim_ids = gsub("sim", "", basename(sim_dirs))
      sim_ids = sort(as.numeric(sim_ids))

      sim_ids_ordered = paste0("sim", sim_ids)
      sim_dirs_ordered = file.path(project.path, sim_ids_ordered)
      #########
      Alevel_files = sapply(sim_dirs_ordered, FUN = list.files, pattern = "A_Level.out",
                            full.names = T)
      Alevel_data = lapply(Alevel_files, readLines, n = -1L, encoding = "unknown")
      names(Alevel_data) = basename(sim_dirs_ordered)

      Alevel_data[[1]] = Alevel_data[[1]][-length(Alevel_data[[1]])]

      Alevel_data[-1] = lapply(Alevel_data[-1], FUN = function(x) x[-c(1:5, length(x))])
      Alevel_data_all = do.call("c", Alevel_data)
      Alevel_data_all = c(Alevel_data_all, "end")

      names(Alevel_data_all) = NULL


      Tlevel_files = sapply(sim_dirs_ordered, FUN = list.files, pattern = "T_Level.out",
                            full.names = TRUE)

      Tlevel_data = lapply(Tlevel_files, readLines, n = -1L, encoding = "unknown")
      Tlevel_data[[1]] = Tlevel_data[[1]][-length(Tlevel_data[[1]])]
      Tlevel_data[-1] = lapply(Tlevel_data[-1], FUN = function(x) x[-c(1:9, length(x))])

      Tlevel_data_all = do.call("c", Tlevel_data)
      Tlevel_data_all = c(Tlevel_data_all, "end")
      names(Tlevel_data_all) = NULL



      nod_inf_files = sapply(sim_dirs_ordered, FUN = list.files,
                             pattern = "Nod_Inf.out", full.names = T)

      obs_node_files = sapply(sim_dirs_ordered, FUN = list.files,
                              pattern = "Obs_Node.out", full.names = T)

      nod_inf_data = lapply(nod_inf_files, readLines, n = -1L, encoding = "unknown")

      nod_inf_data[-1] = lapply(nod_inf_data[-1], FUN = function(x) x[-c(1:5)])

      nod_inf_data_all = do.call("c", nod_inf_data)

      obs_node_data = lapply(obs_node_files, readLines, n = -1L, encoding = "unknown")
      obs_node_data[[1]] = obs_node_data[[1]][-length(obs_node_data[[1]])]
      obs_node_data[-1] = lapply(obs_node_data[-1], FUN = function(x) x[-c(1:11, length(x))])

      obs_node_data_all = do.call("c", obs_node_data)
      obs_node_data_all = c(obs_node_data_all, "end")

      write(Alevel_data_all, file = file.path(project.path, "A_Level.out"),
            append = F)
      cat("A_Level.out...\n")

       write(Tlevel_data_all, file = file.path(project.path, "T_Level.out"), append = F)
        cat("T_Level.out...\n")

      write(obs_node_data_all, file = file.path(project.path, "Obs_Node.out"))

       cat("Nod_Inf.out...\n")
      write(nod_inf_data_all, file = file.path(project.path, "Nod_Inf.out"),
            append = F)
      cat("Obs_Node.out...\n")

}

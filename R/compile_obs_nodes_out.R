#' Compile outputs at observation nodes, from all simulations
#'
#' @param outputs.path
#' @param num.of.sims
#' @param output
#' @param obs.nodes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compile.obs_nodes.out<- function(outputs.path, num.of.sims,
       output = c("h", "theta"), obs.nodes, ...) {

      obs_out_list = vector("list", length = num.of.sims)

      for(a in 1: num.of.sims){
            cat("reading Obs_Node.out for simulation #", a, "...")
            sim_path  = file.path(outputs.path, paste0("sim", a))
            obs_node_out = read.obs_node(simulation.path = sim_path,
                          obs.output = output, obs.nodes = obs.nodes)
            if(a > 1) {
                  last_out = obs_out_list[[a-1]]
                  obs_node_out$Time = obs_node_out$Time + last_out$Time[nrow(last_out)]

            }

            obs_out_list[[a]] = obs_node_out
            cat("done\n")

      }

       obs_node_data = data.frame(do.call("rbind", obs_out_list), row.names = NULL,
           check.names = FALSE)

         return(obs_node_data)

}


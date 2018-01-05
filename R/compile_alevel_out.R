#' Compile ALEVEL.OUT data from all simulations
#'
#' @param outputs.path
#' @param output
#' @param num.of.sims
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compile.A_level.out<- function(outputs.path, output = NULL, num.of.sims = 1, ...) {

         alevel_out_list = vector("list", length = num.of.sims)

        for(a in 1: num.of.sims){
            cat("reading Node_inf.out for simulation ", a, "...")
            sim_path  = file.path(outputs.path, paste("sim", a, sep = ""))
            alevel_data = read.A_level.out(simulation.path = sim_path, output)

            cat("done\n")

            if(a > 1) {
                 last_alevel = alevel_out_list[[a-1]]
                  alevel_data$Time = last_alevel$Time[nrow(last_alevel)] +
                        alevel_data$Time
                  rm(last_alevel)
            }

            alevel_out_list[[a]] = alevel_data

        }

      alevel_out_compiled = data.frame(do.call("rbind", alevel_out_list),
            row.names = NULL, check.names = FALSE)

          return(alevel_out_compiled)

}

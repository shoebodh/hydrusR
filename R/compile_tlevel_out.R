#' Compiles output of TLEVEL.OUT from all simulations
#' @param outputs.path
#' @param output
#' @param num.of.sims
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compile.T_level.out<- function(outputs.path, output = NULL, num.of.sims = 1, ...)
   {
        tlevel_out_list = vector("list", length = num.of.sims)

        for(a in 1: num.of.sims){
            cat("reading Tlevel.out for simulation ", a, "...")
            sim_path  = file.path(outputs.path, paste("sim", a, sep = ""))
            tlevel_data = read.T_level.out(simulation.path = sim_path, output = output)

            cat("done\n")

            if(a > 1) {
                 last_tlevel = tlevel_out_list[[a-1]]
                  tlevel_data$Time = last_tlevel$Time[nrow(last_tlevel)] +
                        tlevel_data$Time
                  rm(last_tlevel)
            }

            tlevel_out_list[[a]] = tlevel_data

        }

      tlevel_out_compiled = data.frame(do.call("rbind", tlevel_out_list),
            row.names = NULL, check.names = FALSE)

    return(tlevel_out_compiled)

}

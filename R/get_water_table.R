#' Calculate WT depth from soil moisture profile and parameters
#'
#' @param simulation.path
#' @param sim.num
#'
#' @return
#' @export
#'
#' @examples
get_water_table<- function(simulation.path, sim.num) {
      tlevel_data = vector("list", length = sim.num)
      for(a in 1: sim.num) {
            sim_path  = file.path(simulation.path, paste("sim", a, sep = ""))

            Tlevel<- read.table(file = file.path(sim_path, "T_LEVEL.OUT"),
                                header = T, sep = "", dec = ".",
                                na.strings = "NA", colClasses = NA, as.is = TRUE, skip  = 6,
                                check.names = TRUE, fill = T,
                                strip.white = FALSE, blank.lines.skip = TRUE,
                                comment.char = "#",
                                allowEscapes = FALSE, flush = FALSE,
                                stringsAsFactors = default.stringsAsFactors(),
                                fileEncoding = "", encoding = "unknown")

            node_out = read.nod_inf(simulation.path = sim_path, output = c("Moisture", "Head"), warn = F)
            sp_time = unique(node_out$Time)
            all_nodes = unique(node_out$Node)
            dep = rep(sp_time, each = length(all_nodes))
            node_out_split = split(node_out, dep)

            wt_pos = function(nodeout_data) {
                  pos = nodeout_data[which(nodeout_data$Head >= 0), ]
                  neg = nodeout_data[which(nodeout_data$Head <= 0), ]
                  pos_neg = rbind(neg[nrow(neg), ], pos[1, ])

                  if(identical(pos[1, ], neg[nrow(neg), ])) {
                        wt_depth = pos$Depth[1]
                  } else {
                        wt_depth = (abs(pos_neg$Depth[1]*pos_neg$Head[2]) +
                                          abs(pos_neg$Depth[2]*pos_neg$Head[1]))/(sum(abs(pos_neg$Head)))

                  }


            }

            wtdepth = sapply(node_out_split, FUN = wt_pos)


            options(warn = -1)
            Tlevel.dat<- data.frame(Time = as.numeric(Tlevel$Time),
                                    hTop = as.numeric(Tlevel$hTop),
                                    hBot = as.numeric(Tlevel$hBot),
                                    evap = as.numeric(Tlevel$vTop),
                                    transp = as.numeric(Tlevel$vRoot),
                                    stor = as.numeric(Tlevel$Volume))
            options(warn = 0)
            rm(Tlevel)

            Tdat = na.omit(Tlevel.dat)

            Tdat$wt =  wtdepth[2:length(wtdepth)]
            Tdat$evap = ifelse(Tdat$evap < 0, 0, Tdat$evap)
            Tdat$transp = ifelse(Tdat$transp < 0, 0, Tdat$transp)
            Tdat$et = Tdat$evap + Tdat$transp

            tlevel_data[[a]] = Tdat

            cat("simulation", a, "...\n")

      }

      tdata_all = do.call("rbind", tlevel_data)
      tstep = tdata_all[2, "Time"] - tdata_all[1, "Time"]

      tdata_all = data.frame(tdata_all, row.names = NULL)

      tdata_all$Time = (1:nrow(tdata_all))*tstep

      tdata_all$et = tdata_all$et*tstep

      return(tdata_all)

}

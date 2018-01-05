#' This is the main simulation function
#'
#' @param project.path
#' @param hydrus.path
#' @param profile.depth
#' @param beginT
#' @param endT  end time
#' @param deltaT time step
#' @param bot.bc.type head or flux type
#' @param bot.bc.value  value of the bc
#' @param const.bot.bc Logical, to set if bottom BC is constant
#' @param soil.para Hydraulic parameters of soil (van Genuchten)
#' @param atm.bc.data data frame containing atmonspheric boundary conditions (time variable BC)
#' @param ini.wt Initial water table depth
#' @param rdepth rooting depth
#' @param obs.nodes Observation node points (vector)
#' @param output.on.console Logical, whether the shell output of HYDRUS1D run should be displayed on R console, default = F
#'
#' #' @return
#' @export
#'
#' @examples

run.H1D.simulation = function(project.path, hydrus.path = NULL, profile.depth,
                              beginT, endT, deltaT, bot.bc.type, bot.bc.value, const.bot.bc,
                              soil.para, atm.bc.data, ini.wt,
                              rdepth, obs.nodes, output.on.console = TRUE, ...) {

      if(is.null(hydrus.path)|missing(hydrus.path)){
            hydrus.path = "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"
      }

      # write(initial_profile, file = file.path(project.path, "INI_PROFILE.DAT"), append = F)
      maxTp = endT/deltaT
      times_s = seq(beginT, endT, by = deltaT)

      prev_sims = dir(project.path, pattern = "sim", full.names = T)

      if(length(prev_sims > 0)){
            mapply(FUN = unlink, prev_sims, recursive = T, force = T)
      }

      if(maxTp <= 960) {

            write.ini.cond(project.path, profile.depth = profile.depth, wt.depth = ini.wt)

            write.root.dist(project.path,  rdepth = rdepth, rbeta = 0.962)

            write.obs.nodes(project.path, Z = profile.depth, dz = deltaz,
                            obs.nodes = obs.nodes)

            write.hydraulic.para(project.path, para = soil.para)

            write.bottom.bc(constant.bc = const.bot.bc, bc.type = bot.bc.type,
                            bc.value = bot.bc.value, project.path = project.path)

            write.atmosph.in(project.path, maxAL = maxTp, deltaT = deltaT,
                             atm.bc.data = atm.bc.data[1:maxTp, ])

            write.print.times(project.path, tmin = beginT, tmax = endT, tstep = deltaT)

            call.H1D(project.path, hydrus.path = hydrus.path, output.on.console = output.on.console)

      } else {

            cat("Running times", 1, "to", 960*deltaT, "...\n")

            write.ini.cond(project.path, profile.depth = profile.depth, wt.depth = ini.wt)

            write.root.dist(project.path,  rdepth = rdepth, rbeta = 0.962)

            write.obs.nodes(project.path, Z = profile.depth, dz = deltaz, obs.nodes = obs.nodes)

            write.hydraulic.para(project.path, para = soil_para)

            write.bottom.bc(constant.bc = const.bot.bc, bc.type = bot.bc.type,
                            bc.value = bot.bc.value, project.path = project.path)

            write.atmosph.in(project.path, maxAL = 960, deltaT = deltaT,
                             atm.bc.data = atm_bc_data[1:960, ])

            write.print.times(project.path, tmin = beginT, tmax = 960*deltaT, tstep = deltaT)

            call.H1D(project.path, hydrus.path = hydrus.path, output.on.console = output.on.console)

            cat("simulation from time", 1, "to", 960*deltaT, "success...\n")

            sim_number = ceiling(maxTp/960)

            sim1_files = list.files(project.path, full.names = TRUE)

            sim1_folder = file.path(project.path,"sim1")
            dir.create (sim1_folder)

            sapply(sim1_files, file.copy, to = sim1_folder)

            hyd.output<- read.table(file.path(project.path, "NOD_INF.OUT"), header = T, sep = "", dec = ".",
                                    na.strings = "NA", colClasses = NA, as.is = TRUE,
                                    skip = 10, check.names = TRUE, fill = T,
                                    strip.white = FALSE, blank.lines.skip = TRUE,
                                    comment.char = "#",
                                    allowEscapes = FALSE, flush = FALSE,
                                    stringsAsFactors = F,
                                    fileEncoding = "", encoding = "unknown")

            #################
            time_ind = grep("Time:", hyd.output$Node)
            to_skip = time_ind[length(time_ind)]+2

            head_profile = hyd.output[to_skip:nrow(hyd.output), c("Node", "Depth", "Head")]
            head_profile = head_profile[2:(nrow(head_profile) - 1), ]

            for(s in 2:sim_number) {

                  sim_index = s


                  beginTnew = ((sim_index-1)*960 + 1)

                  if(s < sim_number){
                        endTnew =  sim_index*960
                  } else {
                        endTnew = nrow(atm.bc.data)
                  }

                  sim_times_s = seq(beginTnew, endTnew)

                  # simulations_dir = file.path(rootPath, soil_type, output_folder)
                  # if(!dir.exists(simulations_dir)) dir.create(simulations_dir)

                  sim_folder = paste("sim", s, sep = "")

                  atm_bc_data_s = atm.bc.data[sim_times_s, ]
                  # atm_bc_data_s$tAtm = seq(1, length(sim_times_s))*deltaT

                  cat("Running times", ceiling(beginTnew*deltaT), "to",
                      endTnew*deltaT, "...\n")

                  write.ini.cond(project.path, profile.depth = profile.depth,
                                 pr.vec = as.numeric(head_profile$Head))

                  write.obs.nodes(project.path, Z = profile.depth, dz = deltaz,
                                  obs.nodes = obs.nodes)

                  # write.print.times(project.path, tmin = deltaT, atm_bc_data_s$tAtm[nrow(atm_bc_data_s)], tstep = 0.25)

                  write.print.times(project.path, tmin = atm_bc_data_s$tAtm[1],
                                    tmax = atm_bc_data_s$tAtm[nrow(atm_bc_data_s)], tstep = 0.25)

                  write.bottom.bc(constant.bc = const.bot.bc, bc.type = bot.bc.type,
                                  bc.value = bot.bc.value, project.path = project.path)

                  write.atmosph.in(project.path, maxAL = nrow(atm_bc_data_s), deltaT = deltaT,
                                   atm.bc.data = atm_bc_data_s)

                  call.H1D(project.path, hydrus.path = hydrus.path, output.on.console = output.on.console)

                  cat("simulation from time", ceiling(beginTnew*deltaT), "to",
                      endTnew*deltaT, "success...\n")


                  sim_out_dir = file.path(project.path, sim_folder)
                  if(!dir.exists(sim_out_dir)) dir.create(sim_out_dir)

                  sim_s_files = list.files(project.path, include.dirs = F, full.names = T)
                  sapply(sim_s_files, FUN = file.copy, to = sim_out_dir)

                  hyd.output<- read.table(file.path(project.path, "NOD_INF.OUT"), header = T, sep = "", dec = ".",
                                          na.strings = "NA", colClasses = NA, as.is = TRUE,
                                          skip = 10, check.names = TRUE, fill = T,
                                          strip.white = FALSE, blank.lines.skip = TRUE,
                                          comment.char = "#",
                                          allowEscapes = FALSE, flush = FALSE,
                                          stringsAsFactors = F,
                                          fileEncoding = "", encoding = "unknown")

                  #################
                  time_ind = grep("Time:", hyd.output$Node)
                  to_skip = time_ind[length(time_ind)]+2

                  head_profile = hyd.output[to_skip:nrow(hyd.output), c("Node", "Depth", "Head")]
                  head_profile = head_profile[2:(nrow(head_profile) - 1), ]
            }
      }

      cat("The simulation finished successfully...\n")

      cat("combining all calculations, ...\n")
      #####
      join.output.files(project.path)

      sim_dirs = dir(project.path, pattern = "sim", full.names = TRUE)
      mapply(FUN = unlink, sim_dirs, recursive = T, force = T)


      cat("simulation completed successfully")

}
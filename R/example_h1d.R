#' Runs a test hydrus project
#'
#' @param ...
#'
#' @export
#' @examples
#'
#' example.h1d(ntime = 100)

example.h1d<- function(profile.depth = 200, dz = 1, ntime = 1000, rdepth = 100,
                   soil.para = list(thr = 0.045, ths = 0.43,
                  Alfa = 0.145, n = 2.69,Ks = 29.7, l = 0.45),
                   hydrus.path =  "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx", ...){

      rootPath = path.expand("~") ## where to create the project
      project_name = "testproject"  ## Name of the project
      project_path = file.path(rootPath, project_name)

      library(hydrusR)
      library(data.table)
      library(dplyr)

      ## Basic inputs
      sunit = "cm" ## Space units
      tunit = "hours" ## time units
      pTimes = 1

      ##Profile inputs
      profile_depth = profile.depth
      deltaz = dz
      profile_nodes = seq(0, profile_depth, by = deltaz)

      initial_wtable = 30
      obs_nodes_all = seq(20, profile_depth, by = 20)
      nObsNodes = length(obs_nodes_all)

      ### Time inputs
      time_step = 1
      endTime = ntime ### total time steps
      total_timesteps = endTime/time_step
      ntimes = length(1:total_timesteps)

      ## LAI and pet
      input_pet = TRUE
      LAI = 4.0
      et_rate = 0.6

      ## Boundary conditions
      const_botbc = TRUE
      bot_bc_type = "flux"
      const_botFlux = 0.0000 ##### in cm/hr

      ### Atmospheric top boundary conditions
      atm_bc_data = data.frame(tAtm = seq(time_step, endTime, time_step),
                               Prec = numeric(ntimes),
                               rSoil = numeric(ntimes),
                               rRoot = numeric(ntimes),
                               hCritA = rep(10000, ntimes),
                               rB = numeric(ntimes),
                               hB = numeric(ntimes),
                               ht = numeric(ntimes),
                               RootDepth = numeric(ntimes))

      const_et = rep(et_rate, 365)
      hourly_et =  et.hourly(Et.Daily = const_et)

      hourly_et = hourly_et[rep(seq_len(nrow(hourly_et)), each = 1/time_step), ] ### for 0.25 time steps
      hourly_et = dplyr::mutate(hourly_et, et = et/(1/time_step))

      hourly_et$rSoil = hourly_et$et/2
      hourly_et$rRoot = hourly_et$et/2

      if(isTRUE(input_pet)) {
            atm_bc_data$rRoot = rep(LAI, nrow(atm_bc_data))
            atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]
      } else {
            atm_bc_data$rRoot = hourly_et$rRoot[1:nrow(atm_bc_data)]
            atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]

      }

      if(isTRUE(const_botbc)){
            atm_bc_data$hB = rep(0, nrow(atm_bc_data))
      }

      atm_bc_data = atm_bc_data[1:ntimes, ]

       #### Creates a blank hydrus project with three files

      create.H1D.project(project.name = project_name, parent.dir = rootPath,
                         TimeUnit = tunit, PrintTimes = pTimes,
                         processes = c("WaterFlow = T", "RootWaterUptake = T"),
                         geometry = c(ProfileDepth = profile_depth,
                                      NumberOfNodes = length(profile_nodes),
                                      ObservationNodes = nObsNodes))

      ### create the soil profile from created info
      create.soil.profile(project.path = project_path, out.file = "PROFILE.DAT",
                          profile.depth = profile_depth, dz = deltaz)

       ##### Default hydrus path in Windows


      run.H1D.simulation(project.path = project_path, hydrus.path = hydrus.path,
                         profile.depth = profile.depth,
                         beginT = 0, endT = endTime, deltaT = time_step,
                         bot.bc.type = bot_bc_type, bot.bc.value = const_botFlux,
                         const.bot.bc = TRUE, soil.para = soil.para,
                         atm.bc.data = atm_bc_data, ini.wt = initial_wtable, TimeUnit = tunit,
                         rdepth = rdepth,
                         obs.nodes = obs_nodes_all, show.output = T)

}

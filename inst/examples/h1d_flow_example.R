library(hydrusR)
library(data.table)
library(dplyr)

## Basic inputs
profile_depth = 200
ntimes = 3000
tstep = 1
deltaz = 1
rdepth = 100
time_step = 1
soil_para = list(thr = 0.045, ths = 0.43,
                 Alfa = 0.145, n = 2.69,Ks = 29.7, l = 0.45)

 # hydrus_path =  "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"
 hydrus_path = "/home/sacharya/.PlayOnLinux/wineprefix/Hydrus_1D/drive_c/Program Files/PC-Progress/Hydrus-1D 4.xx"


project_name = "h1dExample"
parent_dir = path.expand("~")
project_path = path.expand(file.path(parent_dir, project_name))

## Basic inputs
TimeUnit = "cm" ## Space units
SpaceUnit = "hours" ## time units
PrintTimes = 1
show_output = T

##Process inputs
rwu = T   ###  rootwater uptake

##Profile/geometry inputs
profile_nodes = seq(0, profile_depth, by = deltaz)
initial_wtable = 30
obs_nodes_all = seq(20, profile_depth, by = 20)
nObsNodes = length(obs_nodes_all)
rooting_depth = 120

### Time inputs
endTime = ntimes ### total time steps
total_timesteps = endTime/tstep
ntimesteps = length(1:total_timesteps)

## LAI and pet
input_pet = TRUE
LAI = 4.0
et_rate = 0.6

## Boundary conditions inputs
const_botbc = TRUE
bot_bc_type = "flux"
const_botFlux = 0.0000 ##### in cm/hr

### Atmospheric top boundary conditions
atm_bc_data = data.frame(tAtm = seq(time_step, endTime, tstep),
                         Prec = numeric(ntimesteps),
                         rSoil = numeric(ntimesteps),
                         rRoot = numeric(ntimesteps),
                         hCritA = rep(10000, ntimesteps),
                         rB = numeric(ntimesteps),
                         hB = numeric(ntimesteps),
                         ht = numeric(ntimesteps),
                         RootDepth = numeric(ntimesteps))

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

atm_bc_data = atm_bc_data[1:ntimesteps, ]

#### Creates a blank hydrus project with three files

create.H1D.project(project.name = project_name, parent.dir = parent_dir,
                   TimeUnit = TimeUnit, PrintTimes = PrintTimes,
                   processes = c(WaterFlow = T, RootWaterUptake = rwu),
                   geometry = c(ProfileDepth = profile_depth,
                                NumberOfNodes = length(profile_nodes),
                                ObservationNodes = nObsNodes))

### create the soil profile (PROFILE.DAT) info
create.soil.profile(project.path = project_path, out.file = "PROFILE.DAT",
                    profile.depth = profile_depth, dz = deltaz)

##Write root distribution
write.obs.nodes(project.path = project_path, obs.nodes = obs_nodes_all)

write.ini.cond(project.path = project_path, wt.depth = initial_wtable)

write.root.dist(project.path = project_path,  rdepth = rooting_depth, rbeta = 0.962)

write.hydraulic.para(project.path = project_path, para = soil_para)

write.bottom.bc(constant.bc = TRUE, bc.type = bot_bc_type,
                bc.value = const_botFlux, project.path = project_path)


 ##### Default hydrus path in Windows

run.H1D.simulation(project.path = project_path, hydrus.path = hydrus_path,
                   profile.depth = profile_depth,
                   beginT = 0, endT = endTime, deltaT = tstep,
                   bot.bc.type = bot_bc_type, bot.bc.value = const_botFlux,
                   const.bot.bc = TRUE,atm.bc.data = atm_bc_data, TimeUnit = TimeUnit,
                   show.output = show_output)

# run.H1D.simulation(project.path = project_path, hydrus.path = hydrus_path,
#                    profile.depth = profile_depth,
#                    beginT = 0, endT = endTime, deltaT = tstep,
#                    bot.bc.type = bot_bc_type, bot.bc.value = const_botFlux,
#                    const.bot.bc = TRUE, soil.para = soil_para,
#                    atm.bc.data = atm_bc_data, ini.wt = initial_wtable,
#                    TimeUnit = TimeUnit, rdepth = rdepth,
#                    obs.nodes = obs_nodes_all, show.output = T)

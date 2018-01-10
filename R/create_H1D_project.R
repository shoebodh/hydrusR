#' Creates a new hydrus project folder with "HYDRUS1D.DAT" and "DISCRIPT.TXT" files
#'
#'  @param project.name Name of the project
#' @param parent.dir  Path to the project folder
#' @param processes Main processes (e.g., WaterFlow, SoluteTransport etc) to be simualted
#' @param TimeUnit Simulation time unit information (default = days)
#' @param SpaceUnit Vertical spatial unit (decault = cm)
#' @param PrintTimes Time levels at which the outputs should be printed
#' @param geometry Profile geometry info (Depth, # of nodes and # of obs nodes)
#'
#' @export
#' @examples
#'
#' create.H1D.project(project.name = "testproj2", parent.dir = parent_dir, discription = NULL,
#' SpaceUnit = "cm", TimeUnit = "days", PrintTimes = 1,
#' processes = c(WaterFlow = T, SoluteTransport = F, RootWaterUptake = F,
#' RootGrowth = F, Unsatchem = F, HP1 = F, EquillibriumAdsorption = F,
#' initial.cond = c(NumberOfSolutes = 0, InitialCondition = 0),
#' geometry = c(ProfileDepth = 200, NumberOfNodes = 4,
#'          ObservationNodes = 0, MaterialNumber = 1, SubregionNumber = 1))


create.H1D.project<-function(project.name, parent.dir, discription = NULL,
                         TimeUnit = "days", SpaceUnit = "cm", PrintTimes = 1,
                             processes = c(WaterFlow = T, RootWaterUptake = T), geometry, initial.cond, ...) {


      # main_processes = c(WaterFlow = F, SoluteTransport = F, RootWaterUptake = F,
      #                      RootGrowth = F, Unsatchem = F, HP1 = F, EquillibriumAdsorption = F,
      #                      NumberOfSolutes = 0, InitialCondition = 0)
      #

      project_path = file.path(parent.dir, project.name)
      discript_file = file.path(project_path, "DISCRIPT.TXT")
      h1ddat_file = file.path(project_path, "HYDRUS1D.DAT")
      discription = ifelse(is.null(discription), paste("project title", project.name), discription)

      prompt_msg = paste("Folder", project.name, "already exists.All files will be deleted.Proceed? y/n \n")

      if(dir.exists(project_path)) {

            dir_answer = readline(prompt = prompt_msg)
            dir_answer = substr(toupper(dir_answer), start = 1, stop = 1)

            if(dir_answer == "Y") {
                  unlink(project_path, recursive = TRUE, force = TRUE)
                  dir.create(project_path)
            } else {
                  stop("HYDRUS1D project not created\n")

            }
      } else {
            dir.create(project_path)
      }

            args_vec = as.list(match.call())
            args_vec = lapply(args_vec[-1], FUN = function(x) unlist(x))
            # args_vec = unlist(unclass(args_vec))
            args_vec = do.call("c", args_vec)
            args_vec = ifelse(args_vec == TRUE, 1, args_vec)
            args_vec = ifelse(args_vec == FALSE, 0, args_vec)

            names(args_vec) = gsub("processes.", "", names(args_vec), fixed = TRUE)
            names(args_vec) = gsub("geometry.", "", names(args_vec), fixed = TRUE)
            names(args_vec) = gsub("initial.cond.", "", names(args_vec), fixed = TRUE)


            args_vec["ProfileDepth"] = toupper(format2sci(as.numeric(args_vec["ProfileDepth"]),
                                                      ndec = 2, power.digits = 3))

            args_names = names(args_vec)

            h1d_args_names = args_names[!(args_names %in% c("project.name", "parent.dir", "discription"))]
            # h1d_args_names = gsub("Profile.", "", h1d_args_names, fixed = TRUE)


            hydrus1d_template = system.file("templates/HYDRUS1D.DAT", package = "hydrusR")
            h1d_dat = readLines(hydrus1d_template, n = -1L, encoding = "unknown")

            discript_vec = c("Pcp_File_Version=1", discription)

            for(a in 1:length(h1d_args_names)){
                  arg_a = h1d_args_names[a]
                  arg_value = args_vec[arg_a]
                  arg_index = grep(arg_a, h1d_dat)
                  h1d_dat[arg_index] = paste0(arg_a, "=", arg_value)

            }

            write(discript_vec, file = discript_file, append = FALSE)
            write(h1d_dat, file = h1ddat_file, append = FALSE)

      ###
   selector_in = system.file("templates/SELECTOR.IN", package = "hydrusR")
   selector_data = readLines(selector_in, n = -1L, encoding = "unknown")

  lunit_ind = grep("LUnit", selector_data)
  unit_lines = lunit_ind + 1:2
  selector_data[unit_lines] = c(SpaceUnit, TimeUnit)

  write(selector_data, file = file.path(project_path, "selector.in"), append = F)

 # lwat_ind = lunit_ind + 4
 # lwat_input = selector_data[c(lwat_ind, lwat_ind)]

 }


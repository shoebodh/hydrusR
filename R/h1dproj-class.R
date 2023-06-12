#' Convert Units
#'
#' @param x x
#' @param from from
#' @param to to
#'
#' @return new units
#' @export
#'

convert_units = function(x, from, to) {
        x = as.numeric(x)
        units(x) = from
        units(x) = to
        x = as.numeric(x)
        return(x)
}




#' Set H1D Project Class

#' @name h1dproj-class_set
#' @rdname h1dproj-class_set
#' @slot balance A length-one numeric vector
#' @param .Object .Object
#' @import units
h1dproj <- setClass(Class = "h1dproj",
                    slots = c(
                             projname = "character",
                             projpath = "character",
                             processes = "character",
                             geometry = "list",
                             timeInfo = "list",
                             printInfo = "list",
                             iterCriteria = "list",
                             hydraulicModel = "character",
                             hystereisModel = "character",
                             watflowpars = "list",
                             bctype = "list",
                             ictype = "character",
                             projunits = "list",
                             temperature = "numeric"),
                     prototype = list(
                             projname = paste0("h1dProj", as.integer(Sys.time())),
                             projunits = list(depthunit = "cm",
                                         timeunit = "day",
                                         tempunit = "degC",
                                         massunit = "g"),
                                         projpath = path.expand("~"))
)



#' Constructor method for the h1dproj
#' @name h1dproj_initialize
#' @rdname h1dproj_initialize
#' @param .Object .Object
#' @aliases h1dproj_initialize
#' @import units
#' @importFrom methods callNextMethod new
setMethod(f = "initialize", signature = "h1dproj",
          definition = function(.Object) {

                  ### callNextMethod() transfers any value attached to .Object slots during the new() call
                  .Object = methods::callNextMethod(.Object)
                  # projname = ifelse(missing(projname),"h1dproj", projname)
                  # ##Define generic/default values to assign to slot if needed
                  # projpath = ifelse(missing(projpath),
                  #                   file.path(path.expand("~"), projname),
                  #                   file.path(projpath, projname))

                  isempty = function(x) length(x) == 0

                  timeunit = .Object@projunits$timeunit
                  depthunit = .Object@projunits$depthunit
                  tempunit = .Object@projunits$tempunit
                  massunit = .Object@projunits$massunit

                  # if(is.null(.Object@projname)) projname = paste0("h1dproj", as.numeric(Sys.time()))
                  if(isempty(.Object@processes)) .Object@processes = "Water Flow"

                  if(isempty(.Object@geometry)) .Object@geometry = list(nsoilmat = 1L,
                                                        nsubregion = 1L,
                                                        vertical = TRUE,
                                                        deltaz = convert_units(1.0,
                                                                               from = "cm",
                                                                               to = depthunit),
                                                        depth = convert_units(100,
                                                                              from = 'cm',
                                                                              to = depthunit))

                  def_timeInfo = list(initialtime = convert_units(0.0,
                                                                  from = "day",
                                                                  to = projunits$timeunit),
                                      finaltime = convert_units(100,
                                                                from = "day",
                                                                to = projunits$timeunit),
                                      initialtstep = convert_units(0.001,
                                                                   from = "day",
                                                                   to = projunits$timeunit),
                                      mintstep = convert_units(1e-005,
                                                               from = "day",
                                                               to = projunits$timeunit),
                                      maxtstep = convert_units(5.0,
                                                               from = "day",
                                                               to = projunits$timeunit))

                  if(isempty(.Object@timeInfo)) {
                          .Object@timeInfo = def_timeInfo
                  } else {

                        new_timeInfo = c(.Object@timeInfo, def_timeInfo[!names(def_timeInfo) %in% names(.Object@timeInfo)])
                        # new_timeInfo = setNames(new_timeInfo,  union(names(def_timeInfo), names(.Object@timeInfo)))
                        .Object@timeInfo = new_timeInfo
                  }



                  def_printInfo = .Object@printInfo = list(nprintTimes = 100,
                                                          ntlevel = 1,
                                                          printRegInt = TRUE,
                                                          printInterval = 1,
                                                          screenOut = TRUE,
                                                          hitEnteeAtEnd = FALSE)

                  if(isempty(.Object@printInfo)) {
                          .Object@printInfo = def_printInfo
                  } else {

                          new_printInfo = c(.Object@printInfo, def_printInfo[!names(def_printInfo) %in% names(.Object@printInfo)])
                          #new_printInfo = setNames(new_printInfo,  union(names(def_printInfo), names(.Object@printInfo)))
                          .Object@printInfo = new_printInfo

                  }

                  def_iterCriteria = list(maxiter = 10,
                                          wctol = 0.001,
                                          prtol = convert_units(1.0, "cm", depthunit))


                  hydmodel_list = list(singlePorosity = c("VG_Mualem",
                                                          "VG_Mualem_wae",
                                                          "VG_modified",
                                                          "Brooks_Corey",
                                                          "Kosugi"),
                                       dualPorosity = c("Durner",
                                                        "mim_wmt",
                                                        "mim_hmt",
                                                        "dualPerm"))



                 hystModel_list = c("no_hyst",
                                  "hyst_rc",
                                  "hyst_rc_hc",
                                   "hyst_nopump_idc",
                                   "hyst_nopump_iwc")

                 # bc_list = list(top = c("cph", "cf", "abcsl", "abcsro", "vph", "vphf"),
                 #                bottom = c("cph", "cf", "vph", "vf", "fd", "dd", "sf", "hd"))


                def_watflowpars = list(soiltype = "Loam",
                                       pars = list(Qr = 0.078,
                                                     Qs = 0.43,
                                                     Alpha = convert_units(0.036,
                                                                           from = "cm^-1",
                                                                           to = paste0(depthunit, "^-1")),
                                                     n = 1.56,
                                                     Ks = convert_units(24.96,
                                                                        from = "cm/day",
                                                                        to = paste0(depthunit, "/", timeunit)),
                                                     l = 0.5))

                if(isempty(.Object@iterCriteria)) .Object@iterCriteria = def_iterCriteria
                if(isempty(.Object@hydraulicModel)) .Object@hydraulicModel = "VG_Mualem"
                if(isempty(.Object@bctype))  .Object@bctype = list(top = "cph", bottom = "fd")
                if(isempty(.Object@ictype)) .Object@ictype = "iph"
                if(isempty(.Object@watflowpars)) .Object@watflowpars = def_watflowpars
                if(isempty(.Object@temperature)) .Object@temperature = convert_units(20, from = "degC", projunits$tempunit)


                  # def_soil <- list(nmat = 1,
                  #                  nlyrs = 1,
                  #                  zmax = convert_units(600, from = "cm", to = .Object@depthUnit),
                  #                  delz = convert_units(1, from  = "cm", to = .Object@depthUnit))
                  #


                    ##Assign values to each slot depending onn the initialization conditions
                  # .Object@projname <- projname
                  # .Object@projpath = projpath
                  # .Object@processes = processes
                  # .Object@geometry = geometry
                  # .Object@timeInfo = timeInfo
                  # .Object@printInfo = printInfo
                  # .Object@iterCriteria = iterCriteria
                  # .Object@hydraulicModel = hydraulicModel
                  # .Object@watflowpars = watflowpars
                  # .Object@bctype = bctype
                  # .Object@ictype = ictype

                  # if(length(.Object@soil) == 0) .Object@soil <- def_soil
                  # if(length(.Object@timeInfo) == 0) .Object@timeInfo <- def_time

                  if(!dir.exists(.Object@projpath)) dir.create(file.path(.Object@projpath, projname))
                  return(.Object)
          })

###########
##Method to print hydrus 1d project summary
#'Show Method for the h1dproj class
#' @name h1dproj_show
#' @rdname h1dproj_show
#' @param object object
#' @aliases h1dproj_show
#' @import crayon
#' @examples
#' h1dproj()
#' methods::new("h1dproj")
#'
setMethod("show", signature = "h1dproj", definition =
                  function(object) {
                          geometry = object@geometry
                          timeInfo = object@timeInfo
                          projunits = object@projunits

                          yw <- function(x) cyan(x)
                          message(bold(blue("HYDRUS-1D project: ")))
                          message(yw("     Project name:  "), blue(object@projname))
                          message(yw("     Project path:  "), blue(dirname(object@projpath)))
                          message(yw("     Initial time:  "), blue(timeInfo$initialtime))
                          message(yw("     Final time:     "),   blue(timeInfo$finaltime))

                          # message(bold(magenta("..................\n")))
                          message(bold(blue("UNITS:")),
                                  yw("\n     Depth         = "), blue(projunits$depthunit),
                                  yw("\n     Mass        = "), blue(projunits$massunit),
                                  yw("\n     Temperature   = "), blue(projunits$tempunit),
                                  yw("\n     Time          = "), blue(projunits$timeunit))

                          # message(bold(magenta("..................\n")))
                          message(bold(blue("SOIL INFO:")))
                          message(yw("     Number of materials = "), blue(geometry$nsoilmat))
                          message(yw("     Number of Layers    = "), blue(geometry$depth/geometry$deltaz))
                          message(yw(paste0("     Soil Depth (", projunits$depthunit, ") = ")), blue(sprintf("%.2f", geometry$depth)))
                          message(yw(paste0("     delz (", projunits$depthunit, ") = ")), blue(sprintf("%.2f", geometry$deltaz)))

                          return(invisible(NULL))
                  }

)

#' Create PROFILE.DAT
#'
#' @param H1Dproj  H1Dproj
#' @param obs.nodes obs.nodes (default: NULL)
#' @param Conc concentation (default: 0.0)
#'
#' @return create PROFILE.DAT
#' @export
#'
create.profile_dat <- function(H1Dproj, obs.nodes = NULL, Conc = 0.0) {

        profile.depth = H1Dproj@geometry$depth
        dz = H1Dproj@geometry$deltaz
        Temp = H1Dproj@temperature
        out.file = file.path(H1Dproj@projpath, H1Dproj@projname, "PROFILE.DAT")

        profile.template = system.file("templates/PROFILE.DAT", package = "hydrusR")
        profile_dat = readLines(profile.template, n = -1L, encoding = "unknown")

        header_line = profile_dat[1:2]

        dline = profile_dat[3]
        dline_split  = unlist(strsplit(dline, " "))
        dline_split = dline_split[dline_split!= ""]

        dval = dline_split[2]

        dformat_new = format2sci(-profile.depth, ndec = 6, power.digits = 3)

        dline_split_new = dline_split
        dline_split_new[2] = dformat_new
        #   dline_split_new = dline_split_new[]


        fmt_space = c(5, 15, 15, 15)
        fmt_vec = paste("%", fmt_space, "s", sep = "")

        dline_fmt_new = sprintf(fmt = fmt_vec, dline_split_new)
        dline_fmt_new = paste(dline_fmt_new, collapse = "")

        table_header = profile_dat[4]

        dhead_val = substr(table_header, start = 1, stop = 5)
        dhead_val = as.numeric(trimws(dhead_val))

        header_rest = substr(table_header, start = 6, stop = nchar(table_header))
        dhead_val_new = sprintf("%5s", (profile.depth + 1))

        table_header_new = paste0(dhead_val_new, header_rest)

        table_body = profile_dat[5:length(profile_dat)]
        table_body = table_body[-length(table_body)] #### row with number of observation points value

        body_split = strsplit(table_body, split = " ")
        body_split2 = sapply(body_split, FUN = function(x) x[x!= ""])

        # body_new = do.call("rbind", body_split2)

        table_body_new = t(body_split2)

        depth_vec = seq(0, profile.depth, by = dz)
        head_vec = numeric(length(depth_vec))

        row_counts = seq(length(depth_vec))

        zero_vec = numeric(length(depth_vec))
        one_vec = rep(1, length(depth_vec))
        temp_vec = rep(Temp, length(depth_vec))
        conc_vec = rep(Conc, length(depth_vec))

        zero_vec_fmt = mapply(FUN = format2sci, zero_vec, ndec = 6, power.digits = 3)
        one_vec_fmt = mapply(FUN = format2sci, one_vec, ndec = 6, power.digits = 3)


        depth_vec_fmt = mapply(FUN = format2sci, depth_vec, ndec = 6, power.digits = 3)
        depth_vec_fmt = paste0("-", depth_vec_fmt)
        head_vec_fmt = zero_vec_fmt

        mat_vec = one_vec
        layer_vec = one_vec

        beta_vec_fmt = zero_vec_fmt
        axz_vec_fmt = one_vec_fmt
        bxz_vec_fmt = one_vec_fmt
        dxz_vec_fmt = one_vec_fmt

        temp_vec_fmt = mapply(FUN = format2sci, temp_vec, ndec = 6, power.digits = 3)
        conc_vec_fmt = mapply(FUN = format2sci, conc_vec, ndec = 6, power.digits = 3)


        profile_df = data.frame(row_counts,
                                depth_vec_fmt,
                                head_vec_fmt,
                                mat_vec,
                                layer_vec,
                                beta_vec_fmt,
                                axz_vec_fmt,
                                bxz_vec_fmt,
                                dxz_vec_fmt,
                                temp_vec_fmt,
                                conc_vec_fmt)
        profile_mat = as.matrix(profile_df)

        fmt_space_body = c(5, 15, 15, 5, 5, 15, 15, 15, 15, 15, 15)
        fmt_vec_body = paste("%", fmt_space_body, "s", sep = "")

        profile_mat_fmt = profile_mat
        colnames(profile_mat_fmt) = NULL

        for(n in 1: nrow(profile_mat_fmt)){

                profile_mat_fmt[n, ] = sprintf(fmt_vec_body, profile_mat[n, ])

        }

        profile_mat_fmt2 = apply(profile_mat_fmt, MARGIN = 1, FUN = paste, collapse = "")
        tspace = sprintf("%13s", "")
        profile_mat_fmt2 = paste(profile_mat_fmt2, tspace)

        profile_data_new = c(header_line, dline_fmt_new, table_header_new, profile_mat_fmt2, " 0")

        if(!is.null(obs.nodes)) {
                profile_data_new[length(profile_data_new)] = paste0(" ", length(obs.nodes))
                obs_node_line = paste(obs.nodes, collapse = "  ")
                profile_data_new = c(profile_data_new, paste0(" ", obs_node_line))
        }

         write(profile_data_new, file = out.file, append = FALSE)

         # if(!is.null(obs.nodes)) write.obs.nodes(H1Dproj@projpath, obs.nodes)

}


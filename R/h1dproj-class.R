convert_units = function(x, from, to) {
        x = as.numeric(x)
        units(x) = from
        units(x) = to
        x = as.numeric(x)
        return(x)
}

h1dproj<- setClass("h1dproj",
                     slots = c(
                             projname = "character",
                             projpath = "character",
                             depthUnit = "character",
                             timeUnit = "character",
                             tempUnit = "character",
                             massUnit = "character",
                             soil = "list",
                             timeInfo = "list",
                             temperature = "numeric"),

                     prototype = list(
                             depthUnit = "cm",
                             timeUnit = "hour",
                             tempUnit = "degC",
                             massUnit = "g",
                             temperature = 20.0)
)


#############################
#' Constructor method for the h1dproj
#' @name h1dproj
#' @rdname h1dproj-class
#' @import units
#' @aliases h1dproj
setMethod(f = "initialize", signature = "h1dproj",
          definition = function(.Object, projname, projpath, ...) {

                  ### callNextMethod() transfers any value attached to .Object slots during the new() call
                  .Object = callNextMethod(.Object, ...)
                  projname = ifelse(missing(projname),"h1dproj", projname)
                  ##Define generic/default values to assign to slot if needed
                  projpath = ifelse(missing(projpath),
                                    file.path(path.expand("~"), projname),
                                    file.path(projpath, projname))

                  def_soil <- list(nmat = 1,
                                   nlyrs = 1,
                                   zmax = convert_units(600, from = "cm", to = .Object@depthUnit),
                                   delz = convert_units(1, from  = "cm", to = .Object@depthUnit))

                  def_time = list(itime = convert_units(0.0, from = "hour", to = .Object@timeUnit),
                                  ftime = convert_units(100, from = "hour", to = .Object@timeUnit),
                                  delt = convert_units(1.0, from = "hour", to = .Object@timeUnit))

                    ##Assign values to each slot depending onn the initialization conditions
                  .Object@projname <- projname
                  .Object@projpath = projpath

                  if(length(.Object@soil) == 0) .Object@soil <- def_soil
                  if(length(.Object@timeInfo) == 0) .Object@timeInfo <- def_time

                  if(!dir.exists(projpath)) dir.create(projpath)
                  return(.Object)
          })

###########
##Method to print hydrus 1d project summary
#'Show Method for the h1dproj class
#' @import crayon
#' @name show
#' @rdname h1droj-class
#' @aliases h1dproj
#' @usage h1dproj(), new("h1dproj")
#'
setMethod("show", signature = "h1dproj", definition =
                  function(object) {
                          soil = object@soil
                          timeInfo = object@timeInfo

                          yw <- function(x) cyan(x)
                                                     message(bold(blue("AN OBJECT OF CLASS ")), bold(yw(class(object))))
                          message(yw("     Initial time:  "), yw(object@timeInfo$itime))
                          message(yw("     Final time:      "),   yw(object@timeInfo$ftime))
                          message(yw("     Time step:       "), yw(object@timeInfo$delt))

                          # message(bold(magenta("..................\n")))
                          message(bold(blue("UNITS:")),
                                  yw("\n     Depth         = "), yw(object@depthUnit),
                                  yw("\n     Mass        = "), yw(object@massUnit),
                                  yw("\n     Temperature   = "), yw(object@tempUnit),
                                  yw("\n     Time          = "), yw(object@timeUnit))

                          # message(bold(magenta("..................\n")))
                          message(bold(blue("SOIL INFO:")))
                          message(yw("     Number of materials = "), yw(soil$nmat))
                          message(yw("     Number of Layers    = "), yw(soil$nlyrs))
                          message(yw(paste0("     Soil Depth (", object@depthUnit, ") = ")), yw(sprintf("%.2f", soil$zmax)))
                          message(yw(paste0("     delz (", object@depthUnit, ") = ")), yw(sprintf("%.2f", soil$delz)))

                          return(invisible(NULL))
                  }
)


h1d_create.profile<- function(h1d.proj, obs.nodes = NULL, Temp = 20.0, Conc = 0, ...) {

        profile.depth = h1d.proj@soil$zmax
        dz = h1d.proj@soil$delz
        Temp = h1d.proj@temperature
        out.file = file.path(h1d.proj@projpath,  "PROFILE.DAT")

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

         write(profile_data_new, file = out.file, append = FALSE)

         if(!is.null(obs.nodes)) write.obs.nodes(h1d.proj@projpath, obs.nodes)

}


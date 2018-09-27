#' Creates a new PROFILE.DAT file using a template included in the package.
#'
#' @param project.path Location of the H1D project in the directory
#' @param out.file Default is always 'PROFILE.OUT'
#' @param dz Descretization step of the profile.
#' @param obs.nodes Vector of observation points in the profile
#' @param Temp Temperate input default is 20 degree C
#' @param Conc Concentration, zero in initial profile
#'
#' @return
#' @export
#'
#' @examples

create.soil.profile<- function(project.path, out.file = "PROFILE.DAT", profile.depth,  dz = 1,
                               Temp = 20, Conc = 0, ...) {

      profile.template = system.file("templates/PROFILE.DAT", package = "hydrusR")
      profile_dat = readLines(profile.template, n = -1L, encoding = "unknown")

      header_line = profile_dat[1:3]

      dline = profile_dat[4]
      dline_split  = unlist(strsplit(dline, " "))
      dline_split = dline_split[dline_split!= ""]

      dval = dline_split[2]

      dformat_new = format2sci(-profile.depth, ndec = 6, power.digits = 3)

      dline_split_new = dline_split
      dline_split_new[2] = dformat_new
      dline_split_new = dline_split_new[]


      fmt_space = c(5, 15, 15, 15)
      fmt_vec = paste("%", fmt_space, "s", sep = "")

      dline_fmt_new = sprintf(fmt = fmt_vec, dline_split_new)
      dline_fmt_new = paste(dline_fmt_new, collapse = "")

      table_header = profile_dat[5]

      dhead_val = substr(table_header, start = 1, stop = 5)
      dhead_val = as.numeric(gsub(" ", "", dhead_val))
      header_rest = substr(table_header, start = 6, stop = nchar(table_header))
      dhead_val_new = sprintf("%5s", (profile.depth + 1))

      table_header_new = paste0(dhead_val_new, header_rest)

      table_body = profile_dat[6:length(profile_dat)]
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

      profile_data_new = c(header_line, dline_fmt_new, table_header_new, profile_mat_fmt2, 0)

      profile_file = file.path(project.path, out.file)

      write(profile_data_new, file = profile_file, append = FALSE)

}

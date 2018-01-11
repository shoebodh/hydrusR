#' Write print times in selector.in
#'
#' @param project.path Location of the H1D project in the directory
#' @param tmin Begining print time (e.g., 1)
#' @param tmax End print time (e.g. 240 hours)
#' @param tstep Time step to print out put at
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.print.times<- function(project.path, tmin, tmax, tstep, TimeUnit, ...){
      input.file = file.path(project.path, "selector.in")

      hydrus_input = readLines(con = input.file, n = -1L, encoding = "unknown")

      Tprint_ind = grep("TPrint", hydrus_input)
      end_Tprint_ind = grep("BLOCK G", hydrus_input)


      ptimes = seq((tmin + tstep), tmax, by = tstep)

      get.decimals <- function(x) {
            if ((x %% 1) != 0) {
                  nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
            } else {
                  return(0)
            }
      }

      if(length(ptimes) > 1000){
            stop("ERROR!Hydrus 1D doesnot allow printing > 1000 time steps!")
      }

      nrows = floor(length(ptimes)/6)

      p1 = ptimes[1:(nrows*6)]
      rem_tstep= tmax - max(p1)


      ptimes_mat = matrix(p1, nrow = nrows, ncol = 6, byrow = TRUE)
      fmt_vec = character(6)
      tstep_decimals = get.decimals(tstep)
      fmt_vec = c("%11.0f", rep("%12.0f", 5))

      if(tstep_decimals > 0){
            fmt_vec = gsub(pattern = "0", replacement = tstep_decimals, fmt_vec)
      }


      ptimes_mat_fmt = ptimes_mat
      for(p in 1:nrow(ptimes_mat_fmt)){
            ptimes_mat_fmt[p, ] = sprintf(fmt = fmt_vec, ptimes_mat[p, ])
      }

      ptimes_mat_fmt = apply(ptimes_mat_fmt, MARGIN = 1, FUN = paste, collapse = "")

      if(rem_tstep > 0) {
            last_line = (tmax - rem_tstep + tstep):tmax
            last_line_fmt = sprintf(fmt = fmt_vec[1:length(last_line)], last_line)
            last_line_fmt = paste(last_line_fmt, collapse = "")

            ptimes_mat_final = c(ptimes_mat_fmt, last_line_fmt)
      }  else {

            ptimes_mat_final = ptimes_mat_fmt
      }

      ########## Update Max print value
      mpl_ind = grep(pattern = " MPL", hydrus_input)
      mpl_line = hydrus_input[(mpl_ind+1)]
      mpl_line_split = unlist(strsplit(mpl_line, split = " "))
      mpl_line_split[length(mpl_line_split)] = sprintf(fmt = "%2s", as.character(length(ptimes)))
      mpl_line_new = paste(mpl_line_split, collapse = " ")
      hydrus_input[(mpl_ind+1)] = mpl_line_new
      ###########

      tmax_ind = grep(" tMax", hydrus_input)
      tmax_line = hydrus_input[(tmax_ind + 1)]

      # tmax_line_split = unlist(strsplit(tmax_line, split = " "))
      # tmax_line_split[length(tmax_line_split)] = sprintf("%2s", as.character(tmax))
      # tmax_line_new = paste(tmax_line_split, collapse = " ")

      tmax_line_split = c(tmin, tmax)
      tmax_line_new = sprintf(c("%11.0f", "%12.0f"), tmax_line_split)
      tmax_line_new = paste(tmax_line_new, collapse = "")


      hydrus_input[(tmax_ind + 1)] = tmax_line_new

      input_p1 = hydrus_input[1:Tprint_ind]
      input_p2 = ptimes_mat_final
      input_p3 = hydrus_input[end_Tprint_ind:length(hydrus_input)]


      selector_data = c(input_p1, input_p2, input_p3)


     #  timeinfo_ind = grep("*** BLOCK C", selector_data, fixed = TRUE)
     #  timeinfo_data = selector_data[timeinfo_ind+2]
     #
     #  timeinfo_split = unlist(strsplit(x = timeinfo_data, split = " "))
     #  timeinfo_split = timeinfo_split[timeinfo_split != ""]
     #  timeinfo_new = as.numeric(timeinfo_split)
     #
     #  names(timeinfo_split) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
     #  names(timeinfo_new) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
     #
     #  if(TimeUnit == "hours"){
     #
     #        timeinfo_new[c("dt", "dtMin", "dtMax")] = 24*timeinfo_new[c("dt", "dtMin", "dtMax")]
     #
     #  } else if (TimeUnit == "minutes") {
     #        timeinfo_new[c("dt", "dtMin", "dtMax")] = 60*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
     #
     #  } else if(TimeUnit == "seconds"){
     #        timeinfo_new[c("dt", "dtMin", "dtMax")] = 3600*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
     #
     #  } else if(TimeUnit == "years") {
     #        timeinfo_new[c("dt", "dtMin", "dtMax")] = 1/365*timeinfo_new[c("dt", "dtMin", "dtMax")]
     #
     #  }
     #
     #  timeinfo_new[c("dt", "dtMin", "dtMax")] = format2sci(timeinfo_new[c("dt", "dtMin", "dtMax")], ndec = 3, power.digits = 3)
     #  fmt_space = c(12, 13, 12, 8, 8, 6, 6, 6)
     #  fmt_vec = paste("%", fmt_space, "s", sep = "")
     #  timeinfo_new_fmt = sprintf(fmt = fmt_vec, timeinfo_new)
     #  timeinfo_new_str = paste(timeinfo_new_fmt, collapse = "")
     #
     # selector_data[timeinfo_ind + 2] = timeinfo_new_str



      write(selector_data, file = input.file, append = F)
      # write(input_p2, file = input.file, append = T)
      # write(input_p3, file = input.file, append = T)
      # write(input_p4, file = "selector.in", append = T)


}

#' Write atmospheric boundary condition inputs
#'
#' @param file.atmosph.in
#' @param MaxAL
#' @param tstep
#' @param atm.bc.data
#' @param sim_ind
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.atmosph.in<- function(file.atmosph.in, MaxAL,
            tstep, atm.bc.data, sim_ind = 1, ...){

        # default.filename = "ATMOSPH.IN"

      atm_data = readLines(con = file.atmosph.in, n = -1L, encoding = "unknown")
      atm_data_bak = atm_data
      # write(atm_data, file = "ATMOSPH_IN.BAK", append = F)

      maxAL_ind = grep(" MaxAL", atm_data)
      tAtm_ind = grep(" tAtm", atm_data)

      tMax = MaxAL*tstep

      atm_data[(maxAL_ind + 1)] = sprintf("%7.0f", MaxAL)
      end_line = atm_data[grep("end", atm_data)]

      # bc_data = atm_data[(tAtm_ind +1): (end_line - 1)]
      data_ind = (tMax*(sim_ind-1) + 1):(sim_ind*tMax)

      # tAtm = seq(tstep, tMax, by = tstep)

      bc_data_vars = c("Prec", "rSoil", "rRoot", "hCritA", "rB",
                       "hB", "ht")

      bc_data_new = atm.bc.data[data_ind, bc_data_vars]
      bc_data_new = data.frame(tAtm = 1:tMax, bc_data_new, row.names = NULL)
      bc_data_new = bc_data_new[rep(seq_len(nrow(bc_data_new)), each = 4), ]
      bc_data_new$tAtm = seq(tstep, tMax, by = tstep)
      row.names(bc_data_new) = NULL

      # stopifnot(max(bc_data_new$tAtm) == MaxAL)

      decimalplaces <- function(x) {
            if ((x %% 1) != 0) {
                  nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
            } else {
                  return(0)
            }
      }

      tstep_decimals = decimalplaces(tstep)


      fmt_vec = c("%11.0f", "%12.3f", "%12.4f", "%12.4f", "%12.0f", "%12.7f", "%12.4f", "%12.4f")
      fmt_vec[1] = sub(pattern = "0", replacement = tstep_decimals, fmt_vec[1])

      bc_data_fmt = bc_data_new

      for(a in 1:nrow(bc_data_fmt)) {

            bc_data_fmt[a, ] = sprintf(fmt = fmt_vec, bc_data_new[a, ])
      }


      bc_data_fmt = apply(bc_data_fmt, MARGIN = 1, FUN = paste, collapse = "")

      atm_input1 = atm_data[1:tAtm_ind]
      atm_input2 = bc_data_fmt
      atm_input3 = end_line

      atmosph_input_new = c(atm_input1, atm_input2, atm_input3)

      write(atmosph_input_new, file = file.atmosph.in, append = F)


}

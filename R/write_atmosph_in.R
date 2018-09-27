#' Write atmospheric boundary condition inputs
#'
#' @param project.path
#' @param maxAL
#' @param deltaT
#' @param atm.bc.data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.atmosph.in<- function(project.path, maxAL, deltaT,
                            atm.bc.data, hCritS = 0, input.pet = T,
                            LAI = 0.39, ...){

        out.file = "ATMOSPH.IN"
        # default.filename = "ATMOSPH.IN"
        template_atmosph_in = system.file("templates/ATMOSPH.IN", package = "hydrusR")
        atm_data = readLines(con = template_atmosph_in, n = -1L, encoding = "unknown")

        if(file.exists(file.path(project.path, out.file))){
                file.remove(file.path(project.path, out.file))
        }
        extinction_ind = grep("Extinction", atm_data)

        # atm_data_bak = atm_data

        if(input.pet == TRUE){
                atm_data = atm_data
                atm_data[(extinction_ind + 1)] = sprintf("%8s", LAI)

        } else {
                atm_data = atm_data[-c(extinction_ind, extinction_ind + 1)]

        }

        # write(atm_data, file = "ATMOSPH_IN.BAK", append = F)

        hcrits_ind = grep("hCritS", atm_data)
        atm_data[hcrits_ind + 1] = sprintf("%7.0f", hCritS)

        maxAL_ind = grep("MaxAL", atm_data)
        tAtm_ind = grep(" tAtm", atm_data)

        tMax = maxAL*deltaT

        atm_data[(maxAL_ind + 1)] = sprintf("%7.0f", maxAL)
        end_line = atm_data[grep("end", atm_data)]

        # bc_data = atm_data[(tAtm_ind +1): (end_line - 1)]
        # data_ind = (tMax*(sim_ind-1) + 1):(sim_ind*tMax)

        # tAtm = seq(deltaT, tMax, by = deltaT)

        bc_data_vars = c("tAtm", "Prec", "rSoil", "rRoot", "hCritA", "rB",
                         "hB", "ht")

        bc_data_new = atm.bc.data[1:maxAL, bc_data_vars]
        # bc_data_new = data.frame(tAtm = seq(deltaT, tMax, deltaT), bc_data_new, row.names = NULL)
        #  bc_data_new = bc_data_new[rep(seq_len(nrow(bc_data_new)), each = 4), ]
        #  bc_data_new$tAtm = seq(deltaT, tMax, by = deltaT)
        row.names(bc_data_new) = NULL

        tstep_decimals = get.decimalplaces(deltaT)

        fmt_vec = c("%11.0f", "%12.3f", "%12.4f", "%12.4f", "%12.0f", rep("%12.4f",8))
        fmt_vec[1] = sub(pattern = "0", replacement = tstep_decimals, fmt_vec[1])

        bc_data_fmt = bc_data_new

        for(a in 1:nrow(bc_data_fmt)) {
                bc_data_fmt[a, ] = sprintf(fmt = fmt_vec[1:ncol(bc_data_fmt)], bc_data_new[a, ])
        }
        bc_data_fmt = apply(bc_data_fmt, MARGIN = 1, FUN = paste, collapse = "")

        atm_input1 = atm_data[1:tAtm_ind]
        atm_input2 = bc_data_fmt
        atm_input3 = end_line

        atmosph_input_new = c(atm_input1, atm_input2, atm_input3)
        atmosph_in_file = file.path(project.path, out.file)
        write(atmosph_input_new, file = atmosph_in_file, append = F)

}

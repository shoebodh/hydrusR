#' Write bottom Boundary Condition
#'
#' @param constant.bc constant.bc
#' @param bc.type bc.type
#' @param bc.value bc.value
#' @param project.path project.path
#'
#' @return Write bottom Boundary Condition to "SELECTOR.IN"
#' @export

write.bottom.bc<- function(constant.bc, bc.type, bc.value, project.path) {
        ## Writes bottom constant boundary conditions "flux" or "head"
        input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                               n = -1L, encoding = "unknown")
        flow_block_ind = grep("BLOCK B", input_data)
        time_block_ind = grep("BLOCK C", input_data)

        flow_block = input_data[flow_block_ind : (time_block_ind - 1)]
        botInf_ind = grep("BotInf", flow_block)
        hTab1_ind = grep("hTab1", flow_block)

        botInf_input = flow_block[botInf_ind+1]
        botInf_input_split = unlist(strsplit(botInf_input, split = " "))
        botInf_input_split = botInf_input_split[botInf_input_split != ""]

        botInf_fmt_vec = c("%2s", "%6s", "%6s", "%6s", "%7s", "%7s", "%7s")

        if(bc.type == "flux"){

                if(isTRUE(constant.bc)) {

                        botInf_input_split[1] = 'f'
                        botInf_input_split[5] = '-1'

                        botInf_input_fmt = sprintf(fmt = botInf_fmt_vec, botInf_input_split)
                        botInf_input_new = paste(botInf_input_fmt, collapse = "")

                        new_input_names = sprintf(fmt = "%13s", c("rTop", "rBot", "rRoot"))
                        new_input_values = rep(0, 3)
                        new_input_values[2] = bc.value
                        new_value_fmt = sprintf(fmt = c("%12.6f", "%13.6f", "%13.6f"), new_input_values)
                        new_bc_value_fmt = paste(new_value_fmt, collapse = "")
                        newInf_head_fmt = paste(new_input_names, collapse = "")
                        # flow_block_new[botInf_ind + 1] = botInf_input_new

                        flow_block_new = c(flow_block[1:botInf_ind], botInf_input_new,
                                           newInf_head_fmt,
                                           new_bc_value_fmt, flow_block[hTab1_ind:length(flow_block)])



                } else {
                        botInf_input_split[1] = 't'
                        botInf_input_split[5] = '-1'

                        botInf_input_fmt = sprintf(fmt = botInf_fmt_vec, botInf_input_split)
                        botInf_input_new = paste(botInf_input_fmt, collapse = "")

                        flow_block_new = c(flow_block[1:botInf_ind], botInf_input_new,
                                           flow_block[hTab1_ind:length(flow_block)])


                }



        }
        ######################################
        if(bc.type == "head"){
                if(isTRUE(constant.bc)) {

                        botInf_input_split[1] = 'f'
                        botInf_input_split[5] = '1'

                        #       botInf_input_fmt = sprintf(fmt = botInf_fmt_vec, botInf_input_split)
                        #       botInf_input_new = paste(botInf_input_fmt, collapse = "")
                        #
                        #       flow_block_new = c(flow_block[1:botInf_ind], botInf_input_new,
                        #                flow_block[hTab1:length(flow_block)])

                } else {

                        botInf_input_split[1] = 't'
                        botInf_input_split[5] = '1'


                }

                botInf_input_fmt = sprintf(fmt = botInf_fmt_vec, botInf_input_split)
                botInf_input_new = paste(botInf_input_fmt, collapse = "")

                flow_block_new = c(flow_block[1:botInf_ind], botInf_input_new,
                                   flow_block[hTab1:length(flow_block)])


        }


        input_data_new = c(input_data[1:(flow_block_ind-1)], flow_block_new,
                           input_data[time_block_ind:length(input_data)])

        write(input_data_new, file = file.path(project.path, "SELECTOR.IN"), append = F)


}

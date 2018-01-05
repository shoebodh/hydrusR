#' Write soil hydraulic parameters to selector.in
#'
#' @param project.path
#' @param model
#' @param hysteresis
#' @param para
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.hydraulic.para<- function(project.path, model = 0, hysteresis = 0, para, ...) {

    smr_models<- c("van Genuchten (VG)","mod van Genuchten (VGM)",
                   "Brooks Corey(BC)","van Genuchten 6 para (VGM2)",
                   "Kosugi(KOS)","dual Porosity (DUR)","dual porosity 2 (DUR2)",
                   "dual porosity 3 (DUR3)","dual permeability (DUP)")

       hydraulic_models <- data.frame(code = c(0:7, 9),
                   model = smr_models)

      input_data = readLines(con = file.path(project.path, "selector.in"),
                             n = -1L, encoding = "unknown")

      basic_inf_ind  = grep("BLOCK A", input_data)
      flow_block_ind = grep("BLOCK B", input_data)
      time_block_ind = grep("BLOCK C", input_data)
      rwu_block_ind  = grep("BLOCK G", input_data)


      flow_block = input_data[flow_block_ind : (time_block_ind - 1)]

      model_line_ind = grep("Model", flow_block)

      model_line = flow_block[model_line_ind + 1]
      model_line_split = unlist(strsplit(model_line, split = " "))
      non_empty = which(model_line_split != "")
      model_line_split[non_empty[1]] = as.character(model)
      model_line_split[non_empty[2]] = as.character(hysteresis)
      model_line_new = paste(model_line_split, collapse = " ")

      flow_block[model_line_ind+1] = model_line_new

      para_line_ind = grep("thr|ths", flow_block)
      para_values = flow_block[para_line_ind + 1]
      para_values_split = unlist(strsplit(para_values, split = " "))
      para_values_split = para_values_split[para_values_split != ""]

      if(length(para_values_split) != length(para)) {
            stop("ERROR|number of parameters provided doesnot match with model selected!")

      }

      para_values_new = c(para$thr, para$ths,
                          para$Alfa, para$n,
                          para$Ks, para$l)

      format_vec = c("%7.4f", "%8.4f", "%8.4f", "%8.3f", "%11.6f", "%8.2f")

      para_values_fmt = sprintf(fmt = format_vec, para_values_new)
      para_values_new = paste(para_values_fmt, collapse = "")
      flow_block[para_line_ind+1] = para_values_new

      input_data[flow_block_ind : (time_block_ind - 1)] = flow_block

      write(input_data, file =  file.path(project.path, "selector.in"), append = F)

}

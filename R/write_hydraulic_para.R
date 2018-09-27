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

    smr_models<- c("van Genuchten (VG)","van Genuchten 6 para (VGM)",
                   "Brooks Corey(BC)","mod van Genuchten (VGM2)",
                   "Kosugi(KOS)","dual Porosity (DUR)","dual porosity 2 (DUR2)",
                   "dual porosity 3 (DUR3)")

     hydraulic_models <- data.frame(code = c(0:7),
                   model = smr_models, stringsAsFactors = F)

para_name_list = list("0" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                      "1" = c("thr", "ths", "Alfa", "n", "Ks", "l", "thm", "tha", "thk", "Kk"),
                      "2" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                      "3" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                      "4" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                      "5" = c("thr", "ths", "Alfa", "n", "Ks", "l", "w2", "Alfa2", "n2"),
                      "6" = c("thr", "ths", "Alfa", "n", "Ks", "l", "thrIm", "thsIm", "Omega"),
                      "7" = c("thr", "ths", "Alfa", "n", "Ks", "l", "thrIm", "thsIm", "AlfaIm", "nIm", "Omega"))

input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                       n = -1L, encoding = "unknown")

      basic_inf_ind  = grep("BLOCK A", input_data)
      flow_block_ind = grep("BLOCK B", input_data)
      time_block_ind = grep("BLOCK C", input_data)
      rwu_block_ind  = grep("BLOCK G", input_data)

      flow_block = input_data[flow_block_ind : (time_block_ind - 1)]

      model_line_ind = grep("Model", flow_block)

      input_model_name = hydraulic_models[hydraulic_models$code == model, "model"]

      input_para_name = para_name_list[[as.character(model)]]

      # stopifnot(identical(input_para_name, names(para)))

      model_line = flow_block[model_line_ind + 1]

      model_line_split = unlist(strsplit(model_line, split = " "))
      non_empty = which(model_line_split != "")
      model_line_split[non_empty[1]] = as.character(model)
      model_line_split[non_empty[2]] = as.character(hysteresis)
      model_line_new = paste(model_line_split, collapse = " ")

      flow_block[model_line_ind+1] = model_line_new

      para_line_ind = grep("thr|ths", flow_block)

      para_name_fmt_vec = c("%6s", "%8s", "%8s", "%7s", "%11s", "%8s", "%9s", "%8s", "%11s", "%8s")

      para_line_fmt = mapply(FUN = sprintf, input_para_name, fmt = para_name_fmt_vec[1:length(input_para_name)])
      para_line_new = paste(para_line_fmt, collapse = "")

      #       para_values = flow_block[para_line_ind + 1]
      # para_values_split = unlist(strsplit(para_values, split = " "))
      # para_values_split = para_values_split[para_values_split != ""]
      #
      # if(length(para_values_split) != length(para)) {
      #       stop("ERROR|number of parameters provided doesnot match with model selected!")
      #
      # }
      #
      # para_values_new = c(para$thr, para$ths,
      #                     para$Alfa, para$n,
      #                     para$Ks, para$l)

      para_values =  unlist(para[input_para_name])

      value_format_vec = c("%7.4f", "%8.4f", "%8.4f", "%8.3f", "%11.5f", "%8.2f", "%8.3f", "%8.3f", "%11.3f", "%8.3f")
      para_values_fmt = sprintf(fmt = value_format_vec[1:length(para_values)], para_values)
      para_values_new = paste(para_values_fmt, collapse = "")

      flow_block[para_line_ind] = para_line_new
      flow_block[para_line_ind+1] = para_values_new

      input_data[flow_block_ind : (time_block_ind - 1)] = flow_block

      write(input_data, file =  file.path(project.path, "SELECTOR.IN"), append = F)

}

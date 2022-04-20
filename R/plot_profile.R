#' Plot pressure profile
#'
#' @param project.path project.path
#' @param output default: "Head"
#'
#' @return plot pressure profile
#' @export
#'
plot_pressure.profile <- function(project.path, output = "Head"){

      h1d_output = read.nod_inf(project.path, out.file = "Nod_Inf.out", output = output)
      Depth = unique(h1d_output$Depth)

 output_split = split(x = h1d_output, f = h1d_output$Time)

 total_times = length(output_split)

 tindex = seq(0, by = 20, total_times)

 output_in = output_split[tindex]

 output_in = do.call("cbind", output_in)

 output_names = names(output_in)

 output_names_p = grep(paste(output, collapse = "|"), output_names, value = T)

output_p = as.matrix(output_in[, ..output_names_p])

plot_range = range(max(output_p), -(max(output_p)))

matplot(x = output_p, y = Depth, xlim = plot_range, type = "l", xlab = "Hean (L)")

graphics::abline(v = 0, lwd = 2, col = "grey40")

}

#' Plot moisture profile
#'
#' @param project.path project.path
#' @param output default: "Moisture"
#'
#' @return plot moisture profile
#' @export
#' @importFrom graphics abline matplot
plot_moisture.profile <- function(project.path, output = "Moisture"){

      h1d_output = read.nod_inf(project.path, out.file = "Nod_Inf.out", output = output)
      Depth = unique(h1d_output$Depth)

      output_split = split(x = h1d_output, f = h1d_output$Time)

      total_times = length(output_split)

      tindex = seq(0, by = 20, total_times)

      output_in = output_split[tindex]

      output_in = do.call("cbind", output_in)

      output_names = names(output_in)

      output_names_p = grep(paste(output, collapse = "|"), output_names, value = T)

      output_p = as.matrix(output_in[, ..output_names_p])

      plot_range = range(0, (max(output_p)))

      graphics::matplot(x = output_p, y = Depth, xlim = plot_range, type = "l",
      xlab = parse(text = "Theta~(L^3~L^-3)"))
}

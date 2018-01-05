#' Calculate RMSE and Nash-Sutcliffe CEFF of the simulation
#'
#' @param data
#' @param var.name
#' @param print.stats
#' @param plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
model.eval<- function(data, var.name = "",  print.stats = TRUE, plot = FALSE, ...){
   if(plot == TRUE) {
          plot(data$obs, data$pred, type = "p", xlim = range(data$pred),
           ylim = range(data$pred), xlab = paste("Observed", var.name, sep = " "),
           ylab = paste("Predicted", var.name, sep = " "), col = "dodgerblue", bty = "l",
           cex.lab = 1, cex.axis = 1, pch = 16)
      lines(data$obs, data$obs, lwd = 2, col = "red")
      legend("bottomright", bty = "n", legend = c("1:1 line"), col = c("red",
            NA), cex = 1, pch = c(NA, NA), lty = c(1, NA), lwd = 2)
   }
      if(print.stats == TRUE) {
            RMSE <- sqrt(mean(data$obs - data$pred)^2)
            CEFF <- 1 - sum((data$obs - data$pred)^2)/sum((data$obs - mean(data$obs))^2)


            return(c(RMSE = RMSE, CEFF = CEFF))
      }
}





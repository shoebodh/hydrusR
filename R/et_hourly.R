#' Generates hourly ET inputs from provided daily value.
#'
#' @param Et.Daily
#' @param hours
#'
#' @return
#' @export
#'
#' @examples
et.hourly<- function(Et.Daily, hours = 6:20) {
      et_hours = length(hours)
      thours = 1:et_hours
      zero_hours = 24 - length(hours)
      morning = min(hours) - 1
      eve = 24 - max(hours)
      morn_hours = numeric(morning)
      eve_hours = numeric(eve)
      Tcycle = length(thours)

      Et.Hourly<- as.vector(rep(0, length(Et.Daily)*Tcycle))
      Tday = as.vector(rep(thours, length(Et.Daily)))
      X<- rep(Tcycle, length(Et.Daily))
      Et.Daily.ext<- rep(Et.Daily, X)

      for(i in 1: length(Et.Hourly)) {
            Et.Hourly[i] = Et.Daily.ext[i]/Tcycle*(1 + sin(2*pi*Tday[i]/Tcycle - pi/2))
      }
      et_hourly = data.frame(hour = rep(hours, length(Et.Daily)), et = Et.Hourly)

      et_24hour = data.frame(hour = rep(0:23, length(Et.Daily)),
                             et = numeric(length(Et.Daily)))
      hour_ind = which(et_24hour$hour %in% et_hourly$hour)
      et_24hour[hour_ind, "et"] = Et.Hourly

      return(et_24hour)
}

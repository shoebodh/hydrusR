#' Runs a test hydrus project on the system
#'
#' @param ...
#'
#' @export
#'

example.h1d<- function(...){
      example_h1d = system.file("examples/example.R", package = "hydrusR")
   source(example_h1d)
}

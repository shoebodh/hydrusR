#' Runs a test hydrus project
#'
#' @export
#'
#' @examples
#'
#' run.H1D.example()
#'

run.H1D.example<- function(){
  example_script = system.file ("examples/example.R", package = "hydrusR")
  source(example_script)
}


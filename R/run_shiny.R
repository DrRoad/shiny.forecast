#' Launch Forecast App in the default browser
#'
#' @examples
#'   view_forecast()
#' }
#' @export

view_forecast <- function() {
     appDir <- system.file("shinyapp", package = "shiny.forecast")
     if (appDir == "") {
          stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
     }
     shiny::runApp(appDir, display.mode = "normal")
}

#' Run Shiny App for Combined Trials
#' @import shiny
#' @export
run_shiny_app <- function() {
    shiny::shinyApp(
        ui = shiny_ui,
        server = shiny_server
    )
}









#' DDshiny wizard
# DDshiny_legacy <- shinyApp(ui, server)

#' DDwizard for simple designs
#'
#' @export
DDwizard <- shinyApp(wizard.ui2, wizard.server2)


#' Shiny applet for family size example
#'
#' @export
DDchildren <-  shinyApp(children.ui, children.server)

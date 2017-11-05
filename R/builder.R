

builder.ui <- material_page(
  title = "Design Builder",
  # nav_bar_color = "red lighten-2",
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  bootstrapLib(),
  
  material_row(
    material_column(
      # width,

      shiny::tags$h1("Input"),
      # shiny::tags$h1("Input2"),
      uiOutput("cards")
    ),
    material_column(
      # width = 4,
      # offset=6,
      shiny::tags$h1("Output")
      # shiny::tags$h1("Output2")
    )
  )
)

builder.server <- function(input, output, clientData, session) {

  output$cards <- renderUI({
    a <- replicate(6, material_card(
      title = "Example Card",
      # depth = 5,
      shiny::tags$h5("Card Content")
    ), simplify = FALSE)
    a
  })
}




DDbuilder <- shinyApp(builder.ui, builder.server)
DDbuilder

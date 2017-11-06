nav_bar_color = "green lighten-3"

builder.ui <- material_page(
  title = "Design Builder",
  nav_bar_color = nav_bar_color,
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  bootstrapLib(),

  material_row(
    material_column(
      width=4,

      material_card("Design Steps",
      # shiny::tags$h1("Input2"),
      shiny::tags$div(style="overflow-y: scroll; height: 500px",
        uiOutput("cards")
      ),
      material_button("add_step", "Add Step"),
      material_button("export", "Export")
      )
    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Design Output",
      bsCollapse(id="outputCollapse", open="Summary",
                 bsCollapsePanel("Summary", "The summary"),
                 bsCollapsePanel("Quick Diagnosis ", "Diagnosis",
                                 paste("Number of simulations: 5"),
                                 paste("Number of draws: 10"))
                 # bsCollapsePanel("Export", "export here")
      )
      )

      # shiny::tags$h1("Output2")
    )
  )
)

builder.server <- function(input, output, clientData, session) {

  output$cards <- renderUI({
    a <- replicate(10, material_card(
      title = "Example Card",
      # depth = 5,
      shiny::tags$h5("Card Content")
    ), simplify = FALSE)
    a
  })
}




DDbuilder <- shinyApp(builder.ui, builder.server)
DDbuilder





welcome <-         material_modal(
  modal_id = "welcome_modal",
  button_text = "Modal",
  title = "Welcome",
  button_color = "red lighten-3",
  # material_dropdown("import_prerolled", "Import from library", c("Two Arm"="", "p Arms"="","Two Way Factorial"="")),
  material_button("import_library", "Import from library..."),
  material_button("import_file", "Import from file..."),
  material_button("import_url", "Import from url...")
)
welcome[[2]][[1]] <- NULL# skip making button
welcome[[2]][[1]]$children[[2]] <- NULL # skip close button in modal dialog
welcome[[2]][[1]]$children[[1]]$attribs$style="box-shadow:none; border:none;"

#' @import shinymaterial
#' @import shinyBS
#'
inspector.ui <- material_page(
  title = "Design Inspector",
  nav_bar_color = nav_bar_color,
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  bootstrapLib(),
  welcome,
  shiny::tags$script("
     $(document).ready(function(){
      $('#welcome_modal').modal('open');
     });
                     "),
  material_row(
    material_column(
      width = 4,
      material_card(
        "Design Parameters",
        material_text_box("d_N", "N:"),
        material_text_box("d_n", "n:"),
        material_text_box("d_p", "p:")
      ),
      material_card(
        "Diagnostic Parameters",
        material_text_box("d_Sims", "Num of Simulations:"),
        material_text_box("d_draws", "Num of Draws (per Simulation):")
        # material_button("RUN", "Run Design")
      ),
      material_button("run", "Run Design"),
      material_button("export", "Export...")
      
    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Output",
      bsCollapse(id="outputCollapse", open="Summary",
        bsCollapsePanel("Summary", "The summary"),
        bsCollapsePanel("Diagnostics", "The diagnostics")
      )
      # shiny::tags$h1("Output2")
      )
    )
  )
)



inspector.server <- function(input, output, clientData, session) {
  
  output$cards <- renderUI({
    a <- replicate(6, material_card(
      title = "Example Card",
      # depth = 5,
      shiny::tags$h5("Card Content")
    ), simplify = FALSE)
    a
  })
}




DDinspector <- shinyApp(inspector.ui, inspector.server)
# DDinspector

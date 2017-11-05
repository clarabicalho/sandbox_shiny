welcome <- shinymaterial:::create_material_object(
  js_file =
    "shiny-material-modal.js",
  material_tag_list =
    shiny::tagList(
      # shiny::tags$div(
      #   class = "fixed-action-btn",
      #   shiny::tags$button(
      #     `data-target` = "welcome-modal",
      #     class = "btn",
      #     style = "",
      #     icon_tag = NULL,
      #       "button_text"
      #   )
      # ),
      shiny::tags$div(
        id = "welcome_modal",
        class = "modal",
        shiny::tags$div(
          class = "modal-content",
          shiny::tags$h4("title"),
          "content"
        ),
        shiny::tags$div(
          class = "modal-footer",
          shiny::tags$a(
            href = "javascript:void(0)",
            class = "modal-action modal-close waves-effect waves-green btn-flat",
            "Close"
          )
        )
      )
    )
)

make_modal_trigger <- function(btn, what) {
  btn[[2]][[1]]$attribs[["data-target"]] <- what; 
  btn[[2]][[1]]$attribs$class <- paste(btn[[2]][[1]]$attribs$class, "modal-trigger")
  btn
}

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
  nav_bar_color = "blue accent-4",
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
        material_text_box("d_p", "p:"),
        material_button("RUN", "Run Design")
      )
    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Output",
      bsCollapse(id="outputCollapse", open="Summary",
        bsCollapsePanel("Summary", "The summary"),
        bsCollapsePanel("Diagnostics", "The diagnostics"),
        bsCollapsePanel("Export", "export here")
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
DDinspector

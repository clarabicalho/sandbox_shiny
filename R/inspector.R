require(shiny); require(shinyBS); require(shinymaterial);


welcome <-         material_modal(
  modal_id = "welcome_modal",
  button_text = "Modal",
  title = "Welcome",
  button_color = "red lighten-3",
  material_button("import_library", "Import from library..."),
  material_button("import_file", "Import from file..."),
  material_button("import_url", "Import from url..."),
  uiOutput(outputId = "import_panel_choice", inline=FALSE)
)
welcome[[2]][[1]] <- NULL# skip making outer button
closebutton <- welcome[[2]][[1]]$children[[2]] # save close button for latter
welcome[[2]][[1]]$children[[2]] <- NULL # skip close button in modal dialog
welcome[[2]][[1]]$children[[1]]$attribs$style="box-shadow:none; border:none;"
welcome[[3]] <-   shiny::tags$script("
     $(document).ready(function(){
      $('#welcome_modal').modal('open');
     });")
welcome[[4]] <- uiOutput("window_closer")

welcome_closer <- shiny::tags$script("
     $(document).ready(function(){
      $('#welcome_modal').modal('close');
     });
                                    ")

importLibrary <- material_card("Import from Library",
  material_radio_button("import_library_dropdown", "Library:",
                    c("Two Arm"="two_arm", "p Arms"="p_arms","Two Way Factorial"="two_fac")
                    ),
  actionButton("import_button", "Import")
)

importFile <- material_card("Import from File",
                            fileInput("import_file1", "Choose RDS File",
                                      accept = ".RDS"
                            ),
                            # uiOutput("import_button", "Import")
                            actionButton("import_button", "Import")
)


importUrl <- material_card("Import from URL",
                           material_text_box("import_url_txt", "URL"),
                           actionButton("import_button", "Import")
)


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
  material_row(
    material_column(
      width = 4,
      uiOutput("designParamaters"),
      material_card(
        "Diagnostic Parameters",
        textInput("d_Sims", "Num of Simulations:", 20),
        textInput("d_draws", "Num of Draws (per Simulation):", 50)
        # material_button("RUN", "Run Design")
      ),
      actionButton("run", "Run Design"),
      actionButton("export", "Export...")

    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Output",
      bsCollapse(id="outputCollapse", open="Summary",
        bsCollapsePanel("Summary", uiOutput("summaryPanel")),
        bsCollapsePanel("Citation", uiOutput("citationPanel")),
        bsCollapsePanel("Diagnostics", uiOutput("diagnosticsPanel")),
        bsCollapsePanel("Code", uiOutput("codePanel")),
        bsCollapsePanel("Simulate", uiOutput("simulationPanel"))
      )
      # shiny::tags$h1("Output2")
      )
    )
  )
)



inspector.server <- function(input, output, clientData, session) {

  DD <-   reactiveValues(design = NULL)


  loadDesign <- function(output, design_fn) {

    f <- names(formals(design_fn))
    v <- as.list(formals(design_fn))

    boxes <- mapply(textInput, paste0("d_", f), paste0(f, ":"),  v, SIMPLIFY = FALSE)

    names(boxes) <- NULL


    output$designParamaters <- renderUI(
      do.call(material_card, c(title="Design Parameters", boxes))
    )
#
#     material_card(
#       "Design Parameters",
#       material_text_box("d_N", "N:"),
#       material_text_box("d_n", "n:"),
#       material_text_box("d_p", "p:")
#     ),

    }

  observeEvent(input$import_library, {
    DD$design <- NULL
    output$import_panel_choice <- renderUI(importLibrary)
  }, ignoreInit = TRUE)
  observeEvent(input$import_url,     {
    DD$design <- NULL
    output$import_panel_choice <- renderUI(importUrl)
  },     ignoreInit = TRUE)
  observeEvent(input$import_file,    {
    DD$design <- NULL
    output$import_panel_choice <- renderUI(importFile)
  },    ignoreInit = TRUE)


  observeEvent(input$import_button, {
    # req(input$import_file_button)
    design <- isolate(DD$design)
    message("***!\n\t", input$import_button, "\n****")
    if(!is.null(design)) {
      output$window_closer <- renderUI(welcome_closer)
      loadDesign(output, design)
    }
  }, ignoreNULL = FALSE)





  observeEvent(input$import_file1, {
    DD$design <- readRDS(input$import_file1$datapath)
    str(DD$design)
    }, ignoreNULL = TRUE)





#
#   output$window_closer <- renderUI({
#     if(isolate(DD$done))
#       welcome_closer
#     })





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

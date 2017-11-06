require(shiny); require(shinyBS); require(shinymaterial); require(DeclareDesign)
round_df <- DDshiny:::round_df

my_tipify <- function(txtbox, tip){
  txtbox[[2]]$class <- paste(txtbox[[2]]$class, "tooltipped")
  txtbox[[2]][["data-tooltip"]] <- tip
  txtbox
}

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
  withMathJax(),
  #TODO This is a super gross way of getting the tooltips to update correctly.
  refresh_tips <- shiny::tags$script("
        setInterval(function(){
              console.log('HIARYLAH');
              $('.tooltipped').tooltip({delay: 50});
        }, 20*1000);
    "),
  welcome,
  material_row(
    material_column(
      width = 4,
      uiOutput("designParamaters"),
      material_card(
        "Diagnostic Parameters",
        my_tipify(textInput("d_sims", "Num of Sims:", 10), "The number of simulated populations are created."),
        my_tipify(textInput("d_draws", "Num of Draws:", 50) ,"The number of samples drawn from each simulation.")
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
        bsCollapsePanel("Summary", verbatimTextOutput("summaryPanel")),
        bsCollapsePanel("Citation", verbatimTextOutput("citationPanel")),
        bsCollapsePanel("Diagnostics", dataTableOutput("diagnosticsPanel")),
        bsCollapsePanel("Code", verbatimTextOutput("codePanel")),
        bsCollapsePanel("Simulate", dataTableOutput("simulationPanel"))
      )
      # shiny::tags$h1("Output2")
      )
    )
  )
)



inspector.server <- function(input, output, clientData, session) {

  DD <-   reactiveValues(design = NULL, design_instance=NULL, diagnosis=NULL)



  output$designParamaters <- renderUI({
  # loadDesign <- function(output, design_fn) {

    design_fn <- req(DD$design)
    f <- names(formals(design_fn))
    v <- as.list(formals(design_fn))

    boxes <- mapply(textInput, paste0("d_", f), paste0(f, ":"),  v, SIMPLIFY = FALSE, USE.NAMES = FALSE)


    if(length(attr(design_fn, "tips")) == length(f)){
      for(i in seq_along(f)){
        boxes[[i]] <- my_tipify(boxes[[i]], attr(design_fn, "tips")[i])
      }
    }



    do.call(material_card, c(title="Design Parameters", boxes))

  })

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
      # loadDesign(output, design)
    }
  }, ignoreNULL = FALSE)





  observeEvent(input$import_file1, {
    DD$design <- readRDS(input$import_file1$datapath)
    str(DD$design)
    }, ignoreNULL = TRUE)



  observeEvent(input$run,{

    design <- DD$design

    DD$args <- list()

    for(n in names(formals(design))){
      DD$args[[n]] <- as.numeric(input[[paste0("d_", n)]])
    }

    message("instantiating design...\n")
    DD$design_instance <- do.call(design, DD$args)

    message("Running diagnosis")
    # browser()
    withProgress(
      DD$diagnosis <- diagnose_design(orig=do.call(DD$design, list()), updated=DD$design_instance, sims = input$d_sims, bootstrap_sims = input$d_draws)
    )
    # bsCollapse(id="outputCollapse", open="Summary",
    #            bsCollapsePanel("Summary", uiOutput("summaryPanel")),
    #            bsCollapsePanel("Citation", uiOutput("citationPanel")),
    #            bsCollapsePanel("Diagnostics", uiOutput("diagnosticsPanel")),
    #            bsCollapsePanel("Code", uiOutput("codePanel")),
    #            bsCollapsePanel("Simulate", uiOutput("simulationPanel"))
    # )
    #

    # browser()
  })


    output$diagnosticsPanel <-    renderDataTable({
      diag_tab <- get_diagnosands(diagnosis = DD$diagnosis)
    # rownames(diag_tab) <- diag_tab$estimand_label
    diag_tab <- round_df(diag_tab, 4)
    diag_tab
    }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

    output$simulationPanel <-    renderDataTable({
      sims_tab <- get_simulations(diagnosis = DD$diagnosis)
      # rownames(diag_tab) <- diag_tab$estimand_label
      sims_tab <- round_df(sims_tab, 4)
      sims_tab
    }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))



    output$citationPanel <- renderPrint(cite_design(DD$design_instance))
    output$summaryPanel <- renderPrint(summary(DD$design_instance))
    output$codePanel    <- renderText({
      paste(deparse(pryr::substitute_q(body(DD$design), DD$args)), collapse="\n")
      })




}




DDinspector <- shinyApp(inspector.ui, inspector.server)
DDinspector

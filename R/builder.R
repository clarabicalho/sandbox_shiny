nav_bar_color = "green lighten-3"

steps_funs <- setNames(c(
    "declare_population",
    "declare_potential_outcomes",
    "declare_sampling",
    "declare_estimand",
    "declare_assignment",
    "reveal_outcomes",
    "declare_estimator"),
    c(
      "Population",
      "Potential outcomes",
      "Sampling",
      "Estimand",
      "Assignment",
      "Reveal outcomes",
      "Estimator"))

steps_labels <- setNames(names(steps_funs), steps_funs)


remove_close_button_from_modal <- function(modal){
  #TODO this is nasty
  closebutton <- modal[[2]][[2]]$children[[2]] # save close button for latter
  modal[[2]][[2]]$children[[2]] <- NULL # skip close button in modal dialog
  modal[[2]][[2]]$children[[1]]$attribs$style="box-shadow:none; border:none;"

  modal
}

builder.ui <- material_page(
  title = "Design Builder",
  nav_bar_color = nav_bar_color,
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  shiny::tags$link(href="https://fonts.googleapis.com/icon?family=Material+Icons", rel="stylesheet"),
  bootstrapLib(),

  material_row(
    material_column(
      width=4,

      material_card("Design Steps",
      # shiny::tags$h1("Input2"),
      shiny::tags$div(style="overflow-y: scroll; height: 500px",
        uiOutput("cards")
      ),
      # material_button("add_step", "Add Step", icon="add_box"),
      # actionButton("add_step", "[+] Add Step"),
      remove_close_button_from_modal(
      material_modal(modal_id="add_step", button_text="Add Step", title="New Step",
                     selectInput("add_type", "Type:", steps_funs),
                     textInput("add_args", "Options"),
                     actionButton("save_add_step", "Save"),
                     actionButton("cancel_add_step", "Cancel"),
                     uiOutput("add_step_closer")
       )),
      tags$br(),
      downloadButton("export", "Export Design"),
      uiOutput("inspectLink")
      )
    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Design Output",
      bsCollapse(id="outputCollapse", open="About",
                 bsCollapsePanel("Summary", "The summary"),
                 bsCollapsePanel("Code", verbatimTextOutput("codePanel"),
                                 downloadButton("download_code", "Export Code...")),
                 bsCollapsePanel("Simulate Data", dataTableOutput("simulationPanel")),
                 bsCollapsePanel("Quick Diagnosis ", "Diagnosis",
                                 paste("Number of simulations: 5"),
                                 paste("Number of draws: 10")),
                 bsCollapsePanel("About", "About DDbuilder...")
                 # bsCollapsePanel("Export", "export here")
      )
      )

      # shiny::tags$h1("Output2")
    )
  )
)

#' @importFrom stringr str_match_all
buildStep <- function(step){


  card <- material_card(title=steps_labels[step$type],
                        shiny::tags$p(step$args),
                        material_modal(modal_id="edit_step", button_text="Edit...", title="Editing",
                                       "HIARYLAH",
                                       actionButton("delete_step", "Delete"))
                        )


  card
}

builder.server <- function(input, output, clientData, session) {

  DD <- reactiveValues(steps=list(list(type='declare_population', args='`N=50`')))



  output$cards <- renderUI({
    # a <- replicate(10, material_card(
    #   title = "Example Card",
    #   # depth = 5,
    #   shiny::tags$h5("Card Content")
    # ), simplify = FALSE)
    # a
    lapply(DD$steps, buildStep)
  })

  output$simulationPanel <-    renderDataTable({
    # sims_tab <- get_simulations(diagnosis = DD$diagnosis)
    sims_tab <- draw_data(DD$design_instance)
    # rownames(diag_tab) <- diag_tab$estimand_label
    sims_tab <- round_df(sims_tab, 4)
    sims_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))


  observeEvent(input$save_add_step,{

    w <- list(type=input$add_type, args=input$add_args)
    DD$steps <- append(DD$steps, list(w))

    message("code.x:\n", code.x(),
            "\n\ncode.pretty", code.pretty(),
            "\n\ncode.body", code.body(),
            "\n\n")

    output$add_step_closer <- renderUI(shiny::tags$script("
     $(document).ready(function(){
      $('#add_step').modal('close');
     });
                                    "))
  })

  observeEvent(input$cancel_add_step, {
    output$add_step_closer <- renderUI(shiny::tags$script("
     $(document).ready(function(){
      $('#add_step').modal('close');
     });
                                    "))
  })

  code.x <- reactive({
    f <- sapply(DD$steps, `[[`, 'type')
    a <- sapply(DD$steps, `[[`, 'args')

    code.x <- paste0("declare_design(\n", paste( sprintf("\t%s(%s)", f, a)   , collapse=",\n") ,"\n)")
    # message("\ncode.x:\n", code.x, '\n\n\n')
    code.x
  })

  ### code panel and download

  output$codePanel <- renderText({
    code.pretty <- gsub('`', '', code.x(), fixed = TRUE)
    # message("\n\ncode.pretty:\n\n", code.pretty, "\n")
    code.pretty
  })

  output$download_code <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".R")
    },
    content = function(file) {
      writeLines(code.pretty(), file)
    })


  ### For constructing template function

  formals.x <- reactive({
    a <- sapply(DD$steps, `[[`, 'args')

    m <- do.call(rbind, str_match_all(a, "`([^` ]+) ?= ?([^ `]+)`"))

    lapply(setNames(m[,3], m[,2]), function(txt) eval(parse(text = txt))  )
  })

  code.body <- reactive({
    gsub("`([^` ]+) ?= ?([^ `]+)`", "\\1", code.x())
    # message("\n\n\ code.body:\n ", code.pretty, "\n")
    # code.pretty

  })



  template.fun <- reactive({
    f <- function() 0
    formals(f) <- formals.x()
    body(f) <- code.body()
    print(f)
    f
  })



  #################


}



#' @export
DDbuilder <- shinyApp(builder.ui, builder.server)
# DDbuilder

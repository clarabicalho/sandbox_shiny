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

pretty_diagnoses <- function(df, digits=4){
  require(reshape2)

  ret <- df[intersect(c('design_ID', 'estimand_label', 'estimator_label'), names(df))]
  names(ret) <- str_replace(str_to_title(names(ret)), "_.*", "")

  ids <- names(ret)

  data_columns <- names(df)
  data_columns <- data_columns[grep('^se[(]|_label$|_ID$', data_columns, invert = TRUE)]

  myfmt <- sprintf('%%.%if', digits)

  for(col in data_columns) {
    title <- str_to_title(str_replace_all(col, '_', ' '))
    x <- sprintf(myfmt, df[[col]])
    secol <- sprintf('se(%s)', col)
    if( secol %in% names(df)) {
      se <- sprintf(myfmt, df[[secol]])
      x <- sprintf(paste('%s(%s)'), x, se)
    }
    ret[[title]] <- x
  }

  ret <- melt(ret, ids, variable.name="Diagnosand")

  if('Design' %in% ids){
    ret <- dcast(ret, ...~Design, value.var = "value")

  }
  ret
}




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
  # handler to receive data from server
  tags$script('
  Shiny.addCustomMessageHandler("closeModal",
        function(name) {
          $(name).modal("close");
        });
  '),

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
                     actionButton("cancel_add_step", "Cancel")
                     # uiOutput("add_step_closer")
       )),
      tags$br(),
      downloadButton("download_design", "Export Design"),
      uiOutput("inspectLink")
      )
    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Design Output",
      bsCollapse(id="outputCollapse", open="About",
                 #bsCollapsePanel("Summary", "The summary"),
                 bsCollapsePanel("Code", verbatimTextOutput("codePanel"),
                                 downloadButton("download_code", "Export Code...")),
                 bsCollapsePanel("Simulate Data", dataTableOutput("simulationPanel")),
                 bsCollapsePanel("Quick Diagnosis ", "Diagnosis",
                                 paste("Number of simulations: 5"),
                                 paste("Number of draws: 10"),
                                 tableOutput("diagnosisPanel")),
                 bsCollapsePanel("About", "About DDbuilder...")
                 # bsCollapsePanel("Export", "export here")
      )
      )

      # shiny::tags$h1("Output2")
    )
  )
)

#' @importFrom stringr str_match_all
buildStep <- function(step,i){
  js="Shiny.onInputChange('%s', %d)"


  card <- material_card(title=steps_labels[step$type],
                        shiny::tags$p(step$args),
                        remove_close_button_from_modal(material_modal(modal_id=paste0("edit_step_",i), button_text="Edit...", title="Editing",
                                       selectInput(sprintf("edit_%d_type", i), "Type:", steps_funs, step$type),
                                       textInput(sprintf("edit_%d_args", i), "Options", step$args),
                                       actionButton(sprintf("edit_%d_save", i), "Save", onclick=sprintf(js, "edit_save", i)),
                                       actionButton(sprintf("edit_%d_cancel", i), "Cancel", onclick=sprintf(js, "edit_cancel", i)),
                                       actionButton(sprintf("edit_%d_delete", i), "Delete", onclick=sprintf(js, "edit_delete", i)),
                                       uiOutput(sprintf("edit_%d_closer", i))
                        ))
  )


  card
}

builder.server <- function(input, output, clientData, session) {

  DD <- reactiveValues(steps=list(list(type='declare_population', args='`N=100`,noise=rnorm(N)')))

  tmpfile <- tempfile()



  output$cards <- renderUI({
    # a <- replicate(10, material_card(
    #   title = "Example Card",
    #   # depth = 5,
    #   shiny::tags$h5("Card Content")
    # ), simplify = FALSE)
    # a
    ret <- list()
    for(i in seq_along(DD$steps)){
      ret[[i]] <- buildStep(DD$steps[[i]], i)
    }

    ret[[length(ret) + 1]] <- tags$script("
      $(document).ready(function(){
        // the href attribute of the modal trigger must specify the modal ID that wants to be triggered
        $('.modal').modal()
      });"
    )
    ret
  })

  output$simulationPanel <-    renderDataTable({
    # sims_tab <- get_simulations(diagnosis = DD$diagnosis)
    sims_tab <- draw_data(design_instance())
    # rownames(diag_tab) <- diag_tab$estimand_label
    sims_tab <- round_df(sims_tab, 4)
    sims_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$diagnosisPanel <-    renderDataTable({
    # sims_tab <- get_simulations(diagnosis = DD$diagnosis)
    sims_tab <- diagnose_design(design_instance(), sims = 5, bootstrap_sims = 5)
    # rownames(diag_tab) <- diag_tab$estimand_label
    sims_tab <- round_df(sims_tab, 4)
    sims_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))



  observeEvent(input$save_add_step,{

    w <- list(type=input$add_type, args=input$add_args)
    DD$steps <- append(DD$steps, list(w))

    # message("code.x:\n", code.x(),
    #         "\n\ncode.pretty", code.pretty(),
    #         "\n\ncode.body", code.body(),
    #         "\n\n")

    session$sendCustomMessage(type = "closeModal", paste0("#", "add_step"))
  })

  observeEvent(input$cancel_add_step, {
    session$sendCustomMessage(type = "closeModal", paste0("#", "add_step"))
  })

  code.x <- reactive({
    f <- sapply(DD$steps, `[[`, 'type')
    a <- sapply(DD$steps, `[[`, 'args')

    code.x <- paste0("declare_design(\n", paste( sprintf("\t%s(%s)", f, a)   , collapse=",\n") ,"\n)")
    # message("\ncode.x:\n", code.x, '\n\n\n')
    code.x
  })

  ### code panel and download

  code.pretty <- reactive({gsub('`', '', code.x(), fixed = TRUE)})

  output$codePanel <- renderText({
    code.pretty()
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
    gsub("`([^` ]+) ?= ?([^ `]+)`", "\\1 = \\1", code.x())
    # message("\n\n\ code.body:\n ", code.pretty, "\n")
    # code.pretty

  })


  ### make template

  template.fun <- reactive({
    f <- function() 0
    formals(f) <- formals.x()
    body(f) <- parse(text=code.body())
    print(f)
    saveRDS(f, tmpfile)
    f
  })

  ## make design instance

  design_instance <- reactive(template.fun()())


  output$download_design <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".RDS")
    },
    content = function(file) {
      # browser()
      saveRDS(design_instance(), file)
    })


  ################# editer observers

  observeEvent(input$edit_cancel,{
    i <- input$edit_cancel

    session$sendCustomMessage(type = "closeModal", sprintf("#edit_%d_closer", i))

  })

  observeEvent(input$edit_delete, {
               i <- input$edit_delete
               session$sendCustomMessage(type = "closeModal", sprintf("#edit_%d_closer", i))

     message("deleting...\n")
     DD$steps[[i]] <- NULL

  })

  observeEvent(input$edit_save, {
     i <- input$edit_save
     w <- list(type=input[[sprintf("edit_%d_type", i)]], args=input[[sprintf("edit_%d_args", i)]])

     session$sendCustomMessage(type = "closeModal", sprintf("#edit_%d_closer", i))

     # output[[]] <- renderUI({
     #             tags$script(sprintf("
     #                $(document).ready(function(){
     #                  $('#edit_step_%d').modal('close');
     #                });", i))
     #           })
     DD$steps[[i]] <- w

  })



  output$inspectLink <- renderUI({

    tags$a(href=paste0("http://localhost:8000/?file=", tmpfile), "Inspector")

  })



}



#' @export
DDbuilder <- shinyApp(builder.ui, builder.server)
# DDbuilder

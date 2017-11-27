





builder.ui <- material_page(
  title = "Declare Design Builder",
  nav_bar_color = nav_bar_color,
  shiny::tags$link(href="https://fonts.googleapis.com/icon?family=Material+Icons", rel="stylesheet"),
  includeCSS(system.file("css/materialize.css", package="DDshiny")),

  bootstrapLib(),
  # handler to receive data from server
  tags$script('
  Shiny.addCustomMessageHandler("openModal",
        function(name) {
          $(name).modal("open");
        });

  Shiny.addCustomMessageHandler("closeModal",
        function(name) {
          $(name).modal("close");
        });
  '),
  uiOutput("editor_modal"),
  material_row(
    material_column(
      width=4,

      material_card("Design Steps",
      shiny::tags$div(style="overflow-y: scroll; height: 500px",
        uiOutput("cards")
      ),
      actionButton("add_step", "[+] Add Step", `data-target`="editor", class="waves-effect waves-light shiny-material-modal-trigger btn"),
      downloadButton("download_design", "Export Design"),
      uiOutput("inspectLink")
      )
    ),
    material_column(
      width = 8,
      material_card("Design Output",
        bsCollapse(id="outputCollapse", open="About",
                 bsCollapsePanel("Code", verbatimTextOutput("codePanel"),
                                 downloadButton("download_code", "Export Code...")),
                 bsCollapsePanel("Simulate Data", dataTableOutput("simulationPanel")),
                 bsCollapsePanel("Quick Diagnosis ",
                                 shiny::tags$p("Number of simulations: 5"),
                                 shiny::tags$p("Number of bootstrap draws: 10"),
                                 tableOutput("diagnosisPanel")),
                 bsCollapsePanel("About DeclareDesign Builder", value="About",
                                 p("This software is in alpha release. Please contact the authors before using in experiments or published work."),
                                 p("This project is generously supported by a grant from the Laura and John Arnold Foundation and seed funding from EGAP.")
                   )
        )
      )

    )
  )
)

#' @importFrom stringr str_match_all
buildStep <- function(step,i){
  js="Shiny.onInputChange('%s', %d)"


  card <- material_card(title=step_obj[[step$type]]$label,
                        shiny::tags$div(step$args),
                        actionButton(sprintf("edit_%i_up", i), "\U25B4", onclick=sprintf(js, "edit_up", i)),
                        actionButton(sprintf("edit_%i_down", i), "\U25BE", onclick=sprintf(js, "edit_down", i)),
                        actionButton(sprintf("edit_%i_del", i), "\U2718", onclick=sprintf(js, "edit_delete", i)),
                        actionButton(sprintf("editor_%i", i), "Edit...", onclick=sprintf(js, "edit_open", i),
                                     `data-target`="editor", class="waves-effect waves-light shiny-material-modal-trigger btn")
                        )


  card
}


builder.server <- function(input, output, clientData, session) {

  library(DeclareDesign)
  library(shinyBS)

  set.seed(20171121) # For Demo TODO

  DD <- reactiveValues(steps=default_builder, editing=-1, add=FALSE)

  tmpfile <- tempfile()

  step_obj <- lapply(setNames(nm=steps_order), function(i) step_obj[[i]])

  output$cards <- renderUI({
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
    sims_tab <- draw_data( DD$design_instance() )
    round_df(sims_tab, 4)
  }, options = list(searching = FALSE, ordering = FALSE, paging = TRUE, pageLength=10, info = FALSE, lengthChange= FALSE))

  output$diagnosisPanel <-    renderTable({
    diagnosands <- get_diagnosands(diagnose_design(DD$design_instance() , sims = 5, bootstrap_sims = 5))
    pretty_diagnoses(diagnosands)
  })




  code.x <- reactive({
    f <- sapply(DD$steps, `[[`, 'type')
    a <- sapply(DD$steps, `[[`, 'args')

    code.x <- sprintf("declare_design(\n%s\n)", paste( sprintf("\t%s(%s)", f, a), collapse=",\n") )
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

  DD$template.fun <- reactive({
    f <- function() 0
    formals(f) <- formals.x()
    body(f) <- parse(text=code.body())
    #print(f)
    saveRDS(f, tmpfile)
    f
  })



  ## make design instance
  DD$design_instance <- reactive( DD$template.fun()() )


  output$download_design <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".RDS")
    },
    content = function(file) {
      # browser()
      saveRDS(DD$template.fun(), file)
    })


  ################# editer observers

  observeEvent(input$add_step,{

    DD$add <- TRUE
    message("Adding step\n")
    session$sendCustomMessage(type = "openModal", "#editor")

  })

  observeEvent(input$edit_open,{

    DD$editing <- input$edit_open
    DD$add <- FALSE

    message("Editing ", DD$editing, "\n")

    step <- DD$steps[[DD$editing]]
    updateSelectInput(session, "edit_type", selected=step$type)
    updateTextInput(session, "edit_args", value=step$args)

    # ))

    session$sendCustomMessage(type = "openModal", "#editor")


  })


  observeEvent(input$edit_cancel,{
    i <- DD$editing

    if(!DD$add) {
      step <- DD$steps[[DD$editing]]
      updateSelectInput(session, "edit_type", selected=step$type)
      updateTextInput(session, "edit_args", value=step$args)
    }


    session$sendCustomMessage(type = "closeModal", "#editor")


  })

  observeEvent(input$edit_delete_btn, {
     i <- DD$editing
     session$sendCustomMessage(type = "closeModal", "#editor")

     message("deleting... ", i, "\n")
     DD$editing <- 1 # reset this here to make sure no `Warning: Error in [[: subscript out of bounds` as editor rerenders in bg
     DD$steps[[i]] <- NULL

  })

  observeEvent(input$edit_save, {
     i <- if(DD$add) length(DD$steps) + 1 else DD$editing
     w <- mk_step(input[["edit_type"]], input[["edit_args"]])

     session$sendCustomMessage(type = "closeModal", "#editor")
     DD$steps[[i]] <- w
  })

  observeEvent(input$edit_up, {
    i <- input$edit_up

    if(i != 1) DD$steps[i - 1:0] <- DD$steps[i - 0:1]

  })

  observeEvent(input$edit_down, {
    i <- input$edit_down
    if(i != length(DD$steps))  DD$steps[i + 0:1] <- DD$steps[i + 1:0]

  })

  observeEvent(input$edit_delete, {
    i <- input$edit_delete
    message("deleting", i)
    DD$steps[i] <- NULL

  })


  output$editor_modal <- renderUI({
    #### Editor dialog
    editor <- remove_close_button_from_modal(
      material_modal(modal_id="editor",
                     button_text="Edit...",
                     title="Editing",
                     uiOutput("step_editor")
      ))
    editor[[2]][[1]] <- NULL# skip making outer button ...
    editor
  })

  # Register server logic for each step
  local(
    for(step in step_obj){
      message("Registering server code for ", step$name)
      step$server(input, output, session)
    }
  )

  output$step_editor <- renderUI({

    message("step_editor enter\n")

    i <- DD$editing
    step <- if(!DD$add) DD$steps[[i]] else mk_step(DECLARE_POPULATION, "")

    message(i, " ", DD$add, " ", step, "\n\n\n")

    steps_labeled <- setNames(names(step_obj), vapply(step_obj, `[[`, NA_character_, "label"))

    material_card("",
      selectInput("edit_type", "Type:", steps_labeled, step$type),
      textInput("edit_args", "Options", step$args),
      uiOutput("step_detail_tabs"),
      actionButton("edit_save", "Save"),
      actionButton("edit_cancel", "Cancel"),
      if(!DD$add)actionButton("edit_delete_btn", "Delete")
    )
  })


  output$step_detail_tabs <- renderUI({
    session$userData[["DD"]] <- DD
    selected_step <- step_obj[[input$edit_type]]
    x <- selected_step$config
    tabsetPanel(selected = 'c',
                tabPanel("Configure", value='c', x),
                tabPanel("Help", value='h', selected_step$help)
    )
  })




  ### For clicking in to inspector

  output$inspectLink <- renderUI({

    tags$a(href=paste0("/?file=", tmpfile), "Inspector", onclick="javascript:event.target.port=8000")

  })



}


make_variable_chooser <- function(id, design_instance, selected) {
  variables <- colnames(draw_data(design_instance))
  selectInput(id, "Variable:", variables, selected)
}


#' @export
DDbuilder <- shinyApp(builder.ui, builder.server)

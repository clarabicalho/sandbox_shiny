
js="Shiny.onInputChange('%s', %d)"


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

step_help_text = list(
  "declare_population" = shiny::tags$div(
    shiny::tags$h5("Declare the Size and Features of the Population"),
    shiny::tags$dl(
      shiny::tags$dt("N"),
      shiny::tags$dd("number of units to draw. If provided as fabricate(N = 5), this determines the number of units in the single-level data. If provided in level, i.e. fabricate(cities = level(N = 5)), N determines the number of units in a specific level of a hierarchical dataset."),
      shiny::tags$dt("ID_label"),
      shiny::tags$dd("(optional) variable name for ID variable, i.e. citizen_ID")
    )
  ),
  "declare_potential_outcomes"=shiny::tags$div(
    shiny::tags$h5("Declare Potential Outcomes"),
    shiny::tags$dl(
      shiny::tags$dt("formula"),
      shiny::tags$dd("eg formula = Y ~ .25 * Z + .01 * age * Z"),
      shiny::tags$dt("assignment_variable_name"),
      shiny::tags$dd("(optional) variable name for Outcomes (Z)"),
      shiny::tags$dt("condition_names"),
      shiny::tags$dd("(optional) conditions the assignment may take")
    )
  ),
  "declare_sampling"=shiny::tags$div(
    shiny::tags$h5("Declare Sampling Procedure"),
    shiny::tags$dl(
      shiny::tags$dt("n"),
      shiny::tags$dd("Use for a design in which n units (or clusters) are sampled. In a stratified design, exactly n units in each stratum will be sampled. (optional)"),
      shiny::tags$dt("prob / simple"),
      shiny::tags$dd("(optional) Take a prob-% fixed-size sample or, if simple is TRUE, a SRS with prob")
    )
  ),
  "declare_estimand"=shiny::tags$div(
    shiny::tags$h5("Declare Estimand"),
    shiny::tags$dl(
      shiny::tags$dt("..."),
      shiny::tags$dd("Named estimands"),
      shiny::tags$dt("subset"),
      shiny::tags$dd("(optional) A subset to calculate the estimand on"),
      shiny::tags$dt("label"),
      shiny::tags$dd("(optional) A label for the estimand if not specified in ...")
    )

  ),
  "declare_assignment"=shiny::tags$div(
    shiny::tags$h5("Declare Assignment"),
    shiny::tags$dl(
      shiny::tags$dt("m"),
      shiny::tags$dd(	"Use for a two-arm design in which m units (or clusters) are assigned to treatment and N-m units (or clusters) are assigned to control. In a blocked design, exactly m units in each block will be treated. (optional)"),
      shiny::tags$dt("m-each"),
      shiny::tags$dd("Use for a multi-arm design in which the values of m_each determine the number of units (or clusters) assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many units (or clusters) should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)"),
      shiny::tags$dt("label"),
      shiny::tags$dd("(optional) A label for the estimand if not specified in ...")
    )
  ),
  "reveal_outcomes"=shiny::tags$div(


  ),
  "declare_estimator"=shiny::tags$div(


  )
  )

#
# step_help_panels <- function(input){
#   out <- shiny::tags$div()
# #  browser()
#
#   for(sf in steps_funs) {
#     condition <- sprintf("input.%s == '%s'", input, sf)
#     # message(condition)
#     cp <- conditionalPanel(
#       condition = condition,
#     )
#     out$children <- append(out$children, list(cp))
#   }
#
#   out
# }

steps_config <- list(
  "declare_population"="Population",
  "declare_potential_outcomes"="",
  "declare_sampling"= shiny::tags$div(

    # material_radio_button("sampling_type", "Sampling Strategy:",
    #              c("Fixed n", "Simple Random Sample")),

    selectInput("sampling_type", "Sampling Type:", c("Complete (n)"="n", "Complete (proportion)"="p", "SRS (probability)"="srs")),
    numericInput("sampling_param", "Param", 0),


    material_checkbox("sampling_strata", "Strata:", FALSE),

    uiOutput("sampling_strata_chooser"),

    material_checkbox("sampling_cluster", "Cluster:", FALSE),
    # verbatimTextOutput("sampling_cluster_text"),

    uiOutput("sampling_cluster_chooser")


  ),
  "declare_estimand"="",
  "declare_assignment"="",
  "reveal_outcomes"="",
  "declare_estimator"="")

#### Editor dialog
editor <- remove_close_button_from_modal(material_modal(modal_id="editor", button_text="Edit...", title="Editing",
                                                        uiOutput("step_editor")
))

# editor[[2]][[1]]$attribs[["style"]] <- "display:inline;" # Yuck #TODO add class, write CSS rule

editor[[2]][[1]] <- NULL# skip making outer button ...

steps_dynamic <- list("declare_sampling"=function(input, output, session, design_instance){
  # browser()

  message("registering callbacks")

  rvs <- reactiveValues(observers=list())

  output$sampling_strata_chooser <- renderUI({
    message("hiarylah");
    if(isTRUE(input$sampling_strata))
      make_variable_chooser("sampling_strata_variable", design_instance, input$sampling_strata_variable)
  })

  # output$sampling_cluster_chooser <- renderUI({message("dfsa[", input$sampling_cluster, "]adfs\n");if(isTRUE(input$sampling_cluster))"foo"})
  output$sampling_cluster_chooser <- renderUI({
    message("dfsa[", input$sampling_cluster, "]adfs\n");
    if(isTRUE(input$sampling_cluster))
      make_variable_chooser("sampling_cluster_variable", design_instance, input$sampling_cluster_variable)
  })

  update_options <- function(input, session){
    options <-
      sprintf("`%s=%s`", switch(input$sampling_type, srs="p", input$sampling_type), input$sampling_param)
    if(isTRUE(input$sampling_type == "sts")) options <- paste(options, ", simple = TRUE")
    if(isTRUE(input$sampling_cluster)) options <- paste(options, ", clust_var =", input$sampling_cluster_variable)
    if(isTRUE(input$sampling_strata)) options <- paste(options, ", strata_var =", input$sampling_strata_variable)

    updateTextInput(session, "edit_args", value=options)
  }


  # observeEvent({
  #   input$sampling_type
  #   input$sampling_param
  #   input$sampling_cluster
  #   input$sampling_block
  #   input$sampling_cluster_variable
  #   input$sampling_block_variable
  # },{
  #  update_options(input, session)
  # })

  # NJF 9/21 Above seems to not work although below does :(
  observeEvent(input$sampling_type, update_options(input, session))
  observeEvent(input$sampling_param, update_options(input, session))
  observeEvent(input$sampling_cluster, update_options(input, session))
  observeEvent(input$sampling_strata, update_options(input, session))
  observeEvent(input$sampling_cluster_variable, update_options(input, session))
  observeEvent(input$sampling_strata_variable, update_options(input, session))



})

builder.ui <- material_page(
  title = "Design Builder",
  nav_bar_color = nav_bar_color,
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  shiny::tags$link(href="https://fonts.googleapis.com/icon?family=Material+Icons", rel="stylesheet"),
  includeCSS(system.file("css/materialize.css", package="DDshiny")),
#  shiny::includeScript(system.file(file.path("js", "shiny-material-checkbox.js"), package = "shinymaterial")),

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
      # shiny::tags$h1("Input2"),
      shiny::tags$div(style="overflow-y: scroll; height: 500px",
        uiOutput("cards")
      ),
      # material_button("add_step", "Add Step"),#, icon="add_box"),
      actionButton("add_step", "[+] Add Step", `data-target`="editor", class="waves-effect waves-light shiny-material-modal-trigger btn"),
      # remove_close_button_from_modal(
      # material_modal(modal_id="add_step", button_text="Add Step", title="New Step",
      #                selectInput("add_type", "Type:", steps_funs),
      #                textInput("add_args", "Options"),
      #                step_help_panels("add_type"),
      #                actionButton("save_add_step", "Save"),
      #                actionButton("cancel_add_step", "Cancel")
      #  )),
      #tags$br(),
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
                 bsCollapsePanel("Quick Diagnosis ",
                                 shiny::tags$p("Number of simulations: 5"),
                                 shiny::tags$p("Number of bootstrap draws: 10"),
                                 tableOutput("diagnosisPanel")),
                 bsCollapsePanel("About",
                                 h5("About the DeclareDesign Inspector"),
                                 p("This software is in alpha release. Please contact the authors before using in experiments or published work."),
                                 p("This project is generously supported by a grant from the Laura and John Arnold Foundation and seed funding from EGAP.")
                 )
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
                        shiny::tags$div(step$args),
                        actionButton(sprintf("edit_%i_up", i), "\U25B4", onclick=sprintf(js, "edit_up", i)),
                        actionButton(sprintf("edit_%i_down", i), "\U25BE", onclick=sprintf(js, "edit_down", i)),
                        actionButton(sprintf("editor_%i", i), "Edit...", onclick=sprintf(js, "edit_open", i),
                                     `data-target`="editor", class="waves-effect waves-light shiny-material-modal-trigger btn")
                        )


  card
}

default_builder <- list(list(type='declare_population', args='`N=100`,noise=rnorm(N)'),
                        list(type='declare_potential_outcomes', args='Y_Z_0=noise, Y_Z_1=noise+1'),
                        list(type='declare_estimand', args='ATE=mean(Y_Z_1 - Y_Z_0), label="ATE"'),
                        list(type='declare_sampling', args='`n=20`'),
                        list(type='declare_assignment', args='`m=10`'),
                        list(type='reveal_outcomes', args=""),
                        list(type='declare_estimator',  args='Y~Z, estimand="ATE"'))

builder.server <- function(input, output, clientData, session) {

  require(DeclareDesign)

  set.seed(20171121) # For Demo TODO

  DD <- reactiveValues(steps=default_builder, editing=-1, add=FALSE)

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
  }, options = list(searching = FALSE, ordering = FALSE, paging = TRUE, pageLength=10, info = FALSE, lengthChange= FALSE))

  output$diagnosisPanel <-    renderTable({
    # sims_tab <- get_simulations(diagnosis = DD$diagnosis)
    diagnosands <- get_diagnosands(diagnose_design(design_instance(), sims = 5, bootstrap_sims = 5))
    # rownames(diag_tab) <- diag_tab$estimand_label
    pretty_diagnoses(diagnosands)
  })



  # observeEvent(input$save_add_step,{
  #
  #   w <- list(type=input$add_type, args=input$add_args)
  #   DD$steps <- append(DD$steps, list(w))
  #
  #   # message("code.x:\n", code.x(),
  #   #         "\n\ncode.pretty", code.pretty(),
  #   #         "\n\ncode.body", code.body(),
  #   #         "\n\n")
  #
  #   session$sendCustomMessage(type = "closeModal", paste0("#", "add_step"))
  # })
  #
  # observeEvent(input$cancel_add_step, {
  #   session$sendCustomMessage(type = "closeModal", paste0("#", "add_step"))
  # })

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
      saveRDS(template.fun(), file)
    })


  ################# editer observers

  observeEvent(input$add_step,{

    DD$add <- TRUE

    message("Adding step ", DD$editing, "\n")

    # step <-DD$steps[[DD$editing]]
    #updateSelectInput(session, "edit_type", selected=steps_funs[1])
    #updateTextInput(session, "edit_args", value="")

    # ))

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

    DD$add <- FALSE

    session$sendCustomMessage(type = "closeModal", "#editor")

    #message("Strata:[", input$sampling_strata, "] T:[", input$sampling_strata_chooser, "]\n")

  })

  observeEvent(input$edit_delete, {
     i <- DD$editing
     session$sendCustomMessage(type = "closeModal", "#editor")

     message("deleting... ", i, "\n")
     DD$steps[[i]] <- NULL

  })

  observeEvent(input$edit_save, {
     i <- if(DD$add) length(DD$steps) + 1 else DD$editing
     w <- list(type=input[["edit_type"]], args=input[["edit_args"]])

     session$sendCustomMessage(type = "closeModal", "#editor")

     DD$steps[[i]] <- w
     DD$steps <- DD$steps #forces refresh
     DD$add <- FALSE
  })

  observeEvent(input$edit_up, {
    i <- input$edit_up

    if(i != 1) DD$steps[i - 1:0] <- DD$steps[i - 0:1]

  })

  observeEvent(input$edit_down, {
    i <- input$edit_down
    if(i != length(DD$steps))  DD$steps[i + 0:1] <- DD$steps[i + 1:0]

  })


  output$step_editor <- renderUI({

    message("step_editor enter\n")

    i <- DD$editing
    step <- if(!DD$add) DD$steps[[i]] else list(type=steps_funs[1], args="")

    message(i, " ", DD$add, " ", step, "\n\n\n")#[", isolate(input$edit_type), "]\n")

    if(step$type %in% names(steps_dynamic)) steps_dynamic[[step$type]](input, output, session, design_instance())

    material_card("",
      selectInput("edit_type", "Type:", steps_funs, step$type),
      textInput("edit_args", "Options", step$args),
      uiOutput("step_detail_tabs"),
      actionButton("edit_save", "Save"),
      actionButton("edit_cancel", "Cancel"),
      if(!DD$add)actionButton("edit_delete", "Delete")
    )
  })


  output$step_detail_tabs <- renderUI({
    x <- steps_config[[input$edit_type]]
    # message(ifelse(steps_config[[input$edit_type]] == "", 'h', 'c'), "\n")
    tabsetPanel(selected = ifelse(identical(x, ""), 'h', 'c'),
                tabPanel("Configure", value='c', x),
                tabPanel("Help", value='h', step_help_text[[input$edit_type]])
    )
  })

  output$editor_modal <- renderUI(editor)



  # output$test_test <- renderUI("test test test")




  ### For clicking in to inspector

  output$inspectLink <- renderUI({

    tags$a(href=paste0("/?file=", tmpfile), "Inspector", onclick="javascript:event.target.port=8000")

  })



  #### step options




}


make_variable_chooser <- function(id, design_instance, selected) {
  variables <- colnames(draw_data(design_instance))
  selectInput(id, "Variable:", variables, selected)
}


#' @export
DDbuilder <- shinyApp(builder.ui, builder.server)
# DDbuilder

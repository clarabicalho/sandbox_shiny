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
  "declare_potential_outcomes"=shiny::tags$div("PO"),
  "declare_sampling"=shiny::tags$div("sampling"),
  "declare_estimand"=shiny::tags$div("estimand"),
  "declare_assignment"=shiny::tags$div("assign"),
  "reveal_outcomes"=shiny::tags$div("reveal"),
  "declare_estimator"=shiny::tags$div("estimator"))


step_help_panels <- function(input){
  out <- shiny::tags$div()
#  browser()

  for(sf in steps_funs) {
    condition <- sprintf("input.%s == '%s'", input, sf)
    # message(condition)
    cp <- conditionalPanel(
      condition = condition,
      step_help_text[[sf]]
      )
    out$children <- append(out$children, list(cp))
  }

  out
}

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

pretty_summary <- function(x) {
  # browser()

  step_summary <- with(x,
                       mapply(pretty_summary_step, seq_along(causal_order_expr), variables_added, variables_modified, quantities_added, causal_order_expr, function_types, N, formulae,
                              SIMPLIFY = FALSE)
  )


  ret <- shiny::tags$div(
    # shiny::tags$header("Design Summary"),



  if (!is.null(x$title)) {
    shiny::tags$p("Study title: ", x$title)
  },

  if (!is.null(x$authors)) {
    shiny::tags$p("Authors: ", x$authors)
  },

  if (!is.null(x$description)) {
    shiny::tags$p(x$description)
  },

  step_summary,


  if (!is.null(x$citation)) {
    shiny::tags$p("Citation:", shiny::tags$br, x$citation)
  }
  )



  ret
}

pretty_summary_step <- function(i, variables_added, variables_modified, quantities_added, causal_order_expr, function_types, N, formulae) {

    step_name <- deparse(causal_order_expr)
    step_class <-
      ifelse(
        function_types != "unknown",
        gsub("_", " ", function_types),
        "custom data modification"
      )


    ret <- material_card(
      paste("Step", i, "(", step_class, "):", step_name),

      if (!is.null(N)) {
        shiny::tags$p(N)
      },

      if (!is.null(formulae)) {
        shiny::tags$p("Formula:", deparse(formula))
      },

      if (!is.null(quantities_added)) {
        if (class(quantities_added) == "data.frame") {
          shiny::tags$p(
            "A single draw of the ", function_types, ":\n",

            HTML(renderTable(quantities_added)())
            )
        } else {
          shiny::tags$p(as.character(quantities_added))
        }
      },

      if (!is.null(variables_added)) {
        lapply(seq_along(variables_added), function(j){
          shiny::tags$p(
            "Added variable:",
            names(variables_added)[j],
            shiny::tags$br(),
            HTML(renderTable(variables_added[[j]])())
            )
        })
      },

      if (!is.null(variables_modified)) {
        lapply(seq_along(variables_modified), function(j){
          shiny::tags$p(
            "Altered variable:",
            names(variables_modified)[j],
            shiny::tags$br(),
            "Before:",
            HTML(renderTable(variables_modified[[j]][["before"]])()),
            shiny::tags$br(),
            "After",
            HTML(renderTable(variables_modified[[j]][["after"]])())
            )
        })
      }

    )


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
                     step_help_panels("add_type"),
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
                                       step_help_panels(sprintf("edit_%d_type", i)),
                                       actionButton(sprintf("edit_%d_save", i), "Save", onclick=sprintf(js, "edit_save", i)),
                                       actionButton(sprintf("edit_%d_cancel", i), "Cancel", onclick=sprintf(js, "edit_cancel", i)),
                                       actionButton(sprintf("edit_%d_delete", i), "Delete", onclick=sprintf(js, "edit_delete", i)),
                                       uiOutput(sprintf("edit_%d_closer", i))
                        ))
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

  DD <- reactiveValues(steps=default_builder)

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
      saveRDS(template.fun(), file)
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

    tags$a(href=paste0("http://:8000/?file=", tmpfile), "Inspector")

  })



}



#' @export
DDbuilder <- shinyApp(builder.ui, builder.server)
# DDbuilder

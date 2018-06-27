# require(shiny); require(shinyBS); require(shinymaterial); require(DeclareDesign)
# round_df <- DDshiny:::round_df

####
####  Welcome page
####

library(DesignLibrary)
# library(shiny)
# library(shinymaterial)
# library(shinythemes)
# library(shinyBS)
# library(ggplot2)

source("R/aaa_helpers.R")

# welcome window ----------------------------------------------------------

welcome <-         material_modal(
  modal_id = "welcome_modal",
  button_text = "Modal",
  title = "Welcome",
  button_color = "red lighten-3",
  # actionButton("import_library", "Import from library..."),
  # actionButton("import_file", "Import from file..."),
  # actionButton("import_url", "Import from url..."),
  # uiOutput("import_panel_choice")
  renderUI(importLibrary)
)


welcome <- remove_close_button_from_modal(welcome)
welcome[[2]][[1]] <- NULL# skip making outer button

welcome[[3]] <-   shiny::tags$script("
       $(document).ready(function(){
        $('#welcome_modal').modal('open', {dismissible: false});
       });")



### Different types of import dialogs
importLibrary <- material_card("Import from DesignLibrary",
                               uiOutput("import_library_ui"),
                               actionButton("import_button", "OK")
)

importFile <- material_card("Import from File",
                            fileInput("import_file1", "Choose RDS File",
                                      accept = ".RDS"
                            ),
                            # uiOutput("import_button", "Import")
                            actionButton("import_button", "OK")
)


importUrl <- material_card("Import from URL",
                           material_text_box("import_url_txt", "URL"),
                           actionButton("import_button", "OK")
)

# diagnostic_params <-       material_card(
#   "Diagnostic Parameters",
#   my_tipify(numericInput("d_sims", "Num of Sims:", 10), "The number of simulated populations are created."),
#   my_tipify(numericInput("d_draws", "Num of Draws:", 50) ,"The number of samples drawn from each simulation."),
#   actionButton("run", "Run Design")
# )

diagnostic_params <-       material_card(
  "Diagnostic Plot",
  my_tipify(numericInput("d_sims", "Num of Sims:", 10), "Diagnosand (y-axis)"),
  my_tipify(numericInput("d_draws", "Num of Draws:", 50) ,"Main design parameter (x-axis)"),
  my_tipify(numericInput("d_draws", "Num of Draws:", 50) ,"Add design parameter (optional layer)"),
  actionButton("run", "Run Design")
)

#' @import shinymaterial
#' @import shinyBS
#'
#'
inspector.ui <- material_page(
  title = "Declare Design Inspector",
  nav_bar_color = nav_bar_color,
  tags$script('
    Shiny.addCustomMessageHandler("closeModal",
          function(name) {
            $(name).modal("close");
          });
    '),
  shiny::tags$title("Declare Design Inspector"),
  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  bootstrapLib(),
  withMathJax(),
  # includeCSS(system.file("inst/css/materialize.css", package="DDshiny")), #NOTE:added folder here

  #TODO This is a super gross way of getting the tooltips to update correctly.
  refresh_tips <- shiny::tags$script("
          setInterval(function(){
                console.log('HIARYLAH');
                $('.tooltipped').tooltip({delay: 50});
          }, 10*1000);
      "),
  uiOutput("welcome"),
  material_row(
    material_column(
      width = 3,
      uiOutput("designParameters")#,
      # uiOutput("plotParameters")
    ),
    material_column(
      width = 6,
      # offset=6,
      material_card("Output",
                    uiOutput("descriptionPanel"),
                    uiOutput("citationPanel"),
                    p("Note: The reults of the design diagnosis are obtained from 500 simulations of the design and 100 bootstrap simulations for the diagnosand estimation."),
                    # verbatimTextOutput("print"),
                    bsCollapse(id="outputCollapse", open="About",
                               # bsCollapsePanel("Citation", uiOutput("citationPanel")),
                               bsCollapsePanel("Summary", uiOutput("summaryPanel")),
                               bsCollapsePanel("Diagnostics", tableOutput("diagnosticsPanel")),
                               bsCollapsePanel("Diagnostic Plot", uiOutput("diagnosticPlot")),
                               bsCollapsePanel("Code", verbatimTextOutput("codePanel"),
                                               downloadButton("download_code", "Export Code...")),
                               bsCollapsePanel("Simulated Data", dataTableOutput("simulationPanel")),
                               bsCollapsePanel("About DeclareDesign Inspector", value="About",
                                               h5("About the DeclareDesign Inspector"),
                                               # p("This software is in alpha release. Please contact the authors before using in experiments or published work."),
                                               p("  This project is generously supported by a grant from the Laura and John Arnold Foundation and seed funding from EGAP.")
                               )
                    )
      )
    ),
    material_column(
      width = 3,
      uiOutput("plotParameters")
      )
    )
  )


inspector.server <- function(input, output, clientData, session) {
  library(DeclareDesign)
  # require(pryr)
  # require(base64enc)
  library(ggplot2)
  library(shinyBS)
  library(stringr)
  library(shinymaterial)

  # session$allowReconnect("force") #TODO


  # create reactive values of DD --------------------------------------------

  DD <-   reactiveValues(design = NULL, design_instance=NULL, diagnosis=NULL, code="",
                         precomputed=FALSE, observers=list(), design_id = NULL)



  # create design parameters from DD ----------------------------------------

  output$designParameters <- renderUI({
    # loadDesign <- function(output, design_fn) {

    design_fn <- req(DD$design)
    v <- get_shiny_arguments(design_fn)
    f <- names(v)
    # f <- names(formals(design_fn))[-length(formals(design_fn))] #remove `code` parameter
    # v <- lapply(f, eval) # here we need to change to read from the attribute

    # boxes <- mapply(textInput, paste0("d_", f), paste0(f, ":"),  v, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    boxes <- list()

    #NOTE: This creates the input values from DD objects.
    #This should only be created for arguments that we chose to pass onto Shiny through the attribute `shiny_args` of the design
    for(i in seq_along(f)){
      fi <- f[i]
      input_id <- paste0("d_", fi)
      input_label <- paste0(fi, ":")
      if(length(v[[i]]) == 1){
        boxes[[i]] <- numericInput(input_id, input_label,  v[[i]])
      } else {
        boxes[[i]] <- selectInput(input_id, input_label, sort(v[[i]]), v[[i]][1])
      }

    }


    if(length(attr(DD$design, "tips")) == length(f)){
      for(i in seq_along(f)){
        boxes[[i]] <- my_tipify(boxes[[i]], attr(DD$design, "tips")[i])
      }
    }


    #NOTE: Here is where we would need to change to take the vignette that is saved/exported by the function called on design (@Jasper)
  #   if(DD$precomputed){
  #     boxes[[length(boxes)+ 1]] <- remove_close_button_from_modal(material_modal("vignette", "Vignette...", title = "", uiOutput("vignette")))
  #     boxes[[length(boxes)]][[2]][[1]]$attribs$style = "display:inline"
  #     boxes[[length(boxes)+ 1]] <-  tags$script(
  #       "$(document).on('change', 'select', function () {
  #       Shiny.onInputChange('run', Math.random());
  #       //Shiny.onInputChange('lastSelectName',name);
  #       // to report changes on the same selectInput
  #       //Shiny.onInputChange('lastSelect', Math.random());
  #   });")
  #     boxes[[length(boxes) + 1]] <- tags$script("
  #                                               $(document).ready(function(){
  #                                               // the href attribute of the modal trigger must specify the modal ID that wants to be triggered
  #                                               $('.modal').modal()
  #                                               });"
  #     )
  #
  # }

    boxes[[length(boxes) + 1]] <- downloadButton("download_design", "Export Design...")


    do.call(material_card, c(title="Design Parameters", boxes))

})

  output$plotParameters <- renderUI({
    design_fn <- req(DD$design)
    v <- get_shiny_arguments(design_fn)
    f <- names(v)
    boxes <- list()

    design_i <- req(DD$design_instance())
    estimators <- unique(paste0(get_estimates(design_i)$estimator_label))

    # diagnosis <- DD$diagnosis_instance()
    # diagnosand_names <- diagnosis$diagnosand_names

    boxes[[1]] <- selectInput("estimator", "Estimator (coefficient)", choices = estimators)
    boxes[[2]] <- uiOutput("coefficient")
    boxes[[3]] <- selectInput("diag_param", "Diagnosand (y-axis)", choices = c("bias", "rmse", "power", "coverage", "mean_se", "type_s_rate", "mean_estimand"))#, diagnosand_names, selected = diagnosand_names[1])
    boxes[[4]] <- selectInput("x_param", "Main parameter (x-axis)",
                              choices = f)
    boxes[[5]] <- selectInput("opt_param", "Add layering parameter (optional)",
                              choices = c("(none)", f))

    tips <- c("Design Estimator",
              "Coefficient",
              "Diagnosand (vertical axis)",
              "Parameter to be placed for the horizontal axis",
              "Parameter used for separate curves")

    for(i in 1:length(boxes)){
      boxes[[i]] <- my_tipify(boxes[[i]], tips[i])
    }

    do.call(material_card, c(title="Plot Parameters", boxes))

  })

  observe(updateSelectInput(session, "opt_param",
                            choices = c("(none)", dplyr::setdiff(names(get_shiny_arguments(DD$design)), input$x_param))))

  output$coefficient <- renderUI({
    design_i <- req(DD$design_instance())
    coefficients <- get_estimates(design_i)$coefficient[get_estimates(design_i)$estimator_label == input$estimator]
    selectInput("coefficient", "Coefficient", choices = coefficients)
  })

  #REVIEW
  output$welcome <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if("file" %in% names(query)){
      fname <- query[["file"]]
      if(file.exists(fname)){
        DD$design <- readRDS(fname)
        message("loaded sidefile")
        return(shiny::tags$script("
                                  console.log('sidefile loaded')
                                  Shiny.onInputChange('import_button', 99999)
                                  "))

      }


    }

    if("topic" %in% names(query)){
      # fname <- file.path(getOption("design.library.path", "~/cache"), paste0(query$topic, ".Rdata"))
      fname <- file.path(getOption("design.library.path", "~/cache"), paste0(query$topic, ".Rdata"))
      if(file.exists(fname)) {
        load(fname, envir = .GlobalEnv)
        DD$precomputed <- TRUE
        DD$design <- designer
        message("loaded topic")
        return(shiny::tags$script("
                                  console.log('topic loaded')
                                  Shiny.onInputChange('import_button', 99999)
                                  "))
      }

      }

    welcome
    })


  output$diagnosticParameters <- renderUI({
    if(!DD$precomputed) diagnostic_params
  })


  # observeEvent(input$import_library, {
  #   DD$design <- NULL
  #   output$import_panel_choice <- renderUI(importLibrary)
  # }, ignoreInit = TRUE)
  # observeEvent(input$import_url,     {
  #   DD$design <- NULL
  #   output$import_panel_choice <- renderUI(importUrl)
  # }, ignoreInit = TRUE)
  # observeEvent(input$import_file,    {
  #   DD$design <- NULL
  #   output$import_panel_choice <- renderUI(importFile)
  # }, ignoreInit = TRUE)


  observeEvent(input$import_button, {
    # req(input$import_file_button)
    design <- isolate(DD$design)
    if(is.character(design)){
      tf <- tempfile()
      download.file(design, tf)
      design <- DD$design <- readRDS(tf)
    }
    message("***!\n\t", input$import_button, "\n****")
    if(!is.null(design)) {
      session$sendCustomMessage(type = "closeModal", "#welcome_modal")
      # loadDesign(output, design)
    }
  }, ignoreNULL = FALSE)
  observeEvent(input$import_file1, {
    DD$design <- readRDS(input$import_file1$datapath)
    DD$precomputed <- FALSE

    # str(DD$design)
  }, ignoreNULL = TRUE)
  observeEvent(input$import_url_txt, {
    DD$design <- input$import_url_txt
    DD$precomputed <- FALSE

    str(DD$design)
  }, ignoreNULL = TRUE)

  #NOTE: HERE WE ARE DRAWING DESIGN LIBRARIES FROM THE DesignLibrary folder (currently local)
  output$import_library_ui <- renderUI({
    # my_design_library_path <- getOption("design.library.path", "~/cache")
    # addResourcePath('datasets', system.file('data', package='DesignLibrary'))
    # my_design_library_path <- "../designs" #NOTE: adapt to final path
    # cached <- dir(paste0(my_design_library_path, "/R"), "_designer[.]R$", full.names = TRUE)
    cached <- str_replace(grep("designer$", ls(as.environment("package:DesignLibrary")), value = TRUE), "_designer", "")
    cached <- intersect(cached, gsub("_shiny_diagnosis.RDS", "", list.files("data", pattern = ".RDS")))
    # cached <- c("simple_two_arm_designer", "regression_discontinuity_designer")
    # cached <- dir(my_design_library_path, "[.]Rdata$", full.names = TRUE)
    names(cached) <- unique(str_to_title(str_replace_all(str_replace(basename(cached), "[.]R$", ""), "_", " ")))
    selectInput("import_library_dropdown", "Library:", cached)
  })

  observeEvent(input$import_library_dropdown,{
    if(paste0(input$import_library_dropdown, "_designer") %in% ls(as.environment("package:DesignLibrary"))){ #NOTE: change this here
      # load(input$import_library_dropdown, envir = .GlobalEnv)
      # design_label <- str_extract(input$import_library_dropdown, pattern = "(?<=R/).*(?=_designer\\.R)")
      # design_label <- input$import_library_dropdown
      #set package environment
      e <- as.environment("package:DesignLibrary")
      DD$design <- get(paste0(input$import_library_dropdown, "_designer"), e)
    }
    DD$precomputed <- TRUE
    diagnosis <- readRDS(paste0("data/", input$import_library_dropdown, "_shiny_diagnosis.RDS"))
    DD$diagnosis <- diagnosis$diagnosis
    DD$args_code <- diagnosis$argument_list
  }, ignoreNULL=TRUE)

  #restrict to diagnosis for the parameters set in shiny `input`
  DD$shiny_args <- reactive({
    args <- list()#formals(DD$design)[-length(formals(DD$design))]
    for(n in intersect(names(formals(DD$design)), sub("d_", "", names(input)))){
      args[[n]] <- as.numeric(input[[paste0("d_", n)]])
    }
    args
  })

  DD$all_args <- reactive({
    args <- formals(DD$design)#formals(DD$design)[-length(formals(DD$design))]
    for(n in intersect(names(formals(DD$design)), sub("d_", "", names(input)))){
      args[[n]] <- as.numeric(input[[paste0("d_", n)]])
    }
    args
  })

  design_id <- reactive({
    if(DD$precomputed){
      shiny_args <- names(get_shiny_arguments(DD$design))
      t <- c()
      for(n in shiny_args){
        v <- which(DD$diagnosis$diagnosands[[n]] == as.numeric(input[[paste0("d_", n)]]))
        v <- DD$diagnosis$diagnosands$design_ID[v]
        t <- c(t, v)
      }
      Mode(t)
    }
  })

  diagnosis_instance <- reactive({
    diag <- lapply(DD$diagnosis, function(o){
      o[o$design_ID == design_id(),]
    })
    if(DD$precomputed) diag$diagnosands <- diag$diagnosands[,!names(diag$diagnosands) %in% names(get_shiny_arguments(DD$design))]
    return(diag)
  })


  args_code <- reactive({
    DD$args_code[[design_id()]]
  })

  # message("instantiating design...\n")
  # # if(exists("DEBUG", globalenv())) browser()

  DD$design_instance <- reactive({
    tryCatch(do.call(DD$design, DD$shiny_args()), error=function(e) 9999999)
    # if(identical(DD$design_instance, 9999999)) {
    # DD$diagnosis <- NULL
    #   # return() # bail out
    # # }
  })

  #
  # DD$args_code <- diagnosis$argument_list
  # DD$precomputed <- TRUE
  # }



  output$print <- renderText(capture.output(str(DD$design_instance())))

  # observeEvent({
  #   input$run;
  #   input$import_button
  # },{
  #   do_run()
  # })
  #
  # do_run <- function() {
  #   message("Run Button Clicked;\n")
  #
  #   design <- DD$design
  #
  #   DD$args <- list() #NOTE:uncomment
  #   # DD$args <- formals(design)
  #
  #   # browser()
  #   for(n in names(formals(design))){
  #     DD$args[[n]] <- as.numeric(input[[paste0("d_", n)]])
  #   }
  #
  #
  #   message("instantiating design...\n")
  #   if(exists("DEBUG", globalenv())) browser()
  #   DD$design_instance <- tryCatch(do.call(design, attr(DD$design, "shiny_arguments")), error=function(e) 9999999)
  #   if(identical(DD$design_instance, 9999999)) {
  #     DD$diagnosis <- NULL
  #     return() # bail out
  #   }
  #
  #   if(!is.null(attr(DD$design_instance, "diagnosis"))){
  #     DD$diagnosis <- attr(DD$design_instance, "diagnosis")
  #     return()
  #   }
  #
  #   message("Running diagnosis")
  #   # browser()
  #   withProgress(
  #     DD$diagnosis <- diagnose_design(original_design=do.call(DD$design, list()),
  #                                     updated_design=DD$design_instance,
  #                                     sims = as.numeric(input$d_sims),
  #                                     bootstrap = as.numeric(input$d_draws))
  #   )
  #
  #   # browser()
  # }
  #

  output$diagnosticsPanel <-    renderTable({
    # message(Sys.time(), "a")
    diag_tab <- get_diagnosands(diagnosis = diagnosis_instance())
    if(DD$precomputed){
      diag_tab <- dplyr::select(diag_tab, -design_ID)
    }
    # rownames(diag_tab) <- diag_tab$estimand_label
    # diag_tab <- round_df(diag_tab, 4)
    # diag_tab
    # message(Sys.time(), "b")
    # on.exit(message(Sys.time(), "c"))
    pretty_diagnoses(diag_tab)
    # as.data.frame(design_id())
  })

  # NOTE: need to index simulations dependent on parameters chosen in each input$d_`arg`.
  # create design_ID index
  # can take design_ID index from the DD$diagnosis (and the columns that align with the parameters)

  # observeEvent(input$import_library_dropdown,{
  #   names <- names(attr(DD$design, "shiny_arguments"))
  #   # design_id <- unique(DD$diagnosis$diagnosands$design_ID[DD$diagnosis$diagnosands[i]])
  #   design_id <- which(
  #     rowSums(sapply(names, FUN = function(i){
  #       DD$diagnosis$diagnosands[i] == input[paste0("d_", i)][[1]]
  #     })) == length(names))
  #
  # }, ignoreNULL=TRUE)

  # sims <- observeEvent()
  #
  # sims <- reactive({
  #   sims <- get_simulations(DD$diagnosis)
  #
  # })


  output$diagnosticsPlot <- renderPlot({
    # message(Sys.time(), "a")

    sims <- get_simulations(diagnosis_instance())
    if("design_ID" %in% names(sims)) sims <- subset(sims, design_ID != "original_design")

    # observeEvent(input$import_library) sims <- subset(sims, design_ID != "original_design")
    # if("design_ID" %in% names(sims)) sims <- subset(sims, design_ID != "original_design")
    sims$covered <- factor(1 + (sims$ci_lower < sims$estimand & sims$estimand < sims$ci_upper), 1:2,
                           labels = c("Estimand not covered by confidence interval", "Estimand covered by confidence interval"))
    sims$estimator_label <- as.factor(sims$estimator_label)
    sims$estimand_label <- as.factor(sims$estimand_label)

    # message(Sys.time(), "b")

    # lowest <- min(sims$est)
    # highest <- max(sims$est)
    # browser()
    # on.exit(message(Sys.time(), "exit"))

    g <- ggplot(sims) + aes(x=est) +
      # geom_density(aes(x=est, y=  (..count.. - min(..count..))/ (max(..count..) - min(..count..)) *   min(x),
      #                  fill=covered, group=covered), alpha=.4, position='stack', color=NA) +
      geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper, color=covered), alpha=.4) +
      # geom_point(aes(y=est), size=.5) +
      # geom_point(aes(y=estimand, col=black), alpha=.8) +
      geom_hline(aes(yintercept=mean(estimand))) +
      geom_text(aes(x=x, y=y, label=label),
                data=function(df){
                  data.frame(x=min(df$est),
                             y=mean(df$estimand),
                             label=sprintf('  Avg Estimand:\n  %4.3f', mean(df$estimand)),
                             stringsAsFactors = FALSE)
                }, hjust='left') +
      facet_wrap(estimand_label~estimator_label) + # this is issue
      ylab("Estimate") +
      scale_x_continuous(labels=NULL, breaks = NULL, name='') +
      scale_color_discrete(drop=FALSE, name = '') +
      # scale_fill_discrete(guide=FALSE)+
      # coord_fixed() +
      coord_flip() +
      dd_theme()

    # message(Sys.time(), "c")


    print(g)
  })

  output$diagnosticPlot <- renderUI({
    plotOutput("user_defined_plot")
  })

  output$user_defined_plot <- renderPlot({

    # browser()
    # N_formal <- eval(formals(DD$design)$N)
    N_formal <- get_shiny_arguments(DD$design)$N

    args <- DD$shiny_args

    plotdf <- NULL

    # browser()
    if(DD$precomputed){
      plotdf <- get_diagnosands(DD$diagnosis)

      #restrict to cases where all other parameters match input
      fix_arg <- names(get_shiny_arguments(DD$design))[!names(get_shiny_arguments(DD$design)) %in% c(input$x_param, input$opt_param)]

      for(col in fix_arg){
        plotdf <- plotdf[plotdf[[col]]==input[[paste0("d_",col)]],]
      }

      #further restrict to estimator chosen
      estimator <- input$estimator #trimws(gsub(".*?[)]$", "", input$estimator), which = "both")
      coefficient <- input$coefficient #regmatches(input$estimator, gregexpr("(?<=\\().*?(?=\\))", input$estimator, perl=T))[[1]][1]

      plotdf <- plotdf[plotdf$estimator_label == estimator &
                         plotdf$coefficient == coefficient,]

    }else{
      for(N in N_formal){
        args$N <- N
        d <- tryCatch(do.call(DD$design, args), error=function(e) NULL)
        if(is.null(d)) next;
        diag <- get_diagnosands(diagnoser(d))
        diag$N <- N
        plotdf <- rbind.data.frame(plotdf, diag, stringsAsFactors = FALSE)
      }
    }

    # plotdf$estimator_label <- paste("Power of", plotdf$estimator_label)

    if(input$import_library_dropdown %in% "mediation_analysis"){
      plotdf$estimator_label <- paste0(plotdf$estimator_label, " (", plotdf$coefficient, ")")
    }

    plotdf$diagnosand <- plotdf[[input$diag_param]]
    plotdf$diagnosand_min <- plotdf[[input$diag_param]] - 1.96*plotdf[[paste0("se(", input$diag_param, ")")]]
    plotdf$diagnosand_max <- plotdf[[input$diag_param]] + 1.96*plotdf[[paste0("se(", input$diag_param, ")")]]
    plotdf$x_param <- plotdf[[input$x_param]]
    ifelse(input$opt_param != "(none)", plotdf$opt_param <- as.factor(plotdf[[input$opt_param]]), plotdf$opt_param <- NA)

    if(input$opt_param != "(none)"){
      p <- ggplot(plotdf) +
        aes(x=x_param, y=diagnosand, ymin=diagnosand_min, ymax=diagnosand_max,
            group=opt_param, color=opt_param, fill=opt_param)

    }else{
      p <- ggplot(plotdf) +
        aes(x=x_param, y=diagnosand, ymin=diagnosand_min, ymax=diagnosand_max)
    }

    p <- p +
      geom_line() +
      geom_point() +
      geom_ribbon(alpha=.3) +
      # scale_color_discrete() +
      scale_y_continuous(name=input$diag_param) + #, limits=0:1, breaks=0:4/4, minor_breaks = 0:10/10) +
      dd_theme() +  labs(fill=input$opt_param,color=input$opt_param, x = input$x_param)

    p

  })


  output$simulationPanel <-    renderDataTable({
    # sims_tab <- get_simulations(diagnosis = DD$diagnosis)
    sims_tab <- draw_data(DD$design_instance())
    # rownames(diag_tab) <- diag_tab$estimand_label
    sims_tab <- round_df(sims_tab, 4)
    sims_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = TRUE, pageLength=10, info = FALSE, lengthChange= FALSE))

  DD$code <- reactive({
    if(!is.null(attr(DD$design_instance(), "code"))){
      code <- attr(DD$design_instance(), "code")
      paste(code, collapse = "\n")
    } else if(requireNamespace("pryr")){
        code <- paste(deparse(pryr::substitute_q(body(DD$design), DD$all_args())), collapse="\n")
        gsub("[{]\n|\n[}]|[}]\n]", "", code) # remove surounding curly
      }
  })

  output$descriptionPanel <- renderUI(HTML(attr(DD$design, "description")))

  output$citationPanel <- renderUI(
    # HTML(format(get_author(paste0(input$import_library_dropdown, "_designer")), style="html"))
    HTML("Author: DeclareDesign Team")
  )

  output$summaryPanel  <- renderUI({
    # pretty_summary(
    # lapply(capture.output(summary(DD$design_instance())), tags$p)
    #)

    pretty_summary(summary(DD$design_instance()))

  })

  output$codePanel     <- renderText(DD$code())


  # NOTE: These two option are from function in DDtools that export the design instance and code for specific parameters

  output$download_design <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".RDS")
    },
    content = function(file) {
      saveRDS(DD$design_instance(), file)
    }
  )

  output$download_code <- downloadHandler(
    filename=function() {
      paste0("design-", Sys.Date(), ".R")
    },
    content = function(file) {
      writeLines(DD$code(), file)
    }
  )

  #here we need to change to read an Rmd files from the vignettes folder (check topics)
  # output$vignette <- renderUI({
  #   if(!requireNamespace("base64enc")) return()
  #   # vig <- vignette(topic) # topic gets loaded to global environment via design library
  #   vig <- vignette(input$import_library_dropdown) # topic gets loaded to global environment via design library
  #   vightml <- base64enc::base64encode(file.path(vig$Dir, "doc", vig$PDF))
  #   vightml <- sprintf('<iframe src="data:text/html;base64,%s" height="500px" width="100%%" frameborder=0 />', vightml   )
  #   HTML(vightml)
  # })

    }


#' @export
DDinspector <- shinyApp(inspector.ui, inspector.server)




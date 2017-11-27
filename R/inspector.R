# require(shiny); require(shinyBS); require(shinymaterial); require(DeclareDesign)
# round_df <- DDshiny:::round_df


####
####  Welcome page
####

welcome <-         material_modal(
  modal_id = "welcome_modal",
  button_text = "Modal",
  title = "Welcome",
  button_color = "red lighten-3",
  actionButton("import_library", "Import from library..."),
  actionButton("import_file", "Import from file..."),
  actionButton("import_url", "Import from url..."),
  uiOutput("import_panel_choice")
)


welcome <- remove_close_button_from_modal(welcome)
welcome[[2]][[1]] <- NULL# skip making outer button

welcome[[3]] <-   shiny::tags$script("
     $(document).ready(function(){
      $('#welcome_modal').modal('open', {dismissible: false});
     });")



 ### Different types of import dialogs
importLibrary <- material_card("Import from Library",
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

diagnostic_params <-       material_card(
  "Diagnostic Parameters",
  my_tipify(numericInput("d_sims", "Num of Sims:", 10), "The number of simulated populations are created."),
  my_tipify(numericInput("d_draws", "Num of Draws:", 50) ,"The number of samples drawn from each simulation."),
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

  # background_color = "blue lighten-4",
  # shiny::tags$h1("Page Content"),
  bootstrapLib(),
  withMathJax(),
  includeCSS(system.file("css/materialize.css", package="DDshiny")),

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
      width = 4,
      uiOutput("designParameters"),
      uiOutput("diagnosticParameters")
    ),
    material_column(
      width = 8,
      # offset=6,
      material_card("Output",
      uiOutput("descriptionPanel"),
      bsCollapse(id="outputCollapse", open="About",
        bsCollapsePanel("Summary", uiOutput("summaryPanel")),
        bsCollapsePanel("Citation", verbatimTextOutput("citationPanel")),
        bsCollapsePanel("Diagnostics", tableOutput("diagnosticsPanel") , plotOutput("diagnosticsPlot")),
        bsCollapsePanel("Power", uiOutput("powerPanel")),
        bsCollapsePanel("Code", verbatimTextOutput("codePanel"),
                        downloadButton("download_code", "Export Code...")),
        bsCollapsePanel("Simulated Data", dataTableOutput("simulationPanel")),
        bsCollapsePanel("About DeclareDesign Inspector", value="About",
                        # h5("About the DeclareDesign Inspector"),
                        p("This software is in alpha release. Please contact the authors before using in experiments or published work."),
                        p("  This project is generously supported by a grant from the Laura and John Arnold Foundation and seed funding from EGAP.")
        )

      )
      # shiny::tags$h1("Output2")
      )
    )
  )
)



inspector.server <- function(input, output, clientData, session) {
  library(DeclareDesign)
  # require(pryr)
  # require(base64enc)
  library(ggplot2)


  DD <-   reactiveValues(design = NULL, design_instance=NULL, diagnosis=NULL, code="",
                         precomputed=FALSE, observers=list())



  output$designParameters <- renderUI({
  # loadDesign <- function(output, design_fn) {

    design_fn <- req(DD$design)
    f <- names(formals(design_fn))
    v <- lapply(as.list(formals(design_fn)), eval)

    # boxes <- mapply(textInput, paste0("d_", f), paste0(f, ":"),  v, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    boxes <- list()

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


    if(length(attr(design_fn, "tips")) == length(f)){
      for(i in seq_along(f)){
        boxes[[i]] <- my_tipify(boxes[[i]], attr(design_fn, "tips")[i])
      }
    }



    if(DD$precomputed){
      boxes[[length(boxes)+ 1]] <- remove_close_button_from_modal( material_modal("vignette", "Vignette...", title = "", uiOutput("vignette")))
      boxes[[length(boxes)+ 1]] <-  tags$script(
           "$(document).on('change', 'select', function () {
                Shiny.onInputChange('run', Math.random());
                //Shiny.onInputChange('lastSelectName',name);
                // to report changes on the same selectInput
                //Shiny.onInputChange('lastSelect', Math.random());
                });")
      boxes[[length(boxes) + 1]] <- tags$script("
      $(document).ready(function(){
        // the href attribute of the modal trigger must specify the modal ID that wants to be triggered
        $('.modal').modal()
      });"
      )


    }

    boxes[[length(boxes) + 1]] <- downloadButton("download_design", "Export Design...")


    do.call(material_card, c(title="Design Parameters", boxes))

  })

  output$welcome <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if("file" %in% names(query)){
      fname <- query[["file"]]
      if(file.exists(fname)){
        DD$design <- readRDS(fname)
        return(NULL)
      }


    }

    if("topic" %in% names(query)){
      fname <- file.path(getOption("design.library.path", "~/cache"), paste0(query$topic, ".Rdata"))
      if(file.exists(fname)) {
        load(fname, envir = .GlobalEnv)
        DD$precomputed <- TRUE
        DD$design <- designer
        return(NULL)
      }

    }


    welcome
  })


  output$diagnosticParameters <- renderUI({
    if(!DD$precomputed) diagnostic_params;
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

  observeEvent(input$import_library_dropdown,{
    if(file.exists(input$import_library_dropdown)){
      load(input$import_library_dropdown, envir = .GlobalEnv)
      DD$precomputed <- TRUE
      DD$design <- designer
    }

  }, ignoreNULL=TRUE)




  observeEvent({
    input$run;
    input$import_button
    },{

    message("Run Button Clicked;\n")

    design <- DD$design

    DD$args <- list()

    # browser()
    for(n in names(formals(design))){
      DD$args[[n]] <- as.numeric(input[[paste0("d_", n)]])
    }

    message("instantiating design...\n")
    if(exists("DEBUG", globalenv())) browser()
    DD$design_instance <- tryCatch(do.call(design, DD$args), error=function(e) 9999999)
    if(identical(DD$design_instance, 9999999)) {
      DD$diagnosis <- NULL
      return() # bail out
    }

    if(!is.null(attr(DD$design_instance, "diagnosis"))){
      DD$diagnosis <- attr(DD$design_instance, "diagnosis")
      return()
    }

    message("Running diagnosis")
    # browser()
    withProgress(
      DD$diagnosis <- diagnose_design(original_design=do.call(DD$design, list()),
                                      updated_design=DD$design_instance,
                                      sims = as.numeric(input$d_sims),
                                      bootstrap_sims = as.numeric(input$d_draws))
    )

    # browser()
  })


    output$diagnosticsPanel <-    renderTable({
      # message(Sys.time(), "a")
      diag_tab <- get_diagnosands(diagnosis = DD$diagnosis)
    # # rownames(diag_tab) <- diag_tab$estimand_label
    # diag_tab <- round_df(diag_tab, 4)
    # diag_tab
      # message(Sys.time(), "b")
      # on.exit(message(Sys.time(), "c"))
      pretty_diagnoses(diag_tab)
    })

    output$diagnosticsPlot <- renderPlot({
      # message(Sys.time(), "a")
      sims <- get_simulations(DD$diagnosis)
      if("design_ID" %in% names(sims)) sims <- subset(sims, design_ID != "original_design")
      sims$covered <- factor(1 + (sims$ci_lower < sims$estimand & sims$estimand < sims$ci_upper), 1:2, labels = c("OOB", "COV"))
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
        facet_wrap(estimand_label~estimator_label) +
        ylab("Estimate") +
        scale_x_continuous(labels=NULL, breaks = NULL, name='') +
        scale_color_discrete(drop=FALSE, guide=FALSE) +
        # scale_fill_discrete(guide=FALSE)+
        # coord_fixed() +
        coord_flip() +
        dd_theme()

      # message(Sys.time(), "c")


        print(g)
    })

    output$powerPanel <- renderUI({
      if(!DD$precomputed) return()
      plotOutput("powerPlot")
    })
    output$powerPlot <- renderPlot({

      # browser()
      N_formal <- eval(formals(DD$design)$N)

      args <- DD$args

      powerdf <- NULL

      # browser()

      for(N in N_formal){
        args$N <- N
        d <- tryCatch(do.call(DD$design, args), error=function(e) NULL)
        if(is.null(d)) next;
        diag <- get_diagnosands(diagnoser(d))
        diag$N <- N
        powerdf <- rbind.data.frame(powerdf, diag, stringsAsFactors = FALSE)
      }


      ggplot(powerdf) + aes(x=N, y=power, ymin=power-`se(power)`, ymax=power+`se(power)`,
                            group=estimator_label, color=estimator_label, fill=estimator_label) +
        geom_line() +
        geom_point() +
        geom_ribbon(alpha=.3) +
        scale_y_continuous(name="Power of Design", limits=0:1, breaks=0:4/4, minor_breaks = 0:10/10) +
        dd_theme() + facet_grid(~estimand_label) #+ scale_fill_discrete(guide=FALSE)



    })


    output$simulationPanel <-    renderDataTable({
      # sims_tab <- get_simulations(diagnosis = DD$diagnosis)
      sims_tab <- draw_data(DD$design_instance)
      # rownames(diag_tab) <- diag_tab$estimand_label
      sims_tab <- round_df(sims_tab, 4)
      sims_tab
    }, options = list(searching = FALSE, ordering = FALSE, paging = TRUE, pageLength=10, info = FALSE, lengthChange= FALSE))

    DD$code <- reactive({
      if(!is.null(attr(DD$design_instance, "code"))){
        attr(DD$design_instance, "code")
      } else if(requireNamespace("pryr")){
        code <- paste(deparse(pryr::substitute_q(body(DD$design), DD$args)), collapse="\n")
        gsub("[{]\n|\n[}]", "", code) # remove surounding curly
      }
    })

    output$descriptionPanel <- renderUI(HTML(attr(DD$design, "description")))


    output$citationPanel <- renderPrint(cite_design(DD$design_instance))

    output$summaryPanel  <- renderUI({
      pretty_summary(summary(DD$design_instance))
    })

    output$codePanel     <- renderText(DD$code())


    output$download_design <- downloadHandler(
      filename=function() {
        paste0("design-", Sys.Date(), ".RDS")
      },
      content = function(file) {
        saveRDS(DD$design_instance, file)
      })

    output$download_code <- downloadHandler(
      filename=function() {
        paste0("design-", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(DD$code(), file)
      })


    output$vignette <- renderUI({
      if(!requireNamespace("base64enc")) return()
      vig <- vignette(topic) # topic gets loaded to global environment via design library
      vightml <- base64enc::base64encode(file.path(vig$Dir, "doc", vig$PDF))
      vightml <- sprintf('<iframe src="data:text/html;base64,%s" height="500px" width="100%%" frameborder=0 />', vightml   )
      HTML(vightml)
    })

    output$import_library_ui <- renderUI({
            my_design_library_path <- getOption("design.library.path", "~/cache")
            cached <- dir(my_design_library_path, "[.]Rdata$", full.names = TRUE)
            names(cached)   <-  str_to_title(str_replace_all(str_replace(basename(cached), "[.]Rdata$", ""), "_", " "))

            selectInput("import_library_dropdown", "Library:", cached)
    })

}


#' @export
DDinspector <- shinyApp(inspector.ui, inspector.server)


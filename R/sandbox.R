
library(DeclareDesign)
library(shiny)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(ggplot2)
library(rlang)
library(DT)
library(rlang)
library(dplyr)
library(plyr)

source("R/sandbox_fns.R")
info <- c("step_type", "label", "description", "expects")
sandbox_df <- do.call(rbind.fill, lapply(sandbox, function(i)
  as.data.frame(t(unlist(sapply(info, function(a) attr(i, a)[1]))))))

step_types <- as.character(sandbox_df[["step_type"]])
step_choices <- as.character(sandbox_df[["description"]])

# sandbox_df$step_type <- str_split_fixed(sandbox_df$step_type, "_", n = 2)[,1]

# design <- population_1(10) + NULL

sandbox.ui <- material_page(
  title = "DeclareDesign Sandbox",
  material_row(
    material_column(
      width = 3,
      material_card(
        selectInput("nsteps", "Number of steps", choice = 1L:12L,
                    selected = 2L),
        textInput("design_name", "Design Name", "my_design")
      ),
    uiOutput("steps"),
    uiOutput("param")
    ),
    material_column(
      width = 8,
      material_card("Code",
                    verbatimTextOutput("codePanel")
                    #downloadButton("download_code", "Export Code...")
        )
    )
  )
)

sandbox.server <- function(input, output, session){

  core <- reactive(input$nsteps)

  steps_chosen <- reactive({
    steps <- c()
    for(i in 1:core()){
      stepi <- as.character(sandbox_df$label[sandbox_df$description == input[[paste0("step_id_", i)]]])
      steps <- c(steps, stepi)
    }
    steps
  })

  output$param <- renderUI({
    param <- list()
    for(i in 1:core()){
      f <- names(formals(get(steps_chosen()[i])))
      f <- setdiff(f, "data")

      if(length(f) != 0){
        p <- list()
        for(j in seq_along(f)){
          fj <- f[j]
          param_id <- paste0("s", i, "_param", j)
          param_label <- paste0(fj, " (Step ", i, "):")
          p[[j]] <- numericInput(param_id, param_label, value = NULL)
        }
      }else{
        p <- NULL
      }
      param[[i]] <- p
    }
    if(length(param) == 0){
      NULL
    }else{
      do.call(material_card, c(title = "Parameters", do.call(list, unlist(param, recursive=FALSE))))
    }
  })

  output$steps <- renderUI({
    boxes <- list()
    for(i in 1:core()){
      j <- i
      type_id <- paste0("type_id_", i)
      type_label <- paste0("Step ", i, " Type")
      step_id <- paste0("step_id_", i)
      step_label <- paste0("Step ", i)
      u <- length(unique(step_types))
      ifelse(i%%u == 0, a <- u, a <- i%%u)
      step_choices_i <- step_choices[step_types == unique(step_types)[a]]
      step_labels_i <- as.character(sandbox_df$label[sandbox_df$step_type == unique(step_types)[a]])
      f <- names(formals(get(step_labels_i[1])))

      f <- setdiff(f, "data")

      boxes[[i]] <- list(
        selectInput(type_id, type_label, choices = unique(step_types), selected = unique(step_types)[a]),
        selectInput(step_id, step_label, choices = step_choices[step_types == unique(step_types)[a]])
        )
    }
    boxes <- do.call(list, unlist(boxes, recursive=FALSE))
    # boxes[[length(boxes)+1]] <- actionButton("go", "Go")

    do.call(material_card, c(title = "Steps", boxes))
  })

    observe({
      updateSelectInput(session, "step_id_1", choices = step_choices[step_types == input[["type_id_1"]]])
      updateSelectInput(session, "step_id_2", choices = step_choices[step_types == input[["type_id_2"]]])
      updateSelectInput(session, "step_id_3", choices = step_choices[step_types == input[["type_id_3"]]])
      updateSelectInput(session, "step_id_4", choices = step_choices[step_types == input[["type_id_4"]]])
      updateSelectInput(session, "step_id_5", choices = step_choices[step_types == input[["type_id_5"]]])
      updateSelectInput(session, "step_id_6", choices = step_choices[step_types == input[["type_id_6"]]])
      updateSelectInput(session, "step_id_7", choices = step_choices[step_types == input[["type_id_7"]]])
      updateSelectInput(session, "step_id_8", choices = step_choices[step_types == input[["type_id_8"]]])
      updateSelectInput(session, "step_id_9", choices = step_choices[step_types == input[["type_id_9"]]])
      updateSelectInput(session, "step_id_10", choices = step_choices[step_types == input[["type_id_10"]]])
      updateSelectInput(session, "step_id_11", choices = step_choices[step_types == input[["type_id_11"]]])
      updateSelectInput(session, "step_id_12", choices = step_choices[step_types == input[["type_id_12"]]])
    })

  design_code <- reactive({
    design_code <- list()
    formal_names <- list()
    for(i in 1:core()){

      f <- get(steps_chosen()[i])
      formal_names <- names(formals(f))
      formal_names <- setdiff(formal_names, "data")
      step_inputs <- sapply(1:length(formal_names), function(j) quos(!!input[[paste0("s", i, "_param",j)]]))
      names(step_inputs) <- formal_names

      if(length(formal_names)!=0){
        design_code[[i]] <- paste0(steps_chosen()[i], " <- ", capture.output(do.call(f, exprs(!!!(step_inputs)))))
      }else{
        design_code[[i]] <- paste0(steps_chosen()[i], " <- ", capture.output(rlang::expr(!!f)))
      }
    }
    design_code[[i+1]] <- paste0(paste0(input$design_name, " <- "), paste0(steps_chosen(), collapse = " + "))
    paste0(unlist(design_code), collapse = "\n")
  })

  output$codePanel <- renderText(design_code())

}

shinyApp(sandbox.ui, sandbox.server)
DesignLibrary:::construct_design_code(design, DesignLibrary:::match.call.defaults(), arguments_as_values = TRUE)


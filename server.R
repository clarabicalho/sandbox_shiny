


##devtools::install_github("DeclareDesign/DeclareDesign", ref = "8f2abbc0583b60e0457d3089a60e8801505dc46b")
library(dd)
library(stringr)
library(knitr)
library(magrittr)
library(tidyr)
library(ggplot2)

remove_spaces <- function(text) {
  gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", str_trim(text)))
}

convert_character_to_vector <- function(text) {
  text <- str_trim(unlist(strsplit(text, split = ",")))
  if (identical(as.character(as.numeric(text)), text)) {
    return(as.numeric(text))
  } else {
    return(text)
  }
}

round_df <- function(df, digits) {
  for (j in 1:ncol(df)) {
    if (class(df[, j]) == "numeric") {
      df[, j] <- round(df[, j], digits = digits)
    }
  }
  return(df)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, clientData, session) {
  # declarations ------------------------------------------------------------

  current_population_code <- reactive({
    paste0(
      "declare_population(N = ",
      input$N,
      ", ",
      input$var_name,
      " = rnorm(N))"
    )
  })

  current_potential_outcomes_code <- reactive({
    paste0(
      "declare_potential_outcomes(Y_Z_0 = ",
      input$y0_po,
      ", Y_Z_1 = ",
      input$y1_po,
      ")"
    )
  })

  current_sampling_code <- reactive({
    paste0("declare_sampling(n = ", input$n, ")")
  })

  current_assignment_code <- reactive({
    paste0("declare_assignment(m = ", input$m, ")")
  })

  current_estimand_code <- reactive({
    paste0("declare_estimand(",
           input$estimand_text,
           ")")
  })

  current_estimator_code <- reactive({
    paste0("declare_estimator(",
           input$formula,
           ", estimand = ", current_estimand_code(), ")")
  })

  current_design_text <- reactive({
    list(
      current_population_code(),
      current_sampling_code(),
      current_potential_outcomes_code(),
      current_assignment_code(),
      "reveal_outcomes"
    )
  })

  current_design_object <- reactive({
    text_list <- lapply(current_design_text(), function(x) paste0(x, "()"))
    design <- do.call(declare_design_, args = text_list)
    return(design)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('declaredesign_data_draw.csv', sep = '')
    },
    content = function(file) {
      write.csv(current_data(), file, row.names = FALSE)
    }
  )

  current_data <- reactive({
    current_design_object()$data_function()
  })

  output$estimate_eval <- renderTable({
    eval(parse(text = current_estimator_code()))(current_data())
  }, rownames = FALSE)

  output$estimand_eval <- renderTable({
    eval(parse(text = current_estimand_code()))(current_data())
  }, rownames = FALSE)

  output$current_sample_data_table <- renderDataTable({
    round_df(current_data(), digits = 3)
  }, options = list(
    searching = FALSE,
    ordering = FALSE,
    pageLength = 10
  ))

  output$script <- renderText({
    paste0(c("library(dd)", current_design_text(), "design <- declare_design(my_population(), my_sampling, my_potential_outcomes, my_assignment, reveal_outcomes(), estimator = my_estimator)"), collapse = "\n\n")
  })

  # Plots -------------------------------------------------------------------

  output$po_plot <- renderPlot({
    current_data() %>%
      gather(key = PO, value = outcome, Y_Z_0, Y_Z_1) %>%
      ggplot(aes(x = outcome)) + geom_histogram() +
      facet_wrap(~PO, ncol = 1) +
      theme_minimal() +
      theme(axis.title = element_blank())
  })

})



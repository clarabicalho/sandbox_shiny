
#' @import DeclareDesign
#' @import stringr
NULL

convert_character_to_function <- function(text){
  eval(parse(text = text))
}

remove_spaces <- function(text){
  gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", str_trim(text)))
}

convert_character_to_vector <- function(text){
  text <- str_trim(unlist(strsplit(text, split = ",")))
  if(identical(as.character(as.numeric(text)), text)){
    return(as.numeric(text))
  } else {
    return(text)
  }
}

round_df <- function(df, digits){
  for(j in 1:ncol(df)){
    if(class(df[,j]) == "numeric"){
      df[,j] <- round(df[,j], digits = digits)
    }
  }
  return(df)
}

convert_character_to_function <- function(text){
  return(eval(parse(text = text)))
}

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {

  # Outputs from registered design -------------------------------------------------------
  observe({

    updateTextInput(session = session, inputId = "potential_outcomes_formula", value = paste0("Y ~ 0.1 + ", input$po_treat_mean - input$po_control_mean, "*Z + noise"))
    updateTextInput(session = session, inputId = "potential_outcomes_formula_impl", value = paste0("Y ~ 0.1 + ", input$po_treat_mean_impl - input$po_control_mean_impl, "*Z + noise"))
  })

  ##current_simple_potential_outcomes_function <- reactive({
  ##
  ##})

  current_sample_data <- reactive({
    draw_data(design = current_design())
  })

  current_population_data <- reactive({
    draw_population(population = current_population(), potential_outcomes = current_potential_outcomes())
  })

  output$current_sample_data_table <- renderDataTable({
    round_df(current_sample_data(), digits = 3)
  }, options = list(searching = FALSE, ordering = FALSE, pageLength = 10))

  output$treatment_table <- renderDataTable({
    tab <- data.frame(table(current_sample_data()$Z))
    names(tab) <- c("Condition name", "Frequency")
    tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$sampling_table <- renderDataTable({
    tab <- data.frame(table(current_sample_data()$sampled))
    names(tab) <- c("Condition name", "Frequency")
    tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('declaredesign_data_draw.csv', sep='')
    },
    content = function(file) {
      write.csv(current_sample_data(), file)
    }
  )

  output$estimand_table <- renderText({
    if(input$estimand_type == "population")
      estimand_data <- current_population_data()
    else
      estimand_data <- current_sample_data()
    round(get_estimands(current_estimand(), data = estimand_data), 3)
  })

  output$po_plot <- renderPlot({
    condition_names <- current_potential_outcomes()$condition_names
    outcome_names <- paste("Y", condition_names, sep = "_")
    par(mfrow = c(length(condition_names),1), las = 1)
    for(j in 1:length(condition_names)){
      hist(current_population_data()[, outcome_names[j]], xlim = c(min(current_population_data()[, outcome_names]),
                                                                   max(current_population_data()[, outcome_names])),
           main = paste("Potential outcomes under condition", condition_names[j]),
           xlab = "value of outcome")
    }
  })

  current_diagnosis <- reactive({
    diagnose_design(design = current_design(), sample_draws = input$sample_draws,
                    population_draws = input$population_draws)
  })

  output$quick_diagnosis1 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$quick_diagnosis2 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$quick_diagnosis3 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$quick_diagnosis3 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$quick_diagnosis4 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$quick_diagnosis5 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$quick_diagnosis6 <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis[c(1, 9:10, 20, 22), , drop = FALSE], 4)
    diag_tab <- cbind(c("ATE", "Bias", "Coverage", "Power", "Avg. Est"), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$diagnosis_table <- renderDataTable({
    diag_tab <- round(current_diagnosis()$diagnosis, 4)
    diag_tab <- cbind(rownames(diag_tab), diag_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$estimates_table <- renderDataTable({
    est_tab <- round(get_estimates(current_estimator(), data = current_sample_data()), 3)
    est_tab <- cbind(rownames(est_tab), est_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  # Registered design -------------------------------------------------------

  current_population <- reactive({
    call <- paste("declare_population(noise = declare_variable(),",
                  input$cov_name, " = declare_variable(normal_mean = ", input$cov_mean, ", normal_sd = ", input$cov_sd, "), N = ", input$N, ")")
    print(call)
    eval(parse(text = call))
  })

  current_sampling <- reactive({
    declare_sampling(n = input$n)
  })

  current_potential_outcomes <- reactive({
    declare_potential_outcomes(formula = as.formula(input$potential_outcomes_formula),
                               condition_names = convert_character_to_vector(input$condition_names),
                               assignment_variable_name = input$assignment_variable_name)
  })

  current_assignment <- reactive({
    if(input$custom_assignment_function == ""){
      declare_assignment(condition_names = convert_character_to_vector(input$condition_names),
                         probability_each = c(input$prob_assign, 1-input$prob_assign))
      ##call <- paste("declare_assignment(condition_names = convert_character_to_vector(input$condition_names),
      ##                   probability_each = c(input$prob_assign, 1-input$prob_assign))
      ##arguments <- list(condition_names = convert_character_to_vector(input$condition_names),
      ##                  probability_each = c(input$prob_assign, 1-input$prob_assign))
    } else {
      ##d<-input$custom_assignment_function
      ##save(d, file = "~/desktop/ddd.RData")
      declare_assignment(condition_names = convert_character_to_vector(input$condition_names),
                         custom_assignment_function = convert_character_to_function(input$custom_assignment_function))
      ##arguments <- list(condition_names = convert_character_to_vector(input$condition_names),
      ##                   custom_assignment_function = input$custom_assignment_function)
    }
    ##eval(call("declare_assignment", arguments))
  })

  current_estimand <- reactive({
    declare_estimand(estimand_text = input$estimand_text, potential_outcomes = current_potential_outcomes())
    ## eval(call("declare_estimand", estimand_text = input$estimand_text, potential_outcomes = current_potential_outcomes()))
    ##arguments <- list(estimand_text = input$estimand_text, potential_outcomes = current_potential_outcomes())
    ##do.call(declare_estimand, args = arguments)
  })

  current_estimator <- reactive({
    estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z,
                                         estimand = current_estimand(),
                                         labels = "dim")
    estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient,
                                      estimates_options = list(coefficient_name = "Z"),
                                      formula = Y ~ Z, estimand = current_estimand(), labels = "lm")

    get(x = input$estimator)
  })

  current_design <- reactive({

    population <- current_population()
    sampling <- current_sampling()
    assignment <- current_assignment()
    estimand <- current_estimand()
    estimator <- current_estimator()
    potential_outcomes <- current_potential_outcomes()

    declare_design(population = population,
                   sampling = sampling,
                   assignment = assignment,
                   estimator = estimator,
                   potential_outcomes = potential_outcomes,
                   label = "Simple Design")
  })

  # Print inputs for the comparison -----------------------------------------

  output$N <- renderText({
    print(input$N)
  })

  output$prob_assign <- renderText({
    print(input$prob_assign)
  })

  output$estimator <- renderText({
    print(input$estimator)
  })

  output$estimator <- renderText({
    if(input$estimator == "estimator_lm")
      print("The estimator declared was linear regression")
    else
      print("The estimator declared was difference-in-means")
  })

  output$potential_outcomes_formula <- renderText({
    print(input$potential_outcomes_formula)
  })

  output$condition_names <- renderText({
    print(input$condition_names)
  })

  output$assignment_variable_name <- renderText({
    print(input$assignment_variable_name)
  })

  output$n <- renderText({
    print(paste0("The chosen sample size was ", input$n))
  })

  output$assignment_summary <- renderText({
    if(input$assignment_input_type == "simple")
      print(paste("The declared probability of assignment was", input$prob_assign))
    else
      print(paste("A custom assignment function was declared."))
  })

  output$estimand_chosen <- renderText({
    if(input$estimand_input_type == "simple")
      print(paste("A simple estimand was chosen: ", input$estimand_type))
    else
      print(paste("A custom estimand was chosen: ", input$estimand_text))
  })

  # Outputs for impl design ----------------------------------------
  current_sample_data_impl <- reactive({
    draw_data(design = current_design_impl())
  })

  current_population_data_impl <- reactive({
    draw_population(population = current_population_impl(), potential_outcomes = current_potential_outcomes_impl())
  })

  output$current_sample_data_table_impl <- renderDataTable({
    round_df(current_sample_data_impl(), digits = 3)
  }, options = list(searching = FALSE, ordering = FALSE, pageLength = 10))

  output$treatment_table_impl <- renderDataTable({
    tab <- data.frame(table(current_sample_data_impl()$Z))
    names(tab) <- c("Condition name", "Frequency")
    tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$sampling_table_impl <- renderDataTable({
    tab <- data.frame(table(current_sample_data_impl()$sampled))
    names(tab) <- c("Condition name", "Frequency")
    tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$estimand_table_impl <- renderText({
    if(input$estimand_type_impl == "population")
      estimand_data <- current_population_data_impl()
    else
      estimand_data <- current_sample_data_impl()
    round(get_estimands(current_estimand_impl(), data = estimand_data), 3)
  })

  output$estimates_table_impl <- renderDataTable({
    est_tab <- round(get_estimates(current_estimator_impl(), data = current_sample_data_impl()), 3)
    est_tab <- cbind(rownames(est_tab), est_tab)
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output$po_plot_impl <- renderPlot({
    condition_names <- current_potential_outcomes_impl()$condition_names
    outcome_names <- paste("Y", condition_names, sep = "_")
    par(mfrow = c(length(condition_names),1), las = 1)
    for(j in 1:length(condition_names)){
      hist(current_population_data_impl()[, outcome_names[j]], xlim = c(min(current_population_data_impl()[, outcome_names]),
                                                                        max(current_population_data_impl()[, outcome_names])),
           main = paste("Potential outcomes under condition", condition_names[j]),
           xlab = "value of outcome")
    }
  })

  current_diagnosis_impl <- reactive({
    diagnose_design(design = current_design_impl(), sample_draws = input$sample_draws_compare,
                    population_draws = input$population_draws_compare)
  })

  output$diagnosis_table_compare <- renderDataTable({
    diag_tab <- round(cbind(current_diagnosis()$diagnosis, current_diagnosis_impl()$diagnosis), 4)
    diag_tab <- cbind(rownames(diag_tab), diag_tab)
    colnames(diag_tab)[2:3] <- c("Registered", "Implemented")
    diag_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))


  # impl design ------------------------------------------------------

  current_population_impl <- reactive({
    call <- paste("declare_population(noise = declare_variable(),",
                  input$cov_name_impl, " = declare_variable(normal_mean = ", input$cov_mean_impl, ", normal_sd = ", input$cov_sd_impl, "), N = ", input$N_impl, ")")
    print(call)
    eval(parse(text = call))
  })

  current_sampling_impl <- reactive({
    declare_sampling(n = input$n_impl)
  })

  current_potential_outcomes_impl <- reactive({
    declare_potential_outcomes(formula = as.formula(input$potential_outcomes_formula_impl),
                               condition_names = convert_character_to_vector(input$condition_names_impl),
                               assignment_variable_name = input$assignment_variable_name_impl)
  })

  current_assignment_impl <- reactive({
    if(input$custom_assignment_function_impl == ""){
      declare_assignment(condition_names = convert_character_to_vector(input$condition_names_impl),
                         probability_each = c(input$prob_assign_impl, 1-input$prob_assign_impl))
      ##call <- paste("declare_assignment(condition_names = convert_character_to_vector(input$condition_names),
      ##                   probability_each = c(input$prob_assign, 1-input$prob_assign))
      ##arguments <- list(condition_names = convert_character_to_vector(input$condition_names),
      ##                  probability_each = c(input$prob_assign, 1-input$prob_assign))
    } else {
      ##d<-input$custom_assignment_function
      ##save(d, file = "~/desktop/ddd.RData")
      declare_assignment(condition_names = convert_character_to_vector(input$condition_names_impl),
                         custom_assignment_function = convert_character_to_function(input$custom_assignment_function_impl))
      ##arguments <- list(condition_names = convert_character_to_vector(input$condition_names),
      ##                   custom_assignment_function = input$custom_assignment_function)
    }
    ##eval(call("declare_assignment", arguments))
  })

  current_estimand_impl <- reactive({
    declare_estimand(estimand_text = input$estimand_text_impl, potential_outcomes = current_potential_outcomes_impl())
    ## eval(call("declare_estimand", estimand_text = input$estimand_text, potential_outcomes = current_potential_outcomes()))
    ##arguments <- list(estimand_text = input$estimand_text, potential_outcomes = current_potential_outcomes())
    ##do.call(declare_estimand, args = arguments)
  })

  current_estimator_impl <- reactive({
    estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z,
                                         estimand = current_estimand_impl(),
                                         labels = "dim")
    estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient,
                                      estimates_options = list(coefficient_name = "Z"),
                                      formula = Y ~ Z, estimand = current_estimand_impl(), labels = "lm")

    get(x = input$estimator_impl)
  })

  current_design_impl <- reactive({

    population <- current_population_impl()
    sampling <- current_sampling_impl()
    assignment <- current_assignment_impl()
    estimand <- current_estimand_impl()
    estimator <- current_estimator_impl()
    potential_outcomes <- current_potential_outcomes_impl()

    declare_design(population = population,
                   sampling = sampling,
                   assignment = assignment,
                   estimator = estimator,
                   potential_outcomes = potential_outcomes,
                   label = "impl Design")
  })

}


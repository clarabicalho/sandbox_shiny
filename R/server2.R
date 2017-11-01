server2 <- function(input, output, clientData, session) {


  current_population <- reactive(tab01_make_population(input$N, input$cov_name, input$cov_mean, input$cov_sd))

  current_potential_outcome <- reactive(tab01_make_potential_outcome(input$po_input_type,
                                                                     input$po_treat_mean,
                                                                     input$po_control_mean,
                                                                     input$potential_outcomes_formula,
                                                                     input$condition_names,
                                                                     input$assignment_variable_name))


  current_sampling <- reactive(tab01_make_sampling(input$n))


  current_estimand <- reactive(tab01_current_estimand(input$estimand_input_type, input$estimand_type, input$estimand_text))

  current_assignment <- reactive(tab01_make_assignment(input$assignment_input_type, input$prob_assign, input$custom_assignment_function))

  current_estimator <- reactive(tab01_make_estimator(current_estimand(), input$estimator))

  current_design <-reactive(tab01_make_design(current_estimand(),
                                              current_population(),
                                              current_potential_outcome(),
                                              current_sampling(),
                                              current_assignment(),
                                              current_estimator()
                                              ))


  current_diagnosis <- reactive(tab01_make_diagnosis(current_design(), input$population_draws, input$sample_draws))


  quick_tbl <- renderDataTable({
    diag_tab <- current_diagnosis()
    diag_tab <- get_diagnosands(diagnosis = diag_tab)
    rownames(diag_tab) <- diag_tab$estimand_label
    diag_tab <- diag_tab[c('mean_estimate', 'bias', 'coverage', 'power', 'mean_estimand')]
    diag_tab <- round_df(diag_tab, 4)
    diag_tab <- as.data.frame(t(diag_tab))
    diag_tab <- data.frame(Diagnosand=str_to_title(str_replace_all(row.names(diag_tab), "_", " ")), # labels
                           diag_tab)
    diag_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))



  output[['quick_diagnosis1']] <- quick_tbl
  output[['quick_diagnosis2']] <- quick_tbl
  output[['quick_diagnosis3']] <- quick_tbl
  output[['quick_diagnosis4']] <- quick_tbl
  output[['quick_diagnosis5']] <- quick_tbl
  output[['quick_diagnosis6']] <- quick_tbl


  output[['po_plot']] <- renderPlot(tab01_make_po_plot(current_population(), current_potential_outcome()))


  output[['estimand_table']] <- renderPrint({
    round_df(get_estimands(current_design()), 3)
  })


  output[['treatment_table']] <- renderDataTable(tab01_make_treatment_table(current_design()),
                                                 options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output[["estimates_table"]] <- renderDataTable(tab01_make_estimator_table(current_design()),
                                                 options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))


  ### Tab 02 - download sample

  current_sample_data <- reactive({

    draw_data(current_design())
  })

  output[['current_sample_data_table']] <- renderDataTable({
    tab <- data.frame(table(current_sample_data()$Z))
    names(tab) <- c("Condition name", "Frequency")
    tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  output[['downloadData']] <- downloadHandler(
    filename = 'declaredesign_data_draw.csv',
    content = function(file) {
      write.csv(current_sample_data(), file)
    },
    contentType='text/csv'
  )

  #### Tab 3 Diagnosis

  output[['diagnosis_table']] <- renderDataTable({
    diag_tab <- current_diagnosis()
    diag_tab <- get_diagnosands(diagnosis = diag_tab)
    # rownames(diag_tab) <- diag_tab$estimand_label
    diag_tab <- round_df(diag_tab, 4)
    diag_tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))

  ### Tab 4 - Register - TBD

  ### Tab 5 - Implementation

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




  ## 5.2

  current_population_impl <- reactive({tab01_make_population(input$N_impl, input$cov_name_impl, input$cov_mean_impl, input$cov_sd_impl)})

  current_potential_outcome_impl <- reactive(tab01_make_potential_outcome(input$po_input_type_impl,
                                                                     input$po_treat_mean_impl,
                                                                     input$po_control_mean_impl,
                                                                     input$potential_outcomes_formula_impl,
                                                                     input$condition_names_impl,
                                                                     input$assignment_variable_name_impl))


  current_sampling_impl <- reactive(tab01_make_sampling(input$n_impl))

  current_estimand_impl <- reactive(tab01_current_estimand(input$estimand_input_type_impl, input$estimand_type_impl, input$estimand_text_impl))

  current_assignment_impl <- reactive(tab01_make_assignment(input$assignment_input_type_impl, input$prob_assign_impl, input$custom_assignment_function_impl))


  current_estimator_impl <- reactive(tab01_make_estimator(current_estimand(), input$estimator))


  current_design_impl <-reactive(tab01_make_design( current_estimand_impl(),
                                                    current_population_impl(),
                                                    current_potential_outcome_impl(),
                                                    current_sampling_impl(),
                                                    current_assignment_impl(),
                                                    current_estimator_impl()
        ))

  current_diagnosis_impl <- reactive(tab01_make_diagnosis(current_design_impl(), input$population_draws_compare, input$sample_draws_compare))


  output[['po_plot_impl']] <- renderPlot(tab01_make_po_plot(current_population_impl(), current_potential_outcome_impl()))

  output[['estimand_table_impl']] <- renderPrint({
    round_df(get_estimands(current_design_impl()), 3)
  })

  output[['treatment_table_impl']] <- renderDataTable(tab01_make_treatment_table(current_design_impl()),
                                                 options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))



  output[["estimates_table_impl"]] <- renderDataTable(tab01_make_estimator_table(current_design_impl()))

  ####tab 6 comparison

  output$diagnosis_table_compare <- renderDataTable(tab06_make_diagnosis_comparison(current_diagnosis(), current_diagnosis_impl())
    , options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))




}

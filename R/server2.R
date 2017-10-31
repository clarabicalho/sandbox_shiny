server2 <- function(input, output, clientData, session) {


  current_population <- reactive({
    N <- input$N

    #TODO covariates
    cov_name <- input$cov_name
    cov_mean <- input$cov_mean
    cov_sd  <- input$cov_sd

    declare_population(N=N, noise=rnorm(N, 0, 1)   )

  })

  current_potential_outcome <- reactive({
    if(input$po_input_type == 'Simple'){
      po_treat_mean <- input$po_treat_mean
      po_control_mean <- input$po_control_mean

      ret <- declare_potential_outcomes(Y_Z_0=rnorm(N, po_control_mean),
                                 Y_Z_1=rnorm(N, po_treat_mean))

    } else if(input$po_input_type == 'Custom'){
      potential_outcomes_formula <- as.formula(input$potential_outcomes_formula)
      condition_names <- input$condition_names
      assignment_variable_name <- input$assignment_variable_name

      ret <- declare_potential_outcomes(formula=formula, condition_names=condition_names, assignment_variable_name=assignment_variable_name)
    }
    ret
  })

  current_sampling <- reactive({
    n <- input$n
    declare_sampling(n=n)
  })

  simple_estimand <- list(
    population=declare_estimand(ATE=mean(Y_Z_1 - Y_Z_0)),
    sample=declare_estimand(ATE_s=mean(Y_Z_1 - Y_Z_0))
  )

  custom_estimand <- reactive({
    ret <- tryCatch({
    estimand_text <- input$estimand_text
    e <- parse(text=estimand_text)[[1]]

    ret <- eval(call("declare_estimand",Custom=e))
    }, error=function(e) data.frame()) # returning noop estimand
    ret
  })

  current_estimand <- reactive({
    if(input$estimand_input_type == 'Simple'){
      if(!input$estimand_type %in% names(simple_estimand)) warning('invalid simple estimand selected !?')
      ret <- simple_estimand[[input$estimand_type]]
    } else if(input$estimand_input_type == 'Custom'){
      ret <- custom_estimand()
    }
    ret
  })

  current_assignment <- reactive({

    if(input$assignment_input_type == 'Simple'){
      p <- input$prob_assign
      ret <- declare_assignment(prob=p)
    } else if(input$assignment_input_type == 'Custom'){
      assignment_text <- input$custom_assignment_function
      ret <- eval(parse(text=assignment_text)[[1]])
    }
    ret
  })

  current_estimator <- reactive({
    estimand <- current_estimand()
    estimator <- input$estimator

    if(estimator == 'estimator_lm'){
      ret <- declare_estimator(formula=Y~Z, model=lm, estimand=estimand)
    }
    else if(estimator == 'estimator_d_i_m'){
      ret <- declare_estimator(formula=Y~Z, model=estimatr::difference_in_means, estimand=estimand)
    }
    else {
      warning('Should be impossible !?')
    }
    ret
  })

  current_design <-reactive({

    estimand <- current_estimand()
    custom <- if(!any(vapply(simple_estimand, identical, TRUE, estimand))) estimand else NULL

    args <- list(population=current_population(),
                 potential_outcome=current_potential_outcome(),
                 pop_estimand=simple_estimand$population,
                 sampling=current_sampling(),
                 sam_estimand=simple_estimand$sample,
                 custom_estimand=custom,
                 assignment=current_assignment(),
                 reveal=reveal_outcomes,
                 estimator=current_estimator()
    )
    args <- Filter(is.function, args)

    do.call(declare_design, args)
  })

  current_diagnosis <- reactive({
    design <- current_design()
    # diagnosands <- declare_diagnosands()
    diagnose_design(design, sims=5, bootstrap_sims = 5, parallel = FALSE)


  })


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


  output[['po_plot']] <- renderPlot({
    require(ggplot2)
    pop <- current_population()
    poutcome <- current_potential_outcome()

    ggplot(poutcome(pop())) +
      stat_density(aes(x=Y_Z_0), fill=1, alpha=.7) +
      stat_density(aes(x=Y_Z_1), fill=2, alpha=.7)
  })

  output[['estimand_table']] <- renderPrint({
    # estimand_data <- if(input$estimand_type == "population")
    #   current_population_data()
    # else
    #   current_sample_data()
    df <- get_estimands(current_design())
    round_df(df, 3)
  })


  output[['treatment_table']] <- renderDataTable({
    tab <- data.frame(table(draw_data(current_design())$Z))
    names(tab) <- c("Condition name", "Frequency")
    tab
  }, options = list(searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE))


}

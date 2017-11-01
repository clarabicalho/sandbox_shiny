tab01_06_declare_estimators <- tabPanel("6. Declare Estimators",
         fixedPage( fixedRow(
           column(
             wellPanel(p("Characterize the estimator function: the procedure for generating estimates of quantities we want to learn about.")),

             inputPanel(selectInput("estimator", "Estimator",
                                    choices = c("Linear regression" = "estimator_lm",
                                                "Difference-in-means" = "estimator_d_i_m"))
             ), width = 6),
           column(
             h4("Estimates using mock data"),
             dataTableOutput("estimates_table")
             , width = 4),
           column(width = 2,
                  h5("Quick summary"),
                  dataTableOutput("quick_diagnosis6")
           )
         ))
)

tab01_make_estimator <- function(estimand, estimator){

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
}

tab01_make_design <- function(current_estimand, current_population, current_potential_outcome, current_sampling, current_assignment, current_estimator){

  custom <- if(isTRUE(attr(current_estimand, "custom"))) estimand else NULL

  args <- list(population=current_population,
               potential_outcome=current_potential_outcome,
               pop_estimand=simple_estimand$population,
               sampling=current_sampling,
               sam_estimand=simple_estimand$sample,
               custom_estimand=custom,
               assignment=current_assignment,
               reveal=reveal_outcomes,
               estimator=current_estimator
  )
  args <- Filter(is.function, args)

  do.call(declare_design, args)
}

tab01_make_diagnosis <- function(design, population_draws, sample_draws){
  diagnose_design(design, sims=population_draws, bootstrap_sims = sample_draws, parallel = FALSE)
}

tab01_make_estimator_table <- function(current_design){
  est_tab <- round_df(get_estimates(current_design), 3)
  est_tab <- cbind.data.frame(colnames(est_tab), t(est_tab))
  colnames(est_tab) <- c("", "")
  est_tab
}

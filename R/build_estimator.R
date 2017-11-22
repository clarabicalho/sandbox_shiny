step_obj[[DECLARE_ESTIMATOR]] <- local({

step_help_text <- shiny::tags$div(
  shiny::tags$h5("Declare Estimator"),
  shiny::tags$dl(
    shiny::tags$dt("formula"),
    shiny::tags$dd("A model formula"),
    shiny::tags$dt("model"),
    shiny::tags$dd("A model function such as lm, glm, or difference_in_means"),
    shiny::tags$dt("coefficient_name"),
    shiny::tags$dd("(optional) A character vector of coefficients that represent quantities of interest, i.e. Z. Only relevant when a model is chosen or for some estimator_function's such as difference_in_means and lm_robust."),
    shiny::tags$dt("estimand"),
    shiny::tags$dd("(optional) An estimand object created using declare_estimand. Estimates from this estimator function will be associated with the estimand, for example for calculating the bias and coverage of the estimator."),
    shiny::tags$dt("label"),
    shiny::tags$dd("(optional) A label for the estimand if not specified in ...")
  )

)


steps_config <- shiny::tags$div(
  textInput("estimator_formula", "Formula", ""),
  selectInput("estimator_model", "Model", c("estimatr::difference_in_means", "estimatr::lm_robust", "lm", "glm")),
  textInput("estimator_coefficient", "Coefficient"),
  textInput("estimator_estimand", "Estimand"),
  textInput("estimator_label", "Label", "")
)


steps_dynamic <- function(input, output, session){


  update_options <- function(input, session){

    options <- paste(collapse=", ", c(
      input$estimand_options,
      if(input$estimand_subset   != "") sprintf("subset=%s", input$estimand_subset),
      if(input$estimand_label    != "") sprintf("label='%s'", input$estimand_label),
      if(input$estimand_function != "") sprintf("estimand_function=%s", input$estimand_function)
    ))


    updateTextInput(session, "edit_args", value=options)
  }

  # NJF 9/21 Above seems to not work although below does :(
  observeEvent(input$estimand_options,    update_options(input, session))
  observeEvent(input$estimand_subset,     update_options(input, session))
  observeEvent(input$estimand_label,      update_options(input, session))
  observeEvent(input$estimand_function,   update_options(input, session))
}


list(
  name=DECLARE_ESTIMATOR,
  label="Estimator",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})

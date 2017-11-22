step_obj[[DECLARE_ESTIMAND]] <- local({

step_help_text <- shiny::tags$div(
  shiny::tags$h5("Declare Estimand"),
  shiny::tags$dl(
    shiny::tags$dt("..."),
    shiny::tags$dd("Named estimands"),
    shiny::tags$dt("subset"),
    shiny::tags$dd("(optional) A subset to calculate the estimand on"),
    shiny::tags$dt("label"),
    shiny::tags$dd("(optional) A label for the estimand if not specified in ...")
  )

)


steps_config <- shiny::tags$div(

  textInput("estimand_options", "Estimand"),
  textInput("estimand_subset", "subset", ""),
  textInput("estimand_label", "Label", ""),
  textInput("estimand_function", "Custom Estimand Function", "")
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
  name=DECLARE_ESTIMAND,
  label="Estimand",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})

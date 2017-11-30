step_obj[[DECLARE_ESTIMAND]] <- local({

step_help_text <- shiny::tags$div(
  shiny::tags$h5("Declare Estimand"),
  shiny::tags$dl(
    shiny::tags$dt("..."),
    shiny::tags$dd("Named estimands"),
    shiny::tags$dt("subset"),
    shiny::tags$dd("(optional) A subset to calculate the estimand on"),
    shiny::tags$dt("label"),
    shiny::tags$dd("(optional) A label for the estimand if not specified via ...")
  )

)


steps_config <- shiny::tags$div(

  textInput("estimand_options", "Estimand", "mean(Y_Z_1 - Y_Z_0)"),
  textInput("estimand_subset", "Subset", ""),
  textInput("estimand_label", "Label", "ATE"),
  textInput("estimand_function", "Custom Estimand Function", "")
)


steps_dynamic <- function(input, output, session){


  update_options <- function(){

    options <- paste(collapse=", ", c(
                 input$estimand_options,
                 if(input$estimand_subset   != "") sprintf("subset=%s", input$estimand_subset),
                 if(input$estimand_label    != "") sprintf("label='%s'", input$estimand_label),
                 if(input$estimand_function != "") sprintf("estimand_function=%s", input$estimand_function)
    ))

    updateTextInput(session, "edit_args", value=options)
  }

  observe_i_and_update(update_options, input, "estimand_", "options", "subset", "label", "function")

}


list(
  name=DECLARE_ESTIMAND,
  label="Estimand",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})

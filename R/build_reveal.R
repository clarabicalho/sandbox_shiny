step_obj[[REVEAL_OUTCOMES]] <- local({

step_help_text <- shiny::tags$div(
  shiny::tags$h5("Reveal Observed Outcomes"),
  shiny::tags$p('Typically, a design includes a potential outcomes declaration and an assignment declaration. Reveal outcomes uses the random assignment to pluck out the correct potential outcomes. This is analogous to the "switching equation" (Gerber and Green 2012, Chapter 2).'),
  shiny::tags$dl(
    shiny::tags$dt("outcome_variable_names"),
    shiny::tags$dd("The outcome prefix(es) of the potential outcomes"),
    shiny::tags$dt("assignment_variable_names"),
    shiny::tags$dd("(optional) The bare (unquote) name(s) of the assignment variable"),
    shiny::tags$dt("attrition_variable_name"),
    shiny::tags$dd("(optional) The bare (unquote) name of the attrition variable"),
    shiny::tags$dt("outcome_function"),
    shiny::tags$dd("If specified, reveal_outcomes draws outcomes using outcome_function rather than the switching equation.")
  )
)


steps_config <- shiny::tags$div(

  textInput("reveal_outcome_variable_names", "Outcome Variable Names", ""),
  textInput("reveal_assignment_variable_names", "Assignment Variable Names", ""),
  textInput("reveal_attrition_variable_name", "Attrition Variable Name", ""),
  textInput("reveal_outcome_function", "Outcome Function", "")

)


steps_dynamic <- function(input, output, session){

  update_options <- function(){

    options <- c(outcome_variable_names=input$reveal_outcome_variable_names,
                 assignment_variable_names=input$reveal_assignment_variable_names,
                 attrition_variable_name=input$reveal_attrition_variable_name,
                 outcome_function=input$reveal_outcome_function)

    options <- Filter(nchar, options)
    options <- paste(names(options), options, sep="=", collapse =", ")

    updateTextInput(session, "edit_args", value=options)
  }

  observe_i_and_update(update_options, input, "reveal_", "outcome_variable_names","assignment_variable_names","attrition_variable_name","outcome_function")

}

list(
  name = REVEAL_OUTCOMES,
  label = "Reveal Outcomes",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})

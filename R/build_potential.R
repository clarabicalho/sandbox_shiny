step_obj[[DECLARE_POTENTIAL_OUTCOMES]] <- local({

step_help_text <- shiny::tags$div(
  shiny::tags$h5("Declare Potential Outcomes"),
  shiny::tags$dl(
    shiny::tags$dt("formula"),
    shiny::tags$dd("eg formula = Y ~ .25 * Z + .01 * age * Z"),
    shiny::tags$dt("assignment_variable_name"),
    shiny::tags$dd("(optional) variable name for Outcomes (Z)"),
    shiny::tags$dt("condition_names"),
    shiny::tags$dd("(optional) conditions the assignment may take")
  )
)


steps_config <- shiny::tags$div(

  selectInput("potential_type", "Outcome Type:", c("Binary", "Likert", "Normal", "Identity")),

  textInput("potential_outcome_name", "Outcome Name", "Y"),

  uiOutput("potential_condition_chooser"),

  textInput("potential_condition_names", "Condition Names", "")


)


steps_dynamic <- function(input, output, session){


  output$potential_condition_chooser <- renderUI({
      make_variable_chooser("potential_condition_variable", session$userData$DD$design_instance(), input$potential_condition_variable)
  })


  update_options <- function(){

    options <- sprintf ("%s~%s(%s%s)",  input$potential_outcome_name,
                        switch(input$potential_type, Binary="draw_binary", Likert="draw_discrete", Normal="rnorm", ""),
                        switch(input$potential_type, Normal=sprintf("n=length(%s), ", input$potential_condition_variable), input$potential_condition_variable),
                        switch(input$potential_type, Likert=", breaks=1:7", Normal=input$potential_condition_variable, "")
                        )

    if(isTRUE(input$potential_condition_names != ""))
      options <- sprintf("%s, condition_names = %s", options, input$potential_condition_names)

    updateTextInput(session, "edit_args", value=options)
  }

  # NJF 9/21 Above seems to not work although below does :(
  observe_i_and_update(update_options, input, "potential_", "type", "outcome_name", "condition_variable", "condition_names")
}


list(
  name = DECLARE_POTENTIAL_OUTCOMES,
  label = "Potential Outcomes",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})


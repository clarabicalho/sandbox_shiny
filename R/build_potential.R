step_help_text[[DECLARE_POTENTIAL_OUTCOMES]] <- shiny::tags$div(
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


steps_config[[DECLARE_POTENTIAL_OUTCOMES]] <- shiny::tags$div(

  selectInput("potential_type", "Outcome Type:", c("Binary", "Likert", "Normal")),

  textInput("potential_outcome_name", "Outcome Name", "Y"),

  uiOutput("potential_condition_chooser"),

  textInput("potential_condition_names", "Condition Names", "")


)


steps_dynamic[[DECLARE_POTENTIAL_OUTCOMES]] <- function(input, output, session, design_instance){

  if(get0(DECLARE_POTENTIAL_OUTCOMES, session$userData, ifnotfound = FALSE)) return() # already done this
  session$userData[[DECLARE_POTENTIAL_OUTCOMES]] <- TRUE
  message("registering callbacks for ", DECLARE_POTENTIAL_OUTCOMES)



  output$potential_condition_chooser <- renderUI({
      make_variable_chooser("potential_condition_variable", design_instance, input$potential_condition_variable)
  })


  update_options <- function(input, session){

    options <- sprintf ("%s~%s(%s%s)",  input$potential_outcome_name,
                        switch(input$potential_type, Binary="draw_binary", Likert="draw_discrete", "Normal"="rnorm"),
                        input$potential_condition_variable,
                        switch(input$potential_type, Likert=", breaks=1:7", "")
                        )

    if(isTRUE(input$potential_condition_names != ""))
      options <- sprintf("%s, condition_names = %s", options, input$potential_condition_names)

    updateTextInput(session, "edit_args", value=options)
  }

  # NJF 9/21 Above seems to not work although below does :(
  observeEvent(input$potential_type, update_options(input, session))
  observeEvent(input$potential_outcome_name, update_options(input, session))
  observeEvent(input$potential_condition_variable, update_options(input, session))
  observeEvent(input$potential_condition_names, update_options(input, session))
}


step_obj[[DECLARE_POTENTIAL_OUTCOMES]] <- list(config=steps_config[[DECLARE_POTENTIAL_OUTCOMES]],
                                     help=step_help_text[[DECLARE_POTENTIAL_OUTCOMES]],
                                     server=steps_dynamic[[DECLARE_POTENTIAL_OUTCOMES]])


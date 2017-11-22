step_obj[[DECLARE_POPULATION]] <- local({

step_help_text <-   shiny::tags$div(
  shiny::tags$h5("Declare the Size and Features of the Population"),
  shiny::tags$dl(
    shiny::tags$dt("N"),
    shiny::tags$dd("number of units to draw. If provided as fabricate(N = 5), this determines the number of units in the single-level data. If provided in level, i.e. fabricate(cities = level(N = 5)), N determines the number of units in a specific level of a hierarchical dataset."),
    shiny::tags$dt("ID_label"),
    shiny::tags$dd("(optional) variable name for ID variable, i.e. citizen_ID")
  )
)



steps_config <- shiny::tags$div(

  selectInput("population_mode", "Mode", c("Simple", "Hierarchical"), selected = "Simple"),

  uiOutput("population_builder")

  # numericInput("population_N", "N", 100, min = 0),
  # textInput("reveal_assignment_variable_names", "Assignment Variable Names", ""),
  # textInput("reveal_attrition_variable_name", "Attrition Variable Name", ""),
  # textInput("reveal_outcome_function", "Outcome Function", "")

)


steps_dynamic <- function(input, output, session){

  output$population_builder <- renderUI(
    switch(input$population_mode, Simple=uiOutput("population_builder_simple"),
                                  Hierarchical=uiOutput("population_builder_hierarchical"))
  )

  output$population_builder_simple <- renderUI("TBD")

  output$population_builder_hierarchical <- renderUI("TBD2")

  update_options <- function(){
#
#     options <- c(outcome_variable_names=input$reveal_outcome_variable_names,
#                  assignment_variable_names=input$reveal_assignment_variable_names,
#                  attrition_variable_name=input$reveal_attrition_variable_name,
#                  outcome_function=input$reveal_outcome_function)
#
#     options <- Filter(nchar, options)
#     options <- paste(names(options), options, sep="=", collapse =", ")

    updateTextInput(session, "edit_args", value=options)
  }

  observe_i_and_update(update_options, input, "population_")

}


list(
  name = DECLARE_POPULATION,
  label = "Population",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})

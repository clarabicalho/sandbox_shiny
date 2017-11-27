type_to_variable_declaration <- function(fmt="%s~%s(%s%s)", name, type, arg) {
  sprintf (fmt,  name,
                    switch(type, Binary="draw_binary", Likert="draw_discrete", "Normal"="rnorm"),
                    arg,
                    switch(type, Likert=", breaks=1:7", "")
  )
}


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

)


steps_dynamic <- function(input, output, session){

  output$population_builder <- renderUI(
    switch(input$population_mode, Simple=uiOutput("population_builder_simple"),
                                  Hierarchical=uiOutput("population_builder_hierarchical"))
  )

  population_builder_simple_state <- reactiveValues(variables=c(noise="Normal"), registered=c())

  output$population_builder_simple <- renderUI({


    shiny::div(
      numericInput("population_simple_N", "N:", 100, 1, Inf),
      uiOutput("population_simple_variables"),
      actionButton("population_simple_add", "Add Variable")
    )

  })

  output$population_simple_variables <- renderUI({

    out <- list()

    for(i in seq_along(population_builder_simple_state$variables)){
      name_name <- sprintf("population_simple_name_%i", i)
      type_name <- sprintf("population_simple_type_%i", i)

      out[[i]] <- shiny::tags$div(
        textInput(name_name, "Variable Name", names(population_builder_simple_state$variables)[i]),
        selectInput(type_name, "Variable Type:", c("Binary", "Likert", "Normal"), population_builder_simple_state$variables[i])
      )

      if(! name_name %in% population_builder_simple_state$registered){
        observe_i_and_update(local({
          i <- i
          name_name <- name_name
          type_name <- type_name
          function(){
            names(population_builder_simple_state$variables)[i] <- input[[name_name]]
            population_builder_simple_state$variables[i] <- input[[type_name]]
            update_options()
          }
        }), input, "", name_name, type_name)
        population_builder_simple_state$registered <- append(population_builder_simple_state$registered, name_name)
      }

    }

    out
  })

  observeEvent(input$population_simple_add, {
    message("adding variable ", length(population_builder_simple_state$variables))
    population_builder_simple_state$variables <- append(population_builder_simple_state$variables, c(new="Normal"))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$population_builder_hierarchical <- renderUI("TBD2")

  update_options <- function(){

    if(input$population_mode == "Simple"){
      options <- c( sprintf('`N=%d`', input$population_simple_N),
                    mapply(type_to_variable_declaration,
                           fmt="%s=%s(%s%s)",
                           name=names(population_builder_simple_state$variables),
                           type=population_builder_simple_state$variables,
                           arg="N" )
                )

    } else if(input$population_mode == "Hierarchical"){
      options <- NA_character_
    }

    options <- paste(options, collapse=", ")

    updateTextInput(session, "edit_args", value=options)
  }

  observe_i_and_update(update_options, input, "population_", "simple_N")

}


list(
  name = DECLARE_POPULATION,
  label = "Population",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})

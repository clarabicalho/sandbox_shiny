step_obj[[DECLARE_ASSIGNMENT]] <- local({



step_help_text <-   shiny::tags$div(
  shiny::tags$h5("Declare Assignment"),
  shiny::tags$dl(
    shiny::tags$dt("m"),
    shiny::tags$dd(	"Use for a two-arm design in which m units (or clusters) are assigned to treatment and N-m units (or clusters) are assigned to control. In a blocked design, exactly m units in each block will be treated. (optional)"),
    shiny::tags$dt("m-each"),
    shiny::tags$dd("Use for a multi-arm design in which the values of m_each determine the number of units (or clusters) assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many units (or clusters) should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)"),
    shiny::tags$dt("prob"),
    shiny::tags$dd("Use for a two-arm design to assign prob percent of observations to condition and remainder to control  (optional)"),
    shiny::tags$dt("prob_each"),
    shiny::tags$dd("Use for a multi-arm design to assign each observation to a condition i with prob[i] probability  (optional)"),
    shiny::tags$dt("simple"),
    shiny::tags$dd("TRUE/FALSE use for simple random assignment"),
    shiny::tags$dt("assignment_variable_name"),
    shiny::tags$dd("(optional) Override the default assignment variable name")
  )
)

steps_config <- shiny::tags$div(

  selectInput("assignment_type", "assignment Type:", c("Two Arm (m)"="m",
                                                       "Two Arm (p)"="prob",
                                                       "Multi Arm (m)"="m_each",
                                                       "Multi Arm (p)"="prob_each",
                                                       "SRA (probability)"="sra")),
  textInput("assignment_param", "Param", 10),


  material_checkbox("assignment_block", "Block:", FALSE),

  uiOutput("assignment_block_chooser"),

  material_checkbox("assignment_cluster", "Cluster:", FALSE),

  uiOutput("assignment_cluster_chooser")


)


steps_dynamic <- function(input, output, session){

  observeEvent(input$assignment_type, updateNumericInput(session, "assignment_param",
                                                       label=paste0("Choose ",
                                                                    switch(input$assignment_type, sra="prob_each", input$assignment_type),
                                                                    ":")
  ))


  output$assignment_block_chooser <- renderUI({
    message("hiarylah");
    if(isTRUE(input$assignment_block)) {
      # browser()
      make_variable_chooser("assignment_block_variable", session$userData$DD$design_instance(), input$assignment_block_variable)
    }
  })

  output$assignment_cluster_chooser <- renderUI({
    message("dfsa[", input$assignment_cluster, "]adfs\n");
    if(isTRUE(input$assignment_cluster))
      make_variable_chooser("assignment_cluster_variable", session$userData$DD$design_instance(), input$assignment_cluster_variable)
  })

  update_options <- function(){
    options <- sprintf("%s=%s",
                       switch(input$assignment_type, sra="prob_each", input$assignment_type),
                       input$assignment_param)
    if(isTRUE(input$assignment_type %in% c('m', 'prob'))) options <- sprintf("`%s`", options) # only two arm works for inspector for now.

    if(isTRUE(input$assignment_type == "sra")) options <- paste0(options, ", simple = TRUE")
    if(isTRUE(input$assignment_cluster))       options <- paste0(options, ", clust_var =", input$assignment_cluster_variable)
    if(isTRUE(input$assignment_block))        options <- paste0(options, ", block_var =", input$assignment_block_variable)

    updateTextInput(session, "edit_args", value=options)
  }

  observe_i_and_update(update_options, input, "assignment_", "type", "param", "cluster", "block", "cluster_variable", "block_variable")

}

list(
  name=DECLARE_ASSIGNMENT,
  label="Assignment",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})


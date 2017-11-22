step_obj[[DECLARE_SAMPLING]] <- local({

step_help_text <- shiny::tags$div(
  shiny::tags$h5("Declare Sampling Procedure"),
  shiny::tags$dl(
    shiny::tags$dt("n"),
    shiny::tags$dd("Use for a design in which n units (or clusters) are sampled. In a stratified design, exactly n units in each stratum will be sampled. (optional)"),
    shiny::tags$dt("prob / simple"),
    shiny::tags$dd("(optional) Take a prob-% fixed-size sample or, if simple is TRUE, a SRS with prob")
  )
)

steps_config <- shiny::tags$div(

  selectInput("sampling_type", "Sampling Type:", c("Complete (n)"="n", "Complete (p)"="p", "SRS (p)"="srs")),
  numericInput("sampling_param", "Param", 0),


  material_checkbox("sampling_strata", "Strata:", FALSE),

  uiOutput("sampling_strata_chooser"),

  material_checkbox("sampling_cluster", "Cluster:", FALSE),

  uiOutput("sampling_cluster_chooser")


)


steps_dynamic <- function(input, output, session){


  output$sampling_strata_chooser <- renderUI({
    # message("hiarylah");
    if(isTRUE(input$sampling_strata))
      make_variable_chooser("sampling_strata_variable", session$userData$DD$design_instance(), input$sampling_strata_variable)
  })

  output$sampling_cluster_chooser <- renderUI({
    # message("dfsa[", input$sampling_cluster, "]adfs\n");
    if(isTRUE(input$sampling_cluster))
      make_variable_chooser("sampling_cluster_variable", session$userData$DD$design_instance(), input$sampling_cluster_variable)
  })

  update_options <- function(input, session){
    options <- sprintf("`%s=%s`",
                       switch(input$sampling_type, srs="p", input$sampling_type),
                       input$sampling_param)

    if(isTRUE(input$sampling_type == "srs")) options <- paste0(options, ", simple = TRUE")
    if(isTRUE(input$sampling_cluster))       options <- paste0(options, ", clust_var =", input$sampling_cluster_variable)
    if(isTRUE(input$sampling_strata))        options <- paste0(options, ", strata_var =", input$sampling_strata_variable)

    updateTextInput(session, "edit_args", value=options)
  }

  # NJF 9/21 Above seems to not work although below does :(
  observeEvent(input$sampling_type, update_options(input, session))
  observeEvent(input$sampling_param, update_options(input, session))
  observeEvent(input$sampling_cluster, update_options(input, session))
  observeEvent(input$sampling_strata, update_options(input, session))
  observeEvent(input$sampling_cluster_variable, update_options(input, session))
  observeEvent(input$sampling_strata_variable, update_options(input, session))
}

list(
  name = DECLARE_SAMPLING,
  label = "Sampling",
  config=steps_config,
  help=step_help_text,
  server=steps_dynamic
)

})


tab01_02_declare_potential_outcomes <- tabPanel("2. Declare Potential Outcomes",
         fixedPage(
           fixedRow(
             column(width = 4,
                    wellPanel("Characterize the potential outcomes function: the outcomes that each unit might exhibit depending on how the causal process being studied changes the world."),
                    inputPanel(
                      radioButtons("po_input_type", "Options for Defining Potential Outcomes", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                    ),
                    conditionalPanel(condition = "input.po_input_type == 'Simple'",
                                     wellPanel(numericInput("po_treat_mean", "Mean outcome in treatment group", 0.5),
                                               numericInput("po_control_mean", "Mean outcome in control group", 0))
                    ),
                    conditionalPanel(condition = "input.po_input_type == 'Custom'",
                                     wellPanel(textInput("potential_outcomes_formula", label = "Potential Outcomes Formula", value = "Y ~ 0.1 + 1 * Z + noise"),
                                               textInput("condition_names", label = "Condition names (separated by commas)", value = "0, 1"),
                                               textInput("assignment_variable_name", label = "Assignment Variable Name", value = "Z"))
                    )
             ),
             column(width = 4,
                    plotOutput("po_plot")
             ),
             column(width = 2,
                    h5("Quick summary"),
                    dataTableOutput("quick_diagnosis2")
             )
           ))
)

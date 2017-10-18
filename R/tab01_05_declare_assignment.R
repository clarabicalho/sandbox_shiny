tab01_05_declare_assignment <- tabPanel("5. Declare Assignment",
         fixedPage(fixedRow(

           column(width = 6,
                  wellPanel(p("Characterize the assignment function: the manner in which units are assigned to reveal one potential outcome or another.")),
                  inputPanel(
                    radioButtons("assignment_input_type", "Options for Defining Assignment", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                  ),
                  conditionalPanel(condition = "input.assignment_input_type == 'Simple'",
                                   wellPanel(numericInput("prob_assign", "Proportion of units to assign to treatment", 0.5, min = 0, max = 1))
                  ),
                  conditionalPanel(condition = "input.assignment_input_type == 'Custom'",
                                   wellPanel(textInput("custom_assignment_function", label = "Custom assignment function"))
                  )),

           column(width = 4,
                  dataTableOutput('treatment_table')
           ),
           column(width = 2,
                  h5("Quick summary"),
                  dataTableOutput("quick_diagnosis5")
           )
         ))
)

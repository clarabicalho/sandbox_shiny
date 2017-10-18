tab05_05_declare_assignment <- tabPanel("5. Declare Assignment",
         splitLayout(


           sidebarPanel(
             fluidRow(inputPanel(
               textOutput("assignment_summary")
             ), width = 8),
             inputPanel(
               radioButtons("assignment_input_type_impl", "Options for Defining Assignment", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
               , width = 8),
             conditionalPanel(condition = "input.assignment_input_type_impl == 'Simple'",
                              numericInput("prob_assign_impl", "Proportion of units to assign to treatment", 0.5, min = 0, max = 1)
             ),
             conditionalPanel(condition = "input.assignment_input_type_impl == 'Custom'",
                              textInput("custom_assignment_function_impl", label = "Custom assignment function")
             ), width = 8),

           mainPanel(
             dataTableOutput('treatment_table_impl')
           )
         )
)

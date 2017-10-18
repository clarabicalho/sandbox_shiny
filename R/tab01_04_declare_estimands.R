tab01_04_declare_estimands <- tabPanel("4. Declare Estimands",
         fixedPage(fixedRow(
           column(width = 6,
                  wellPanel(p("Characterize the estimands: the specification of the things that we want to learn about the world, described in
terms of potential outcomes.")),
                  inputPanel(
                    radioButtons("estimand_input_type", "Options for Defining Estimand", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                  ),
                  conditionalPanel(condition = "input.estimand_input_type == 'Simple'",
                                   radioButtons("estimand_type", label = "Choose a simple estimand:", choices =  list("Population average treatment effect" = "population",
                                                                                                                      "Sample average treatment effect" = "sample"),
                                                selected = "population")
                  ),
                  conditionalPanel(condition = "input.estimand_input_type == 'Custom'",
                                   textInput("estimand_text", "Estimand", "mean(Y_1 - Y_0)")
                  )),
           column(width = 3,
                  h4("Value of estimand"),
                  verbatimTextOutput("estimand_table")
           ),
           column(width = 2,
                  h5("Quick summary"),
                  dataTableOutput("quick_diagnosis4")
           )
         ))
)

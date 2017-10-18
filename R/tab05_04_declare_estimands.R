tab05_04_declare_estimands <- tabPanel("4. Declare Estimands",
         fluidRow(sidebarPanel(
           textOutput("estimand_chosen")
         )),

         splitLayout(
           sidebarPanel(
             inputPanel(
               radioButtons("estimand_input_type_impl", "Options for Defining Estimand", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
               , width = 8),
             conditionalPanel(condition = "input.estimand_input_type_impl == 'Simple'",
                              radioButtons("estimand_type_impl", label = "Choose a simple estimand:", choices =  list("Population average treatment effect" = "population",
                                                                                                                      "Sample average treatment effect" = "sample"),
                                           selected = "population")
             ),
             conditionalPanel(condition = "input.estimand_input_type_impl == 'Custom'",
                              textInput("estimand_text_impl", "Estimand", "mean(Y_1 - Y_0)")
             ), width = 8),
           mainPanel(
             h4("Value of estimand"),
             verbatimTextOutput("estimand_table_impl")
           )
         )
)

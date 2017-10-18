tab05_02_declare_potential_outcomes <- tabPanel("2. Declare Potential Outcomes",
         fluidRow(sidebarPanel(
           "The potential outcomes formula was declared as", textOutput("potential_outcomes_formula", inline = T), ". The condition names were declared as",
           textOutput("condition_names", inline = T), ". The assignment variable name was declared as", textOutput("assignment_variable_name", inline = T)
         )),
         splitLayout(
           sidebarPanel(
             inputPanel(
               radioButtons("po_input_type_impl", "Options for Defining Potential Outcomes", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
               , width = 8),
             conditionalPanel(condition = "input.po_input_type_impl == 'Simple'",
                              numericInput("po_treat_mean_impl", "Mean outcome in treatment group", 0.5),
                              numericInput("po_control_mean_impl", "Mean outcome in control group", 0)
             ),
             conditionalPanel(condition = "input.po_input_type_impl == 'Custom'",
                              textInput("potential_outcomes_formula_impl", label = "Potential Outcomes Formula", value = "Y ~ 0.1 + 1 * Z + noise"),
                              textInput("condition_names_impl", label = "Condition names (separated by commas)", value = "0, 1"),
                              textInput("assignment_variable_name_impl", label = "Assignment Variable Name", value = "Z")
             ), width = 8),
           mainPanel(
             plotOutput("po_plot_impl")
           )
         )
)



shinyUI(fluidPage(
  theme = "bootstrap.css",

  navbarPage(
    "DeclareDesign",
    tabPanel(
      "Declare",
      fluidRow(),
      tabsetPanel(
        type = "pills",
        tabPanel("Population",
                 fixedPage(
                   wellPanel(
                     "Define the size of the population",
                     numericInput("N", "Number of units", 1000, width = "150px")),
                   wellPanel(
                     "Define pre-treatment covariate",
                     fluidRow(column(4, textInput("var_name", "Variable name", "noise")),
                              column(4, selectInput("var_type", "Variable type", c("Binary (binomial)", "Continuous (normal)", "Custom"))),
                              column(4, conditionalPanel(condition = "input.var_type == 'Binary (binomial)'",
                                                         numericInput(inputId = "p", label = "Probability", value = 0.5, min = 0, max = 1)
                              )),
                              column(4, conditionalPanel(condition = "input.var_type == 'Continuous (normal)'",
                                                         numericInput(inputId = "mu", label = "Mean", value = 5),
                                                         numericInput(inputId = "var", label = "Variance", value = 0.5, min = 0, max = 1)
                              )),
                              column(4, conditionalPanel(condition = "input.var_type == 'Custom'",
                                                         textInput("var_function", "R expression for variable", "rnorm(N)")
                              ))
                     )
                   )
                 )),
        tabPanel("Potential Outcomes",
                 fixedPage(
                   column(4, wellPanel(
                     "Define your potential outcomes",
                     textInput("y0_po", "Y_i(0) potential outcome", "noise"),
                     textInput("y1_po", "Y_i(1) potential outcome", "noise + .25")
                   )),
                   column(width = 4,
                                 plotOutput("po_plot")
                          )
                 )),
        tabPanel("Sampling",
                 fixedPage(
                   wellPanel(
                     "Simple random sampling",
                     numericInput("n", "Number of units sampled", 100)
                   )
                 )),
        tabPanel("Assignment",
                 fixedPage(
                   wellPanel(
                     "Complete random assignment",
                     numericInput("m", "Number of units assigned to treatment", 50)
                   )
                 )),
        tabPanel("Estimation",
                 fixedPage(
                   wellPanel(
                     "Estimand",
                     textInput("estimand_text", "Describe your estimand as a function of potential outcomes and covariates", "mean(Y_Z_1 - Y_Z_0)"),
                     "The current value of the estimand is ",
                     tableOutput("estimand_eval")
                   ),

                   wellPanel(
                     "Estimator",
                     textInput("formula", "Write a formula", "Y ~ Z"),
                     textInput("cond_1", "control condition", 0),
                     textInput("cond_2", "treatment condition", 1),
                     tableOutput("estimate_eval")
                   )
                 ))

      )
    ),
    tabPanel(
      "Draw",
      fluidRow(mainPanel(
        p(
          "In this tab, you can view or download mock data created under the design characterized by you in the first tab. Simulated data like this can help communicate features of a design to others, but can also be useful to explore analyses and imagine the questions that can be answered with a given design. "
        ),
        width = 12
      )),

      fluidRow(sidebarPanel(
        downloadButton('downloadData', 'Download')
      )),
      fluidRow(mainPanel(
        dataTableOutput('current_sample_data_table')
      ))
    ),
    tabPanel("Script",
             verbatimTextOutput("script")),
    tabPanel("Diagnose",
             "pls make")
  )
))

tab01_01_declare_population <-
            tabPanel("1. Declare Population",
                     fixedPage(
                       fixedRow(column(width = 6,
                                       wellPanel(p("Characterize the population: the set of units about which inferences are sought.")),
                                       wellPanel(numericInput("N", "Population size", 100, min = 1)),
                                       wellPanel("Define pre-treatment covariate",
                                                 numericInput("cov_mean", "Mean", 1),
                                                 numericInput("cov_sd", "Std. Dev.", 1),
                                                 textInput("cov_name", "Variable name", "income"))

                       ),
                       column(width = 2,
                              h5("Quick summary"),
                              dataTableOutput("quick_diagnosis1") )
                       ))
            )

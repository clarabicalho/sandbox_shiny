tab01_06_declare_estimators <- tabPanel("5. Declare Estimators",
         fixedPage( fixedRow(
           column(
             wellPanel(p("Characterize the estimator function: the procedure for generating estimates of quantities we want to learn about.")),

             inputPanel(selectInput("estimator", "Estimator",
                                    choices = c("Linear regression" = "estimator_lm",
                                                "Difference-in-means" = "estimator_d_i_m"))
             ), width = 6),
           column(
             h4("Estimates using mock data"),
             dataTableOutput("estimates_table")
             , width = 4),
           column(width = 2,
                  h5("Quick summary"),
                  dataTableOutput("quick_diagnosis6")
           )
         ))
)

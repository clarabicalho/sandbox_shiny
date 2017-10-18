tab05_06_declare_estimators <-
tabPanel("6. Declare Estimators",
         fluidRow(sidebarPanel(
           textOutput("estimator")
         )),
         splitLayout(
           sidebarPanel(
             selectInput("estimator_impl", "Estimator",
                         choices = c("Linear regression" = "estimator_lm",
                                     "Difference-in-means" = "estimator_d_i_m"))
             , width = 8),
           mainPanel(
             h4("Estimates using mock data"),
             dataTableOutput("estimates_table_impl")
           )
         )
)

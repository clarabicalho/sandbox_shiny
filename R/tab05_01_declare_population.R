tab05_01_declare_population <- tabPanel("1. Declare Population",
         fluidRow(sidebarPanel(
           "The population size was declared to be", textOutput("N", inline = T)
         )),
         splitLayout(
           sidebarPanel(
             numericInput("N_impl", "Sample size", 100, min = 1
             ),
             wellPanel("Define pre-treatment covariate", numericInput("cov_mean_impl", "Mean", 1), numericInput("cov_sd_impl", "Std. Dev.", 1),
                       textInput("cov_name_impl", "Variable name", "income"))
           )
         )
)

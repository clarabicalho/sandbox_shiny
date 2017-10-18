tab05_03_declare_sampling<- tabPanel("3. Declare Sampling",
         fluidRow(sidebarPanel(
           textOutput("n")
         )),
         splitLayout(
           sidebarPanel(
             numericInput("n_impl", "Number of units to sample from population", 50, min = 1
             )
           )
         )
)

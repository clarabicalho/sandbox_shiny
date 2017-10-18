tab06_comparison <-
  tabPanel("Comparison",
         fluidRow(mainPanel(p("In this tab, compare the design as registered (characterized in the 'Characterize' tab) to the design you actually implemented (characterized in the 'Implemented Design' tab)."), width = 12)),

         fluidRow(
           inputPanel(
             numericInput("population_draws_compare", "Population draws", 5, min = 1),
             numericInput("sample_draws_compare", "Sample draws", 5, min = 1),
             width = 12)
         ),
         fluidRow( dataTableOutput("diagnosis_table_compare") )
  )

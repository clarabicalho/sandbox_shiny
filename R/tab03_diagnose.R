tab03_diagnose <- tabPanel("Diagnose",
         fluidRow(mainPanel(p("In this tab, learn about the statistical properties of the research design defined in the first tab. The output of the diagnosis (below) is a summary of important statistical properties of the design, including the statistical power, bias, and frequentist coverage (among other uses, an indicator of whether the statistical power is calculated correctly)."), width = 12)),

         fluidRow(
           inputPanel(
             numericInput("population_draws", "Population draws", 5, min = 1),
             numericInput("sample_draws", "Sample draws", 5, min = 1),
             width = 12)
         ),
         fluidRow( mainPanel(dataTableOutput("diagnosis_table"), with = 10) )
)

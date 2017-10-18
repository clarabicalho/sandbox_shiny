tab05_implemented_study <- tabPanel("Implemented Study",
   fluidRow(mainPanel(p("In this tab, you can characterize the research design as it was actually implemented. You will see the design characterized in the first tab above, representing the design as pre-registered."), width = 12)),

   tabsetPanel(type = "pills",
               tab05_01_declare_population,
               tab05_02_declare_potential_outcomes,
               tab05_03_declare_sampling,
               tab05_04_declare_estimands,
               tab05_05_declare_assignment,
               tab05_06_declare_estimators
   )
)

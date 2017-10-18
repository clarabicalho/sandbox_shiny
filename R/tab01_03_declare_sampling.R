tab01_03_declare_sampling <- tabPanel("3. Declare Sampling",
         fixedPage(
           fixedRow(column(width = 6,
                           wellPanel(p("Characterize the sampling strategy: the strategy used to select units to include in the study sample.")),
                           wellPanel( numericInput("n", "Number of units to sample from population", 50, min = 1)
                           )),
                    column(width = 2,
                           h5("Quick summary"),
                           dataTableOutput("quick_diagnosis3")
                    )))
)

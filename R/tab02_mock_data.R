tab02_mock_data <- tabPanel("Mock Data",
    fluidRow(mainPanel(p("In this tab, you can view or download mock data created under the design characterized by you in the first tab. Simulated data like this can help communicate features of a design to others, but can also be useful to explore analyses and imagine the questions that can be answered with a given design. "), width = 12)),

    fluidRow(

      sidebarPanel(
        downloadButton('downloadData', 'Download')
      )
    ),
    fluidRow(
      mainPanel(
        dataTableOutput('current_sample_data_table')
      )
    )
)

tab04_register <- tabPanel("Register",
         fluidRow(mainPanel(p("In this tab, download a filled out template for you to use to pre-register your research design with a design registry such as the EGAP registry."), width = 12)),
         fluidRow(
           sidebarPanel(
             h4("Download pre-analysis plan template for your design"),
             downloadButton('downloadRmd', 'Download (Disabled in Demo Version)'),
             width = 10
           )
         )
)

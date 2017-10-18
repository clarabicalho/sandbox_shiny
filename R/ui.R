
ui <- fluidPage(theme = "cerulean",

                  navbarPage(
                    "DeclareDesign DEMO",
                    tabPanel("Characterize",
                             fluidRow(mainPanel(a(href = "http://alpha.declaredesign.org", "DeclareDesign"), "is a tool for formally characterizing, diagnosing, and pre-registering research designs.", p("In this tab, you can characterize an experimental research design in six steps, from the population data-generating process to the estimators used in analysis. Characterizing designs prior to conducting a study increases the potential for learning. For one thing, it can enable identification of low-powered or highly biased strategies, avoiding false negatives. Furthermore, it wards against the risk of falsely inferring significant results where there are none. By clarifying the set of choices made prior to analysis and publicly pre-registering them, design characterization can enable comparison between planned choices and implemented choices, and can enhance confidence in findings among the scientific community."), width = 12)),
                    tab01_characterize,
                    tab02_mock_data,
                    tab03_diagnose,
                    tab04_register,
                    tab05_implemented_study,
                    tab06_comparison,
                    tab07_about
                    )
                  )
)

tab01_characterize <- tabPanel("Characterize",
         fluidRow(mainPanel(a(href = "http://alpha.declaredesign.org", "DeclareDesign"), "is a tool for formally characterizing, diagnosing, and pre-registering research designs.", p("In this tab, you can characterize an experimental research design in six steps, from the population data-generating process to the estimators used in analysis. Characterizing designs prior to conducting a study increases the potential for learning. For one thing, it can enable identification of low-powered or highly biased strategies, avoiding false negatives. Furthermore, it wards against the risk of falsely inferring significant results where there are none. By clarifying the set of choices made prior to analysis and publicly pre-registering them, design characterization can enable comparison between planned choices and implemented choices, and can enhance confidence in findings among the scientific community."), width = 12)),
         tabsetPanel(type = "pills",
                     tab01_01_declare_population,
                     tab01_02_declare_potential_outcomes,
                     tab01_03_declare_sampling,
                     tab01_04_declare_estimands,
                     tab01_05_declare_assignment,
                     tab01_06_declare_estimators
                     )
)


wizard.ui2 <- fluidPage(theme = "cerulean",
                navbarPage(
                  "DeclareDesign DEMO",
                  tab01_characterize,
                  tab02_mock_data,
                  tab03_diagnose,
                  tab04_register,
                  tab05_implemented_study,
                  tab06_comparison,
                  tab07_about
                )
)


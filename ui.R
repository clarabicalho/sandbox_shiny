library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = "cerulean",
                  
                  navbarPage(
                    "DeclareDesign DEMO",
                    tabPanel("Characterize", 
                             fluidRow(mainPanel(a(href = "http://alpha.declaredesign.org", "DeclareDesign"), "is a tool for formally characterizing, diagnosing, and pre-registering research designs.", p("In this tab, you can characterize an experimental research design in six steps, from the population data-generating process to the estimators used in analysis. Characterizing designs prior to conducting a study increases the potential for learning. For one thing, it can enable identification of low-powered or highly biased strategies, avoiding false negatives. Furthermore, it wards against the risk of falsely inferring significant results where there are none. By clarifying the set of choices made prior to analysis and publicly pre-registering them, design characterization can enable comparison between planned choices and implemented choices, and can enhance confidence in findings among the scientific community."), width = 12)),
                             tabsetPanel(type = "pills",
                               tabPanel("1. Declare Population",
                                            fixedPage(
                                              fixedRow(column(width = 6, 
                                                              wellPanel(p("Characterize the population: the set of units about which inferences are sought.")),
                                             wellPanel(numericInput("N", "Population size", 100, min = 1)),
                                             wellPanel("Define pre-treatment covariate", numericInput("cov_mean", "Mean", 1), numericInput("cov_sd", "Std. Dev.", 1), 
                                                         textInput("cov_name", "Variable name", "income"))
                                             
                                             ),
                                             column(width = 2,
                                                    h5("Quick summary"),
                                                    dataTableOutput("quick_diagnosis1") )
                                             ))
                               ),
                               tabPanel("2. Declare Potential Outcomes",
                                        fixedPage(
                                          fixedRow(
                                            column(width = 4,
                                            wellPanel("Characterize the potential outcomes function: the outcomes that each unit might exhibit depending on how the causal process being studied changes the world."),
                                            inputPanel(
                                              radioButtons("po_input_type", "Options for Defining Potential Outcomes", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                                              ),
                                            conditionalPanel(condition = "input.po_input_type == 'Simple'",
                                                             wellPanel(numericInput("po_treat_mean", "Mean outcome in treatment group", 0.5),
                                                                       numericInput("po_control_mean", "Mean outcome in control group", 0))
                                            ),
                                            conditionalPanel(condition = "input.po_input_type == 'Custom'",
                                                             wellPanel(textInput("potential_outcomes_formula", label = "Potential Outcomes Formula", value = "Y ~ 0.1 + 1 * Z + noise"),
                                                             textInput("condition_names", label = "Condition names (separated by commas)", value = "0, 1"),
                                                             textInput("assignment_variable_name", label = "Assignment Variable Name", value = "Z"))
                                            )
                                            ),
                                          column(width = 4, 
                                            plotOutput("po_plot")
                                          ),
                                          column(width = 2,
                                                 h5("Quick summary"),
                                                 dataTableOutput("quick_diagnosis2")
                                          )
                                        ))
                               ),
                               tabPanel("3. Declare Sampling",
                                        fixedPage(
                                          fixedRow(column(width = 6, 
                                                          wellPanel(p("Characterize the sampling strategy: the strategy used to select units to include in the study sample.")),
                                                          wellPanel( numericInput("n", "Number of units to sample from population", 50, min = 1)
                                                                     )),
                                                   column(width = 2,
                                                          h5("Quick summary"),
                                                          dataTableOutput("quick_diagnosis3")
                                                   )))
                               ),
                               tabPanel("4. Declare Estimands",
                                        fixedPage(fixedRow(
                                          column(width = 6,
                                                 wellPanel(p("Characterize the estimands: the specification of the things that we want to learn about the world, described in
terms of potential outcomes.")),
                                            inputPanel(
                                              radioButtons("estimand_input_type", "Options for Defining Estimand", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                                              ),
                                            conditionalPanel(condition = "input.estimand_input_type == 'Simple'",
                                                             radioButtons("estimand_type", label = "Choose a simple estimand:", choices =  list("Population average treatment effect" = "population",
                                                                                                                                                "Sample average treatment effect" = "sample"),
                                                                          selected = "population")
                                            ),
                                            conditionalPanel(condition = "input.estimand_input_type == 'Custom'",
                                                             textInput("estimand_text", "Estimand", "mean(Y_1 - Y_0)")
                                            )),
                                          column(width = 3,
                                            h4("Value of estimand"),
                                            verbatimTextOutput("estimand_table")
                                          ),
                                          column(width = 2,
                                                 h5("Quick summary"),
                                                 dataTableOutput("quick_diagnosis4")
                                          )
                                        ))
                               ),
                               tabPanel("5. Declare Assignment",
                                        fixedPage(fixedRow(
                                          
                                          column(width = 6,
                                                 wellPanel(p("Characterize the assignment function: the manner in which units are assigned to reveal one potential outcome or another.")),
                                            inputPanel(
                                              radioButtons("assignment_input_type", "Options for Defining Assignment", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                                              ),
                                            conditionalPanel(condition = "input.assignment_input_type == 'Simple'",
                                                             wellPanel(numericInput("prob_assign", "Proportion of units to assign to treatment", 0.5, min = 0, max = 1))
                                            ),
                                            conditionalPanel(condition = "input.assignment_input_type == 'Custom'",
                                                             wellPanel(textInput("custom_assignment_function", label = "Custom assignment function"))
                                            )),
                                          
                                          column(width = 4,
                                            dataTableOutput('treatment_table')
                                          ),
                                          column(width = 2,
                                                 h5("Quick summary"),
                                                 dataTableOutput("quick_diagnosis5")
                                          )
                                        ))
                               ),
                               tabPanel("5. Declare Estimators",
                                        fixedPage( fixedRow(
                                          column(
                                            wellPanel(p("Characterize the estimator function: the procedure for generating estimates of quantities we want to learn about.")),
                                            
                                            inputPanel(selectInput("estimator", "Estimator", 
                                                        choices = c("Linear regression" = "estimator_lm", 
                                                                    "Difference-in-means" = "estimator_d_i_m"))
                                            ), width = 6),
                                          column(
                                            h4("Estimates using mock data"),
                                            dataTableOutput("estimates_table")
                                          , width = 4),
                                          column(width = 2,
                                                 h5("Quick summary"),
                                                 dataTableOutput("quick_diagnosis6")
                                          )
                                        ))
                               ))),
                    tabPanel("Mock Data",
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
                    ),
                    tabPanel("Diagnose",
                             fluidRow(mainPanel(p("In this tab, learn about the statistical properties of the research design defined in the first tab. The output of the diagnosis (below) is a summary of important statistical properties of the design, including the statistical power, bias, and frequentist coverage (among other uses, an indicator of whether the statistical power is calculated correctly)."), width = 12)),
                             
                             fluidRow(
                               inputPanel(
                                 numericInput("population_draws", "Population draws", 5, min = 1),
                                 numericInput("sample_draws", "Sample draws", 5, min = 1),
                                 width = 12)
                             ),
                             fluidRow( mainPanel(dataTableOutput("diagnosis_table"), with = 10) )
                    ),
                    tabPanel("Register",
                             fluidRow(mainPanel(p("In this tab, download a filled out template for you to use to pre-register your research design with a design registry such as the EGAP registry."), width = 12)),
                             fluidRow(
                               sidebarPanel(
                                 h4("Download pre-analysis plan template for your design"),
                                 downloadButton('downloadRmd', 'Download (Disabled in Demo Version)'),
                                 width = 10
                               )
                             )
                    ),
                    tabPanel("Implemented Study",
                             fluidRow(mainPanel(p("In this tab, you can characterize the research design as it was actually implemented. You will see the design characterized in the first tab above, representing the design as pre-registered."), width = 12)),
                             
                             tabsetPanel(type = "pills",
                                 tabPanel("1. Declare Population",
                                          fluidRow(sidebarPanel( 
                                            "The population size was declared to be", textOutput("N", inline = T)
                                          )),
                                          splitLayout(
                                            sidebarPanel(
                                              numericInput("N_impl", "Sample size", 100, min = 1
                                              ),
                                              wellPanel("Define pre-treatment covariate", numericInput("cov_mean_impl", "Mean", 1), numericInput("cov_sd_impl", "Std. Dev.", 1), 
                                                        textInput("cov_name_impl", "Variable name", "income"))
                                            )
                                          )
                                 ),
                                 tabPanel("2. Declare Potential Outcomes",
                                          fluidRow(sidebarPanel( 
                                            "The potential outcomes formula was declared as", textOutput("potential_outcomes_formula", inline = T), ". The condition names were declared as", 
                                            textOutput("condition_names", inline = T), ". The assignment variable name was declared as", textOutput("assignment_variable_name", inline = T)
                                          )),
                                          splitLayout(
                                            sidebarPanel(
                                              inputPanel(
                                                radioButtons("po_input_type_impl", "Options for Defining Potential Outcomes", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                                                , width = 8),
                                              conditionalPanel(condition = "input.po_input_type_impl == 'Simple'",
                                                               numericInput("po_treat_mean_impl", "Mean outcome in treatment group", 0.5),
                                                               numericInput("po_control_mean_impl", "Mean outcome in control group", 0)
                                              ),
                                              conditionalPanel(condition = "input.po_input_type_impl == 'Custom'",
                                                               textInput("potential_outcomes_formula_impl", label = "Potential Outcomes Formula", value = "Y ~ 0.1 + 1 * Z + noise"),
                                                               textInput("condition_names_impl", label = "Condition names (separated by commas)", value = "0, 1"),
                                                               textInput("assignment_variable_name_impl", label = "Assignment Variable Name", value = "Z")
                                              ), width = 8),
                                            mainPanel(
                                              plotOutput("po_plot_impl")
                                            )
                                          )
                                 ),
                                 tabPanel("3. Declare Sampling",
                                          fluidRow(sidebarPanel( 
                                            textOutput("n")
                                          )),
                                          splitLayout(
                                            sidebarPanel(
                                              numericInput("n_impl", "Number of units to sample from population", 50, min = 1
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("4. Declare Estimands",
                                          fluidRow(sidebarPanel( 
                                            textOutput("estimand_chosen")
                                          )),
                                          
                                          splitLayout(
                                            sidebarPanel(
                                              inputPanel(
                                                radioButtons("estimand_input_type_impl", "Options for Defining Estimand", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                                                , width = 8),
                                              conditionalPanel(condition = "input.estimand_input_type_impl == 'Simple'",
                                                               radioButtons("estimand_type_impl", label = "Choose a simple estimand:", choices =  list("Population average treatment effect" = "population",
                                                                                                                                                       "Sample average treatment effect" = "sample"),
                                                                            selected = "population")
                                              ),
                                              conditionalPanel(condition = "input.estimand_input_type_impl == 'Custom'",
                                                               textInput("estimand_text_impl", "Estimand", "mean(Y_1 - Y_0)")
                                              ), width = 8),
                                            mainPanel(
                                              h4("Value of estimand"),
                                              verbatimTextOutput("estimand_table_impl")
                                            )
                                          )
                                 ),
                                 tabPanel("5. Declare Assignment",
                                          splitLayout(
                                            
                                            
                                            sidebarPanel(
                                              fluidRow(inputPanel( 
                                                textOutput("assignment_summary")
                                              ), width = 8),
                                              inputPanel(
                                                radioButtons("assignment_input_type_impl", "Options for Defining Assignment", c("Simple", "Custom"), selected = "Simple", inline = FALSE, width = NULL)
                                                , width = 8),
                                              conditionalPanel(condition = "input.assignment_input_type_impl == 'Simple'",
                                                               numericInput("prob_assign_impl", "Proportion of units to assign to treatment", 0.5, min = 0, max = 1)
                                              ),
                                              conditionalPanel(condition = "input.assignment_input_type_impl == 'Custom'",
                                                               textInput("custom_assignment_function_impl", label = "Custom assignment function")
                                              ), width = 8),
                                            
                                            mainPanel(
                                              dataTableOutput('treatment_table_impl')
                                            )
                                          )
                                 ),
                                 tabPanel("5. Declare Estimators",
                                          fluidRow(sidebarPanel( 
                                            textOutput("estimator")
                                          )),
                                          splitLayout(
                                            sidebarPanel(
                                              selectInput("estimator_impl", "Estimator", 
                                                          choices = c("Linear regression" = "estimator_lm", 
                                                                      "Difference-in-means" = "estimator_d_i_m"))
                                              , width = 8),
                                            mainPanel(
                                              h4("Estimates using mock data"),
                                              dataTableOutput("estimates_table_impl")
                                            )
                                          )
                                 )
                             
                    )),
                    tabPanel("Comparison",
                             fluidRow(mainPanel(p("In this tab, compare the design as registered (characterized in the 'Characterize' tab) to the design you actually implemented (characterized in the 'Implemented Design' tab)."), width = 12)),
                             
                             fluidRow(
                               inputPanel(
                                 numericInput("population_draws_compare", "Population draws", 5, min = 1),
                                 numericInput("sample_draws_compare", "Sample draws", 5, min = 1),
                                 width = 12)
                             ),
                             fluidRow( dataTableOutput("diagnosis_table_compare") )
                    ),
                    tabPanel("About",
                             fixedPage(
                               fixedRow(
                                 column(width = 6,
                                        h5("About the DeclareDesign Web version"),
                                        p("DeclareDesign provides a consistent language for designing, diagnosing, and registering research designs. We hope that the use of this software will enable scholars to easily adopt widely agreed upon best practices for scientific research."),
                                        p("The Web version of DeclareDesign provides a way for scholars with no programming ability to access the functionality of the software. The design characterization, mock data, diagnosis, and registration facilities are all available through an easy-to-use interface."),
                                        p("NOTE: This demonstration version provides a limited preview of future Web-based functionality, and illustrates only a limited set of the features of the statistical software itself.")
                                 )
                               )
                             )
                    )
                    
                    
                  )))
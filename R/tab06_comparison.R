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

tab06_make_diagnosis_comparison <- function(current_diagnosis, current_diagnosis_impl){
  diag_tab <- round_df(rbind(current_diagnosis$diagnosands, current_diagnosis_impl$diagnosands), 4)
  diag_tab <- cbind(colnames(diag_tab), t(diag_tab))
  colnames(diag_tab) <- c("","Registered", "Implemented")
  diag_tab
}


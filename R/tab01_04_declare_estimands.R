tab01_04_declare_estimands <- tabPanel("4. Declare Estimands",
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
)

simple_estimand <- list(
  population=declare_estimand(ATE=mean(Y_Z_1 - Y_Z_0)),
  sample=declare_estimand(ATE_s=mean(Y_Z_1 - Y_Z_0))
)


tab01_custom_estimand <- function(estimand_text){
  ret <- tryCatch({
    e <- parse(text=estimand_text)[[1]]

    ret <- eval(call("declare_estimand",Custom=e))
  }, error=function(e) data.frame()) # returning noop estimand
  structure(ret, custom=TRUE)
}

tab01_current_estimand <- function(estimand_input_type, estimand_type, estimand_text){
  if(estimand_input_type == 'Simple'){
    if(!estimand_type %in% names(simple_estimand)) warning('invalid simple estimand selected !?')
    ret <- simple_estimand[[estimand_type]]
  } else if(estimand_input_type == 'Custom'){
    ret <- tab01_custom_estimand(estimand_text)
  }
  ret
}

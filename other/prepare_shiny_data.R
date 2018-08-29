library(DesignLibrary)
library(ShinyDeclareDesign)

functions <- ls("package:DesignLibrary")
designers <- functions[grepl("_designer\\b",functions)]
sims <- 10
bootstraps <- 10

for(designer in designers){

  the_designer <- get(x = designer)
  has_shiny <- !is.null(attributes(the_designer)$shiny_arguments)
  designer_args <- formals(the_designer)
  designer_attr <- attributes(the_designer)
  one_design <- the_designer()
  design_attr <- attributes(one_design)


  if(has_shiny){
    get_or_run_shiny_diagnosis(
      designer = the_designer,
      designer_name = designer,
      sims = sims,
      bootstrap = bootstraps,
      update_existing = TRUE
    )
  }
  print(designer)
}


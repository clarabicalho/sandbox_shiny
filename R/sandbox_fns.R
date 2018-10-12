
library(DeclareDesign)
library(rlang)

available <- function(design_so_far, steps, causal_type) {
  if(!is.list(steps)) "Please provide steps in a list"
  right_type <- sapply(1:length(steps), function(j) causal_type %in% attr(steps[[j]], "causal_type"))
  steps <- steps[right_type]
  var_names <- names(draw_data(design_so_far))
  accept <- sapply(1:length(steps), function(j)  all(attr(steps[[j]], "expects") %in% var_names))
  if(all(!accept)) return("No steps available")
  steps[accept]
}

# Here are some steps

population_1 <- function(N = 10) eval_bare(expr(declare_population(N = !!N, W = runif(!!N), X = runif(!!N))))
attr(population_1, "label") <- deparse(substitute(population_1))
attr(population_1, "description") <- "N obs, W and X cov"
attr(population_1, "step_type") <- "population"

population_2 <- function(N = 10) eval_bare(expr(declare_population(N = !!N, Y = runif(!!N), Z = runif(!!N))))
attr(population_2, "label") <- deparse(substitute(population_2))
attr(population_2, "description") <- "N obs, Y and Z cov"
attr(population_2, "step_type") <- "population"

population_3 <- function(N = 10, blocks = 2) eval_bare(expr(declare_population(N = !!N, Y = runif(!!N), Z = runif(!!N), blocks = rep(1:!!blocks, !!N/!!blocks))))
attr(population_3, "label") <- deparse(substitute(population_3))
attr(population_3, "description") <- "N obs w/ blocks, Y and Z cov"
attr(population_3, "step_type") <- "population"

estimator_dim_01 <- declare_estimator(Y~Z)
attr(estimator_dim_01, "expects") <- c("Y", "Z")
attr(estimator_dim_01, "label") <- deparse(substitute(estimator_dim_01))
attr(estimator_dim_01, "description") <- "Y ~ Z (d-i-m)"

estimator_lmr_01 <- declare_estimator(Y~Z, model = lm_robust, fixed_effects ~ blocks)
attr(estimator_lmr_01, "expects") <- c("Y", "Z", "blocks")
attr(estimator_lmr_01, "label") <- deparse(substitute(estimator_lmr_01))
attr(estimator_lmr_01, "description") <- "Y ~ Z w/ blocks FE (linear)"

fabricate_01 <- declare_step(W = 1 - Y, handler = fabricate)
attr(fabricate_01, "expects") <- c("Y")

all_steps = list(estimator_dim_01, estimator_lmr_01, fabricate_01)
sandbox <- list(population_1, population_2, population_3, estimator_dim_01, estimator_lmr_01)

# Here are three designs-so-far
D1 <- declare_population(N = 10, W = runif(N), X = runif(N)) + NULL
D2 <- declare_population(N = 10, Y = runif(N), Z = runif(N)) + NULL
D3 <- declare_population(N = 10, Y = runif(N), Z = runif(N), blocks = rep(0:1, 5)) + NULL

# This lists what steps are possible after each design-so-far
available(D1, all_steps, "estimator")
available(D2, all_steps, "estimator")
available(D3, all_steps, "estimator")
available(D1, all_steps, "dgp")
available(D2, all_steps, "dgp")
available(D3, all_steps, "dgp")

# Helper functions
# Author: Dean Atali (https://github.com/daattali/advanced-shiny/blob/master/update-input/update-input.R)
# Update multiple Shiny inputs simultaneously
updateShinyInputs <- function(session, updates) {
  lapply(names(updates), function(id) {
    updateShinyInput(session, id, updates[[id]])
  })
}

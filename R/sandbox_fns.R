
library(DeclareDesign)
library(rlang)

rm(list = ls())

# available <- function(design_so_far, steps, causal_type) {
#   if(!is.list(steps)) "Please provide steps in a list"
#   right_type <- sapply(1:length(steps), function(j) causal_type %in% attr(steps[[j]], "causal_type"))
#   steps <- steps[right_type]
#   var_names <- names(draw_data(design_so_far))
#   accept <- sapply(1:length(steps), function(j)  all(attr(steps[[j]], "expects") %in% var_names))
#   if(all(!accept)) return("No steps available")
#   steps[accept]
# }


# population steps --------------------------------------------------------

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

# assignment steps --------------------------------------------------------

# Declare assignment of m units to treatment
assignment_1 <- function(m = 50) eval_bare(expr(declare_assignment(m = !!m)))
attr(assignment_1, "label") <- deparse(substitute(assignment_1))
attr(assignment_1, "description") <- "M units"
attr(assignment_1, "step_type") <- "assignment"

# Declare assignment specifying assignment probability for each block
assignment_2 <- function(block_prob = "1/3, 2/3", blocks_var = "block"){
  eval_bare(expr({
    declare_assignment(block_prob = !!parse(text = paste0("c(","1/3, 2/3",")")),
                       blocks = !!sym(blocks_var))
  }))
}
attr(assignment_2, "label") <- deparse(substitute(assignment_2))
attr(assignment_2, "description") <- "Block"
attr(assignment_2, "step_type") <- "assignment"


# Declare assignment of clusters with probability 1/4
assignment_3 <- function(probability = .5, cluster_var = "cluster", assignment_var = "Z"){
  eval_bare(expr({
    declare_assignment(prob = !!probability,
                       clusters = !!sym(cluster_var),
                       assignment_variable = !!assignment_var)
  }))
  }
attr(assignment_3, "label") <- deparse(substitute(assignment_3))
attr(assignment_3, "description") <- "Cluster"
attr(assignment_3, "step_type") <- "assignment"

# Multiarm
assignment_4 <- function(m_arms = 2) eval_bare(expr(declare_assignment(conditions = 1:!!m_arms)))
attr(assignment_4, "label") <- deparse(substitute(assignment_4))
attr(assignment_4, "description") <- "Multi-arm"
attr(assignment_4, "step_type") <- "assignment"

# Declare assignment using custom handler
#
# custom_assignment <- function(data, assignment_variable = "X") {
#   data[, assignment_variable] <- rbinom(n = nrow(data),
#                                         size = 1,
#                                         prob = 0.5)
#   data
# }


# other steps -------------------------------------------------------------

add_dv <- function(lambda = .9){
  eval_bare(expr({
    declare_step(handler = function(data) {
      within(data, {Y <- NA
      Y[t == 1] <- 1 + u_i[t == 1]
      for(j in 2:max(t))  {Y[t==j] <- (1-!!lambda) + !!lambda*Y[t==(j - 1)] + u_i[t==j]}
      })})})
    )
}

attr(add_dv, "label") <- deparse(substitute(add_dv))
attr(add_dv, "description") <- "Potential outcomes in panel data"
attr(add_dv, "requires") <- c("t", "u_i")
attr(add_dv, "step_type") <- "potential_outcomes"

# fabricate_01 <- declare_step(W = 1 - Y, handler = fabricate)
# attr(fabricate_01, "expects") <- c("Y")

# estimator steps ---------------------------------------------------------

estimator_dim_01 <- declare_estimator(Y~Z)
attr(estimator_dim_01, "expects") <- c("Y", "Z")
attr(estimator_dim_01, "label") <- deparse(substitute(estimator_dim_01))
attr(estimator_dim_01, "description") <- "Y ~ Z (d-i-m)"

estimator_lmr_01 <- declare_estimator(Y~Z, model = lm_robust, fixed_effects ~ blocks)
attr(estimator_lmr_01, "expects") <- c("Y", "Z", "blocks")
attr(estimator_lmr_01, "label") <- deparse(substitute(estimator_lmr_01))
attr(estimator_lmr_01, "description") <- "Y ~ Z w/ blocks FE (linear)"

# create sandbox ----------------------------------------------------------

# all_steps = list(estimator_dim_01, estimator_lmr_01, fabricate_01)
sandbox <- do.call(list, mget(setdiff(names(.GlobalEnv), ".Random.seed")))
# sandbox <- list(population_1, population_2, population_3, estimator_dim_01, estimator_lmr_01)

# # Here are three designs-so-far
# D1 <- declare_population(N = 10, W = runif(N), X = runif(N)) + NULL
# D2 <- declare_population(N = 10, Y = runif(N), Z = runif(N)) + NULL
# D3 <- declare_population(N = 10, Y = runif(N), Z = runif(N), blocks = rep(0:1, 5)) + NULL
#
# # This lists what steps are possible after each design-so-far
# available(D1, all_steps, "estimator")
# available(D2, all_steps, "estimator")
# available(D3, all_steps, "estimator")
# available(D1, all_steps, "dgp")
# available(D2, all_steps, "dgp")
# available(D3, all_steps, "dgp")

# Helper functions
# Author: Dean Atali (https://github.com/daattali/advanced-shiny/blob/master/update-input/update-input.R)
# Update multiple Shiny inputs simultaneously
# updateShinyInputs <- function(session, updates) {
#   lapply(names(updates), function(id) {
#     updateShinyInput(session, id, updates[[id]])
#   })
# }

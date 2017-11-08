
two_way_template <- function(N=c(30,100,500, 1000),
                             beta_A=c(-1,0,1),
                             beta_B=c(-1,0,1),
                             gamma_AB=c(-1,-.5,0,.5,1))
{
  N <- match.arg(N)
  beta_A <- match.arg(beta_A)
  beta_B <- match.arg(beta_B)
  gamma_AB <- match.arg(gamma_AB)



  my_population <- declare_population(N = N, noise = rnorm(N))

  my_potential_outcomes <- declare_potential_outcomes(Y_Z_T1 = noise,
                                                      Y_Z_T2 = noise + beta_A,
                                                      Y_Z_T3 = noise + beta_B,
                                                      Y_Z_T4 = noise + gamma_AB)

  my_assignment <- declare_assignment(num_arms = 4)

  my_estimand <- declare_estimand(interaction = mean(Y_Z_T4 - Y_Z_T3) - mean(Y_Z_T2 - Y_Z_T1))

  my_estimator <- declare_estimator(Y ~ Z1 + Z2 + Z1*Z2,
                                    model = lm_robust,
                                    coefficient_name = "Z1:Z2")

  my_design <-
    declare_design(
      my_population,
      my_potential_outcomes,
      my_estimand,
      my_assignment,
      dplyr::mutate(Z1 = as.numeric(Z %in% c("T2", "T4")),
                    Z2 = as.numeric(Z %in% c("T3", "T4"))),
      reveal_outcomes,
      my_estimator
    )

  my_design
}
attr(two_arm_template, "tips") <- c(
  "N"="Size of population",
  "beta_A"="Main effect of A",
  "beta_B"="Main effect of B",
  "gamma_AB"="Interaction effect of A and B"
)

saveRDS(two_way_template, "~/two_way.RDS")

#################

declare_and_diagnose_memo <- memoise(
  function(...){
  dec <- two_way_template(...)
  diag <- diagnose_design(dec)
  code <- deparse(pryr::substitute_q(body(two_way_template), list(...)))
  code <- paste(code[grep('match.arg', code, invert = TRUE)], collapse='\n')
  list(dec, diag, code)
  },
 cache = cache_filesystem("~/cache/two_way"))


v <- lapply(formals(two_way_template), eval)

combos <- do.call(expand.grid, v)


for(i in 1:nrow(combos)) {
  message(i," of ", nrow(combos), "\n")
  d <- do.call(declare_and_diagnose_memo, combos[i,,drop=FALSE])
  # diagnose_memo()

}

designer <- function(N=c(30,100,500, 1000),
                     beta_A=c(-1,0,1),
                     beta_B=c(-1,0,1),
                     gamma_AB=c(-1,-.5,0,.5,1)) {
  d <- declare_and_diagnose_memo(N=N, beta_A=beta_A, beta_B=beta_B, gamma_AB=gamma_AB)
  structure(d[[1]], diagnosis=d[[2]], code=d[[3]])

}

diagnoser <- function(design, ...) {

  attr(design, "diagnosis")

}

save(declare_and_diagnose_memo, designer, diagnoser, two_way_template, file="~/two_way_memo.Rdata")





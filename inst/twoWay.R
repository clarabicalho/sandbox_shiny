
two_way_template <- function(N=c(30,100,500, 1000),
                             beta_A=c(-1,0,-1),
                             beta_B=c(-1,0,-1),
                             gamma_AB=c(-1,-.5,0,.5,1))
{
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

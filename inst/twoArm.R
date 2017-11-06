



two_arm_template <- function(N=500, n=250, m= 25, tau_1=2, sigma_1=2) {

my_population <- declare_population(N = N, noise = rnorm(N))

my_potential_outcomes <- declare_potential_outcomes(Y_Z_0 = noise, Y_Z_1 = noise + rnorm(N, mean = tau_1, sd = sigma_1))

my_sampling <- declare_sampling(n = n)

my_assignment <- declare_assignment(m = 25)

my_estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

my_estimator <- declare_estimator(Y ~ Z, estimand = my_estimand)

design <- declare_design(my_population,
                         my_potential_outcomes,
                         my_sampling,
                         my_estimand,
                         dplyr::mutate(q = 5),
                         dplyr::mutate(q = 6),
                         my_assignment,
                         reveal_outcomes,
                         my_estimator)
design
}
attr(two_arm_template, "tips") <- c(
  "N"="Size of population",
  "n"="Size of sample",
  "m"="Num under treatment 1",
  "tau_1" = "shift in mean for treatment",
  "sigma_1" = "extra variation in treatment"

)

saveRDS(two_arm_template, "~/two_arm.RDS")


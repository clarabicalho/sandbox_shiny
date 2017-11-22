# And the lord said let there be autocomplete
DECLARE_POPULATION="declare_population"
DECLARE_POTENTIAL_OUTCOMES="declare_potential_outcomes"
DECLARE_SAMPLING="declare_sampling"
DECLARE_ESTIMAND="declare_estimand"
DECLARE_ASSIGNMENT="declare_assignment"
REVEAL_OUTCOMES="reveal_outcomes"
DECLARE_ESTIMATOR="declare_estimator"

mk_step <- function(type, args) list(type=type, args=args)
default_builder <- list(mk_step(DECLARE_POPULATION,         '`N=100`,noise=rnorm(N)'),
                        mk_step(DECLARE_POTENTIAL_OUTCOMES, 'Y_Z_0=noise, Y_Z_1=noise+1'),
                        mk_step(DECLARE_ESTIMAND,           'ATE=mean(Y_Z_1 - Y_Z_0), label="ATE"'),
                        mk_step(DECLARE_SAMPLING,           '`n=20`'),
                        mk_step(DECLARE_ASSIGNMENT,         '`m=10`'),
                        mk_step(REVEAL_OUTCOMES,            ''),
                        mk_step(DECLARE_ESTIMATOR,          'Y~Z, estimand="ATE"'))


steps_order <- c(
  DECLARE_POPULATION,
  DECLARE_POTENTIAL_OUTCOMES,
  DECLARE_SAMPLING,
  DECLARE_ESTIMAND,
  DECLARE_ASSIGNMENT,
  REVEAL_OUTCOMES,
  DECLARE_ESTIMATOR
)


step_obj <- list()




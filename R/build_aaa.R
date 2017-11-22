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


steps_funs <- c(
  "Population"         = DECLARE_POPULATION,
  "Potential outcomes" = DECLARE_POTENTIAL_OUTCOMES,
  "Sampling"           = DECLARE_SAMPLING,
  "Estimand"           = DECLARE_ESTIMAND,
  "Assignment"         = DECLARE_ASSIGNMENT,
  "Reveal outcomes"    = REVEAL_OUTCOMES,
  "Estimator"          = DECLARE_ESTIMATOR)

steps_labels <- setNames(names(steps_funs), steps_funs)

steps_dynamic <- list()

step_obj <- list()

steps_config <- list(
  "declare_population"="Population",
  "declare_potential_outcomes"="",
  "declare_estimand"="",
  "declare_assignment"="",
  "reveal_outcomes"="",
  "declare_estimator"="")


step_help_text = list(
  "declare_population" = shiny::tags$div(
    shiny::tags$h5("Declare the Size and Features of the Population"),
    shiny::tags$dl(
      shiny::tags$dt("N"),
      shiny::tags$dd("number of units to draw. If provided as fabricate(N = 5), this determines the number of units in the single-level data. If provided in level, i.e. fabricate(cities = level(N = 5)), N determines the number of units in a specific level of a hierarchical dataset."),
      shiny::tags$dt("ID_label"),
      shiny::tags$dd("(optional) variable name for ID variable, i.e. citizen_ID")
    )
  ),
  "declare_potential_outcomes"=shiny::tags$div(
    shiny::tags$h5("Declare Potential Outcomes"),
    shiny::tags$dl(
      shiny::tags$dt("formula"),
      shiny::tags$dd("eg formula = Y ~ .25 * Z + .01 * age * Z"),
      shiny::tags$dt("assignment_variable_name"),
      shiny::tags$dd("(optional) variable name for Outcomes (Z)"),
      shiny::tags$dt("condition_names"),
      shiny::tags$dd("(optional) conditions the assignment may take")
    )
  ),
  "declare_estimand"=shiny::tags$div(
    shiny::tags$h5("Declare Estimand"),
    shiny::tags$dl(
      shiny::tags$dt("..."),
      shiny::tags$dd("Named estimands"),
      shiny::tags$dt("subset"),
      shiny::tags$dd("(optional) A subset to calculate the estimand on"),
      shiny::tags$dt("label"),
      shiny::tags$dd("(optional) A label for the estimand if not specified in ...")
    )

  ),
  "declare_assignment"=shiny::tags$div(
    shiny::tags$h5("Declare Assignment"),
    shiny::tags$dl(
      shiny::tags$dt("m"),
      shiny::tags$dd(	"Use for a two-arm design in which m units (or clusters) are assigned to treatment and N-m units (or clusters) are assigned to control. In a blocked design, exactly m units in each block will be treated. (optional)"),
      shiny::tags$dt("m-each"),
      shiny::tags$dd("Use for a multi-arm design in which the values of m_each determine the number of units (or clusters) assigned to each condition. m_each must be a numeric vector in which each entry is a nonnegative integer that describes how many units (or clusters) should be assigned to the 1st, 2nd, 3rd... treatment condition. m_each must sum to N. (optional)"),
      shiny::tags$dt("label"),
      shiny::tags$dd("(optional) A label for the estimand if not specified in ...")
    )
  ),
  "reveal_outcomes"=shiny::tags$div(


  ),
  "declare_estimator"=shiny::tags$div(


  )
)


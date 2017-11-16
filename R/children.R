
children.ui <- local({

  sidebarPanel <- sidebarPanel(
    sliderInput("N_families", "Number of Families in Population", 1, 1000, 100, 1),
    sliderInput("k_children", "Number of children to sample", 1, 100, 20, 1),
    div(class="container",
      sliderInput("children_0", "Rel Prop of 0 children", 0, 10, 2, .1, width='30%'),
      sliderInput("children_1", "Rel Prop of 1 children", 0, 10, 2, .1, width='30%'),
      sliderInput("children_2", "Rel Prop of 2 children", 0, 10, 2, .1, width='30%'),
      sliderInput("children_3", "Rel Prop of 3 children", 0, 10, 2, .1, width='30%'),
      sliderInput("children_4", "Rel Prop of 4 children", 0, 10, 2, .1, width='30%')
    ),
    radioButtons("estimand", "Population estimand of interest", c("Mean Num of Children"='fam', "Mean Num of Children (>0)"='fam.trunc', 'Mean Num of Siblings'='child.sib'))
  )

  mainPanel <- mainPanel(
    p("Imagine you asked a class of third graders how many brothers and sisters they had; how would you estimate the average number of children in a family?"),
    h2("Distributions of Family Size"),
    plotOutput("plot_pop"),
    # h2("Sample Distribution of Num siblings"),
    # plotOutput("plot_sample", width="50%", inline=TRUE),
    h2("Diagnosis"),
    dataTableOutput("diagnosis")




  )


  fluidPage(theme = "cerulean",
     sidebarLayout(sidebarPanel, mainPanel)
  )
})

children.server <- function(input, output, clientData, session) {
  library(ggplot2)
  library(scales)


  families <- reactive({

    N_families <- input$N_families
    probs <- c(input$children_0, input$children_1, input$children_2, input$children_3, input$children_4)

    families <- declare_population(family=level(N=N_families, N_i_children=sample(0:4, N, TRUE, probs)))
    families
  })

  design <- reactive({

    k_children <- input$k_children

    family_size_estimand <- declare_estimand(mu_Family=mean(N_i_children), label="Family Size")
    family_size_truncated <- declare_estimand(mu_Family_trunc=mean(N_i_children), subset=N_i_children > 0, label="Family Size Trunc")

    children <- declare_population(children=level(N=N_i_children, siblings=N_i_children))
    children_siblings_estimand <- declare_estimand(mu_siblings=mean(siblings), label="Mean Siblings")

    srs <- declare_sampling(N=k_children)

    target_Estimand = switch(input$estimand, fam=family_size_estimand, fam.trunc=family_size_truncated, child.sib=children_siblings_estimand)

    unweighted_est <- declare_estimator(formula=siblings~1, model=lm, coefficient_name = `(Intercept)`, estimand=target_Estimand, label="unweighted")
    weighted_est <- declare_estimator(formula=siblings~1, model=lm, weights=1/siblings, coefficient_name = `(Intercept)`, estimand=target_Estimand, label="weighted")

    families_design <- declare_design(
      families(),
      family_size_estimand,
      family_size_truncated,
      children,
      children_siblings_estimand,
      srs,
      unweighted_est,
      weighted_est
    )

    families_design

  })

  output$plot_pop <- renderPlot({
    df <- families()()
    g1 <- ggplot(df) + aes(x=N_i_children) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      ggtitle("Number of Children per family") +
      xlab("")+ ylab("")+
      scale_y_continuous(labels=scales::percent)

    df <- design()$data_function()
    g2 <- ggplot(df) + aes(x=siblings) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      ggtitle("Number of siblings per child") +
      xlab("")+ ylab("")+
      scale_y_continuous(labels=scales::percent)

    gridExtra::marrangeGrob(list(g1, g2), nrow=1, ncol=2, top=NULL)
  })


  output$diagnosis <- renderDataTable({
    d <- design()
    d <- diagnose_design(d, sims = 50, bootstrap_sims = 50)
    d <- get_diagnosands(d)
    round_df(d, 4)
  }, options = list(paging=FALSE, searching=FALSE))


}

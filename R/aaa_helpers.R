### File is name aaa to note have to manually specify file collation - we will just use A-Z ordering. -- NJF 11/21

### Small bits of global config
nav_bar_color = "blue lighten-2"


### actual helpers

pretty_diagnoses <- function(df, digits=4){
  require(reshape2)

  ret <- df[intersect(c('design_ID', 'estimand_label', 'estimator_label'), names(df))]
  names(ret) <- str_replace(str_to_title(names(ret)), "_.*", "")

  ids <- names(ret)

  data_columns <- names(df)
  data_columns <- data_columns[grep('^se[(]|_label$|_ID$', data_columns, invert = TRUE)]

  myfmt <- sprintf('%%.%if', digits)

  for(col in data_columns) {
    title <- str_to_title(str_replace_all(col, '_', ' '))
    x <- sprintf(myfmt, df[[col]])
    secol <- sprintf('se(%s)', col)
    if( secol %in% names(df)) {
      se <- sprintf(myfmt, df[[secol]])
      x <- sprintf(paste('%s(%s)'), x, se)
    }
    ret[[title]] <- x
  }

  ret <- melt(ret, ids, variable.name="Diagnosand")

  if('Design' %in% ids){
    ret <- dcast(ret, ...~Design, value.var = "value")

  }
  ret
}

pretty_summary <- function(x) {
  # browser()

  step_summary <- with(x,
                       mapply(pretty_summary_step, seq_along(causal_order_expr), variables_added, variables_modified, quantities_added, causal_order_expr, function_types, N, formulae,
                              SIMPLIFY = FALSE)
  )


  ret <- shiny::tags$div(
    # shiny::tags$header("Design Summary"),



    if (!is.null(x$title)) {
      shiny::tags$p("Study title: ", x$title)
    },

    if (!is.null(x$authors)) {
      shiny::tags$p("Authors: ", x$authors)
    },

    if (!is.null(x$description)) {
      shiny::tags$p(x$description)
    },

    step_summary,


    if (!is.null(x$citation)) {
      shiny::tags$p("Citation:", shiny::tags$br, x$citation)
    }
  )



  ret
}

pretty_summary_step <- function(i, variables_added, variables_modified, quantities_added, causal_order_expr, function_types, N, formulae) {

  step_name <- deparse(causal_order_expr)
  step_class <-
    ifelse(
      function_types != "unknown",
      gsub("_", " ", function_types),
      "custom data modification"
    )


  ret <- material_card(
    paste("Step", i, "(", step_class, "):", step_name),

    if (!is.null(N)) {
      shiny::tags$p(N)
    },

    if (!is.null(formulae)) {
      shiny::tags$p("Formula:", deparse(formula))
    },

    if (!is.null(quantities_added)) {
      if (class(quantities_added) == "data.frame") {
        shiny::tags$p(
          "A single draw of the ", function_types, ":\n",

          HTML(renderTable(quantities_added)())
        )
      } else {
        shiny::tags$p(as.character(quantities_added))
      }
    },

    if (!is.null(variables_added)) {
      lapply(seq_along(variables_added), function(j){
        shiny::tags$p(
          "Added variable:",
          names(variables_added)[j],
          shiny::tags$br(),
          HTML(renderTable(variables_added[[j]])())
        )
      })
    },

    if (!is.null(variables_modified)) {
      lapply(seq_along(variables_modified), function(j){
        shiny::tags$p(
          "Altered variable:",
          names(variables_modified)[j],
          shiny::tags$br(),
          "Before:",
          HTML(renderTable(variables_modified[[j]][["before"]])()),
          shiny::tags$br(),
          "After",
          HTML(renderTable(variables_modified[[j]][["after"]])())
        )
      })
    }

  )


  ret


}


dd_theme <-
  function() {
    theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        strip.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino"))
  }




remove_close_button_from_modal <- function(modal){
  #TODO this is nasty
  closebutton <- modal[[2]][[2]]$children[[2]] # save close button for latter
  modal[[2]][[2]]$children[[2]] <- NULL # skip close button in modal dialog
  modal[[2]][[2]]$children[[1]]$attribs$style="box-shadow:none; border:none;"

  modal
}

my_tipify <- function(txtbox, tip){
  txtbox[[2]]$class <- paste(txtbox[[2]]$class, "tooltipped")
  txtbox[[2]][["data-tooltip"]] <- tip
  txtbox
}


remove_spaces <- function(text){
  str_replace_all(str_trim(text), "\\s+", " ")
}

convert_character_to_vector <- function(text){
  text <- str_trim(unlist(strsplit(text, split = ",")))
  if(identical(as.character(as.numeric(text)), text)){
    return(as.numeric(text))
  } else {
    return(text)
  }
}

round_df <- function(df, digits){
  i <- vapply(df, is.numeric, TRUE)
  df[i] <- lapply(df[i], round, digits)
  df
}


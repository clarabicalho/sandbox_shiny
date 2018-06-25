### File is name aaa to note have to manually specify file collation - we will just use A-Z ordering. -- NJF 11/21

#' Run diagnosis and store RDS
#' @param ... Design, number of simulations and bootstraps for DeclareDesign
#' @return Diagnosis
#' @export
#'
get_or_run_diagnosis <- function(design,sims,bootstrap) {
  design_name <- substitute(design)
  file_name <- paste0(design_name,"_diagnosis.RDS")
  if(file.exists(file_name)){
    diagnosis <- readRDS(file = file_name)
  } else {
    diagnosis <- DeclareDesign::diagnose_design(design,sims = sims,bootstrap = bootstrap)
    saveRDS(diagnosis, file_name)
  }
  diagnosis
}

#' @export
#'
get_shiny_arguments <- function(designer){
  attributes(designer)$shiny_arguments
}

#' @export
#'
get_constants <- function(designer){
  shiny_arguments <- get_shiny_arguments(designer)
  formals(designer)[!names(formals(designer)) %in% names(shiny_arguments)]
}

#' @export
#'
designer_default_args_text <- function(designer) {
  shinys <- get_shiny_arguments(designer)
  shinys <- lapply(shinys,function(x)x[1])
  args <- c(shinys,get_constants(designer))
  args <- args[names(formals(designer))]
  args <- lapply(args,deparse)
  mapply(paste, names(args), "<-", args, USE.NAMES = FALSE)
}

#' @export
#'
expand_designer_shiny_args_text <- function(designer) {
  shinys <- get_shiny_arguments(designer)
  all_shinys <- expand.grid(shinys)
  lapply(1:nrow(all_shinys),function(i){
    shiny_args <- (all_shinys[i,])
    names(shiny_args) <- names(all_shinys)
    args <- c(shiny_args,get_constants(designer))
    args <- args[names(formals(designer))]
    args <- lapply(args,deparse)
    mapply(paste, names(args), "<-", args, USE.NAMES = FALSE)
  })
}

#' @export
#'
get_shiny_diagnosis <- function(designer,sims,bootstrap) {
  shiny_args <- get_shiny_arguments(designer)
  all_designs <- expand_design(template = designer,expand = TRUE,shiny_args)
  diagnosis <- diagnose_design(all_designs,sims = sims,bootstrap = bootstrap)
  argument_list <- expand_designer_shiny_args_text(designer = designer)
  return(list(diagnosis = diagnosis, argument_list = argument_list))
}

#' @export
#'
get_or_run_shiny_diagnosis <- function(designer,designer_name = NULL,sims,bootstrap,update_existing=FALSE) {
  if(is.null(designer_name)) designer_name <- substitute(designer)
  design_name <- gsub(pattern = "_designer",replacement = "",x = designer_name)
  file_name <- paste0("data/",design_name,"_shiny_diagnosis.RDS")
  parameters <- expand.grid(get_shiny_arguments(designer), stringsAsFactors = FALSE)
  if(update_existing==FALSE & file.exists(file_name)){
    diagnosis_list <- readRDS(file = file_name)
    diagnosis <- diagnosis_list$diagnosis
  } else {
    diagnosis_list <- get_shiny_diagnosis(designer,sims = sims,bootstrap=bootstrap)
    diagnosis <- diagnosis_list$diagnosis
    rows_perID <- as.data.frame(table(diagnosis$diagnosands$design_ID))
    parameters <- parameters[rep(seq_len(nrow(parameters)), times = rows_perID$Freq),, drop = FALSE]
    diagnosis$diagnosands <- cbind(diagnosis$diagnosands,parameters)
    diagnosis_list$diagnosis <- diagnosis
    saveRDS(diagnosis_list, file_name)
  }
  diagnosis
}

### Small bits of global config
nav_bar_color = " light-blue darken-3"


### actual helpers

pretty_diagnoses <- function(df, digits=3){
  ret <- df[intersect(c('design_ID', 'estimand_label', 'estimator_label'), names(df))]
  names(ret) <- str_replace(str_to_title(names(ret)), "_.*", "")

  ids <- names(ret)

  data_columns <- names(df)
  data_columns <- data_columns[grep('^se[(]|_label$|_ID$|coefficient$', data_columns, invert = TRUE)]

  myfmt <- sprintf('%%.%if', digits)

  for(col in data_columns) {
    title <- str_to_title(str_replace_all(col, '_', ' '))
    x <- sprintf(myfmt, df[[col]])
    secol <- sprintf('se(%s)', col)
    if( secol %in% names(df)) {
      se <- sprintf(myfmt, df[[secol]])
      x <- sprintf(paste('%s (%s)'), x, se)
    }
    ret[[title]] <- x
  }

  ret <- reshape2::melt(ret, ids, variable.name="Diagnosand")

  # if('Design' %in% ids){
  #   ret <- dcast(ret, ...~Design, value.var = "value")
  #
  # }
  ret
}

diagnosis_plot <- function(diagnostic_param){
  # param    <- powerdf[[diagnostic_param]]
  # param_se <- powerdf[[paste0("se(", diagnostic_param, ")")]]
  powerdf <- gather(powerdf, param_name, param_value, bias, coverage, power)
  powerdf <- gather(powerdf, param_name, param_se, `se(bias)`, `se(coverage)`, `se(power)`)

  ggplot(powerdf) +
    aes(x=, y=param_value, ymin=param-2*param_se, ymax=power+2*param_se,
        group=estimator_label, color=estimator_label, fill=estimator_label) +
    geom_line() +
    geom_point() +
    geom_ribbon(alpha=.3) +
    scale_y_continuous(name="Power of Design", limits=0:1, breaks=0:4/4, minor_breaks = 0:10/10) +
    dd_theme() +  labs(fill="",color="")

}

pretty_summary <- function(x) {
  # browser()

  step_summary <- with(x,
                       mapply(pretty_summary_step, seq_along(function_types), variables_added, variables_modified, quantities_added, function_types, N, formulae,
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
      material_card("Citation:", HTML(format(x$citation, style="html")))
    }
  )



  ret
}

pretty_summary_step <- function(i, variables_added, variables_modified, quantities_added, function_types, N, formulae) {

  # step_name <- deparse(causal_order_expr)
  step_class <-
    ifelse(
      function_types != "unknown",
      gsub("_", " ", function_types),
      "custom data modification"
    )


  ret <- material_card(
    paste0("Step ", i, " (", step_class, "):"),

    if (!is.null(N)) {
      shiny::tags$p(N)
    },

    if (!is.null(formulae)) {
      shiny::tags$p("Formula:", deparse(formulae))
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

code_to_rmd <- function(design_code){
  rmd <- c("---",
           "output:",
           "  html_document:",
           "    highlight: 'pygments'",
           "---",
           "```{r, eval=FALSE}",
           design_code,
           "```")

  filecon <- "design-code.Rmd"
  writeLines(rmd, filecon)
  sapply(filecon, knitr::knit, quiet = TRUE)
  return(filecon)
}

get_author <- function(designer){
  rdb_path <- file.path(system.file("help", package= "DesignLibrary"),"DesignLibrary")
  help_text <- tools:::fetchRdDB(rdb_path, designer)
  classes <- sapply( help_text, function(x) attr(x, "Rd_tag"))
  author <- help_text[[grep("\\author", classes, fixed = TRUE)]]
  author <- paste(sapply(author, function(i) i[1]))
  author <- gsub("\\n", "", author)
  paste(author[!author %in% ""], collapse = ", ")
}

dd_theme <-
  function() {
    theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text=element_text(),#size=12),
        axis.title=element_text(face="bold"),#size=14,
        strip.text.x = element_text(face="bold"),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        strip.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino", size=16)
      )
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

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

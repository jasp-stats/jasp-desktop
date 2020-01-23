#jasp
# variables = options$variables
# dataset =data = exercise_data
add_polynomials = function(variables, data, degree=2){
  cat = sapply(data[,variables, drop=FALSE], check.non.number)
  f = function(x) paste0("I(", x, "^", degree, ")")
  sapply(variables[!cat], f)
}


### function to organize residual plots
arrange_jasp_plots = function(plot_list, theme){
  
  plot_list = lapply(plot_list, theme_it, theme)
  if (is.null(plot_list[["res.dep"]])){
    plot_list[["res.dep"]] = NULL
    plot = cowplot::plot_grid(plotlist = plot_list)
  } else {
    top.row =suppressMessages(cowplot::plot_grid(plot_list$histo, plot_list$res.dep,ncol=2))
    bottom.row =suppressMessages(cowplot::plot_grid(NULL, plot_list$sl, NULL, ncol=3, rel_widths=c(.25, .5, .25)))
    plot = suppressMessages(cowplot::plot_grid(top.row, bottom.row, nrow=2))
  }
  return(plot)
}

theme_it = function(plot, theme){
  if (theme == "JASP"){
    plot = JASPgraphs::themeJasp(plot)
  } else {
    themes = list("Black and white"="theme_bw()+ theme(text=element_text(size=18))",
                 "Minimal" = "theme_minimal()+ theme(text=element_text(size=18))",
                 "Classic" = "theme_classic()+ theme(text=element_text(size=18))",
                 "Dark" = "theme_dark() + theme(text=element_text(size=18))")
    plot = plot + eval(parse(text=themes[[theme]]))
  }
  return(plot)
}

modify_dv = function(dataset, outcome, family){
  if (family!="Logistic") {
    dataset[,outcome] = as.numeric(dataset[,outcome])
  } else {
    dataset = factor.to.logistic(data = dataset, outcome = outcome)
  }
  return(dataset)
}
  
## find main effects
find.me = function(x) {
  if (length(x)>1){
    return(x)
  } 
}

#term.labels = c("a", "b", "c", "a:b", "b:c")
#term.labels = letters[1:5]
main_effects_2_remove = function(term.labels){
  splits = strsplit(term.labels, ":")
  interactions = unlist(lapply(splits, find.me))
  return(unique(interactions))
}

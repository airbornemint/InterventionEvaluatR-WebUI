import::from(plyr, llply)
import::from(InterventionEvaluatR, evaluatr.init, evaluatr.univariate, evaluatr.univariate.plot, evaluatr.plots, evaluatr.prune)
import::from(ggplot2, ggtitle)

# Run the relevant pieces of evaluatr analysis
app.analyze = function(params, analysisTypes) {
  analysis = do.call(
    evaluatr.init,
    params
  )
  
  if ('univariate' %in% analysisTypes) {
    univariateResults = evaluatr.univariate(analysis)
  }

  if ('impact' %in% analysisTypes) {
    impactResults = evaluatr.impact(analysis)
  }

  # Only keep what we need so we aren't shipping large amounts of never-to-be-used data between worker and UI
  evaluatr.prune(analysis)
}

app.plot = function(analysis, analysisTypes) {
  saveRDS(list(analysis=analysis, analysisTypes=analysisTypes), "/tmp/app.plot.rds")
  groupNames = sprintf("%s %s", analysis$group_name, analysis$groups)
  
  if ('univariate' %in% analysisTypes) {
    univariatePlots = setNames(llply(seq_along(analysis$groups), function(idx) 
      evaluatr.univariate.plot(analysis$results$univariate[[idx]])
    ), groupNames)
  } else {
    univariatePlots = NULL
  }
  
  if ('impact' %in% analysisTypes) {
    impactPlots = evaluatr.plots(analysis)
  } else {
    impactPlots = NULL
  }
  
  list(
    best=llply(seq_along(analysis$groups), function(group) {
      analysis$results$impact$best$variant[[group]]
    }),
    plots=setNames(llply(seq_along(analysis$groups), function(group) {
      if (!is.null(univariatePlots)) {
        univariate = list(
          univariate=ggplotly(
            univariatePlots[[group]]
          )
        )
      } else {
        univariate = list()
      }
      
      if (!is.null(impactPlots)) {
        impact = list(
          tsMonthly=ggplotly(
            impactPlots$groups[[group]]$pred_best + ggtitle(NULL)
          ),
          tsYearly=ggplotly(
            impactPlots$groups[[group]]$pred_best_agg + ggtitle(NULL)
          ),
          prevented=ggplotly(
            impactPlots$groups[[group]]$cumsum_prevented + ggtitle(NULL)
          )
        )
      } else {
        impact = list()
      }
      
      c(univariate, impact)
    }), groupNames)
  )  
}

# This is the version number for the "download results" rds file. Change if making incompatible changes.
SAVE_VERSION_CURRENT = 5
# This is oldest version number for the "download results" rds file that we still accept. Change when dropping support for loading older files.
SAVE_VERSION_COMPATIBLE = 5

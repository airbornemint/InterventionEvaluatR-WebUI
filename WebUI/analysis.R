import::from(plyr, llply)
import::from(InterventionEvaluatR, evaluatr.init, evaluatr.univariate, evaluatr.univariate.plot, evaluatr.plots, evaluatr.prune)
import::from(ggplot2, ggtitle)
import::from(tibble, rownames_to_column)

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

app.vis = function(analysis, analysisTypes) {
  # saveRDS(list(analysis=analysis, analysisTypes=analysisTypes), "/tmp/app.plot.rds")
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
  
  vis = list(
    plots=setNames(llply(seq_along(analysis$groups), function(group) {
      if (!is.null(univariatePlots)) {
        univariate = list(
          univariate=univariatePlots[[group]]
        )
      } else {
        univariate = list()
      }
      
      if (!is.null(impactPlots)) {
        impact = list(
          tsMonthly=impactPlots$groups[[group]]$pred_best + ggtitle(NULL),
          tsYearly=impactPlots$groups[[group]]$pred_best_agg + ggtitle(NULL),
          prevented=impactPlots$groups[[group]]$cumsum_prevented + ggtitle(NULL)
        )
      } else {
        impact = list()
      }
      
      c(univariate, impact)
    }), groupNames)
  )
  
  if ("impact" %in% analysisTypes) {
    normRR = function(rr, variant) {
      rr %>% data.frame() %>% setNames(c("lcl", "median", "ucl")) %>% rownames_to_column("group") %>% mutate(variant=variant)
    }
    
    rr = rbind(
      analysis$results$impact$best$rr_mean %>% normRR(variant="best"), 
      analysis$results$impact$full$rr_mean %>% normRR(variant="full"),
      analysis$results$impact$time$rr_mean %>% normRR(variant="time"), 
      analysis$results$impact$time_no_offset$rr_mean %>% normRR(variant="time_no_offset"), 
      analysis$results$impact$its$rr_end %>% normRR(variant="its"), 
      analysis$results$impact$pca$rr_mean %>% normRR(variant="pca")
    )

    vis = c(vis, list(
      rateRatios=llply(seq_along(analysis$groups), function(idx) {
        rr %>% filter(group==analysis$groups[[idx]]) %>% mutate(
          rr=sprintf("%.2f (%.2f - %.2f)", median, lcl, ucl),
          variant.name=variant
        ) %>%
        select(variant.name, rr)
      }),
      best=llply(seq_along(analysis$groups), function(group) {
        analysis$results$impact$best$variant[[group]]
      })
    ))
  }
}

# This is the version number for the "download results" rds file. Change if making incompatible changes.
SAVE_VERSION_CURRENT = 5
# This is oldest version number for the "download results" rds file that we still accept. Change when dropping support for loading older files.
SAVE_VERSION_COMPATIBLE = 5

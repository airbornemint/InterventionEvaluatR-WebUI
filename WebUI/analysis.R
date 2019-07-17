import::from(plyr, llply)

# Run the relevant pieces of evaluatr analysis
app.analyze = function(params) {
  analysis = do.call(
    evaluatr.init,
    params
  )
  evaluatr.univariate(analysis)
  # Only keep what we need so we aren't shipping large amounts of unused data between worker and UI
  c(
    analysis$results,
    list(
      groups=analysis$groups
    )
  )
}

# Create plots and organize them by analysis group
app.plot = function(params, groups, results) {
  if (is.null(groups)) {
    groups=results$groups
  }
  groupNames = sprintf("%s %s", params$group_name, groups)
  setNames(llply(seq_along(groups), function(group) {
    list(
      univariate=ggplotly(
        evaluatr.univariate.plot(results$univariate[[group]])
      )
    )
  }), groupNames)
}

# This is the version number for the "download results" rds file. Change if making incompatible changes.
CURRENT_SAVE_VERSION = 1

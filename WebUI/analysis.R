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
app.plot = function(params, results) {
  groups = sprintf("%s %s", params$group_name, results$groups)
  setNames(llply(seq_along(results$univariate), function(idx) {
    list(
      univariate=ggplotly(
        evaluatr.univariate.plot(results$univariate[[idx]])
      )
    )
  }), groups)
}
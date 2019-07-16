import::from(plyr, llply)

# Run the relevant pieces of evaluatr analysis
app.analyze = function(params) {
  analysis = do.call(
    evaluatr.init,
    params
  )
  evaluatr.univariate(analysis)
  analysis$results
}

# Create plots and organize them by analysis group
app.plot = function(results) {
  groups = seq_along(results$univariate)
  setNames(llply(seq_along(results$univariate), function(idx) {
    list(
      univariate=ggplotly(
        evaluatr.univariate.plot(results$univariate[[idx]])
      )
    )
  }), groups)
}
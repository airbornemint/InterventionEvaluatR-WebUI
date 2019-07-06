stockDatasets = list(
  `Preumococcal pneumonia, Brazil`="pnas_brazil"  
)

invert.list = function(l) {
  listValues = as.character(l)
  listNames = names(l)
  as.list(setNames(listNames, listValues))
}

null2empty = function(s) {
  if (is.null(s)) {
    ""
  } else {
    s
  }
  
}
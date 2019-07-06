import::from(plyr, compact)

dateFormats = list(
  `YYYY-MM-DD`="%Y-%m-%d",
  `YYYY-DD-MM`="%Y-%d-%m",
  `MM-DD-YYYY`="%m-%d-%Y",
  `DD-MM-YYYY`="%d-%m-%Y"
)

stockDatasets = list(
  `Preumococcal pneumonia, Brazil`="pnas_brazil"  
)

invert.list = function(l) {
  listValues = as.character(l)
  listNames = names(l)
  as.list(setNames(listNames, listValues))
}

# Input if non-null, empty string otherwise
null2empty = function(s) {
  if (is.null(s)) {
    ""
  } else {
    s
  }

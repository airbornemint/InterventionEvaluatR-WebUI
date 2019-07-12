import::from(plyr, compact)

dateFormats = list(
  `YYYY-MM-DD`="%Y-%m-%d",
  `YYYY-DD-MM`="%Y-%d-%m",
  `MM-DD-YYYY`="%m-%d-%Y",
  `DD-MM-YYYY`="%d-%m-%Y"
)

postDurations = list(
  `6 months`=6,
  `12 months`=12,
  `18 months`=18,
  `24 months`=24
)

stockDatasets = list(
  `Pneumonia, Brazil`="pnas_brazil"  
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
}

# TRUE if format is valid date format for v
# Format is valid if the vector can be converted to dates in that format and if differences between nearby dates are >5
validFormat = function(v, format) {
  dates = as.Date(as.character(v), format)
  if(all(!is.na(dates))) {
    # This gets all the deltas between distinct adjacent dates
    diffs = dates %>% sort() %>% unique() %>% diff() %>% unique()
    # Of which none should be <20 or >200, because we're expecting monthly or quarterly data
    all(diffs > 20 & diffs < 200)
  } else {
    FALSE
  }
}

# Auto-detect viable time columns and their formats. Empty list if none are found, NULL if data is NULL
dateColumns = function(data) {
  names(data) %>% 
    sapply(function(name) {
      # List of viable formats for named column; NULL if none
      dateFormats %>% 
        lapply(function(format) {
          if(validFormat(data[[name]], format)) {
            format
          }
        }) %>% 
        compact() %>%
        (function(x) {
          if (length(x) > 0) {
            x
          }
        })
    }, simplify = FALSE, USE.NAMES = TRUE) %>%
    compact() 
}

# Turn of spinner if value is non-null
unspin = function(session, spinner, value) {
  md_update_spinner(session, spinner, hidden=!is.null(value))
  value
}

# True if expr is valid according to the same criteria as shiny::need
checkNeed = function(expr) {
  is.null(need(expr, ""))
}
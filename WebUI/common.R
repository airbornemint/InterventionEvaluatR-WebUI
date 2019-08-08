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
  `Pneumonia in Brazil, 2003-2013`="pnas_brazil"  
)

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

# True if expr is valid according to the same criteria as shiny::need
checkNeed = function(expr) {
  tryCatch(
    is.null(need(expr, FALSE)), 
    error=function(e) if (!inherits(e, "shiny.silent.error")) {
      stop(e)
    } else {
      FALSE
    }
  )
}

nextButton = function(buttonId, spinnerId, title="Next", disabled=TRUE) {
  div(
    class="button-next",
    md_button(
      buttonId,
      span(class="title", title), 
      md_button_spinner(spinnerId), 
      style="primary", disabled=disabled
    )
  )
}


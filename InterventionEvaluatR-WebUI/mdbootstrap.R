import::from(plyr, compact)
import::from(shinyBS, bsButton)

md_page = function(...) {
  tags$html(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/mdb.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/mdbootstrap.css"),
      tags$script(src = "js/mdbootstrap.js")
    )),
    tags$body(
      div(...)
    )
  )
}

md_navbar = function(..., title=NULL) {
  tags$nav(
    tags$a(
      title,
      class="navbar-brand",
      href="#"
    ),
    ...,
    class="navbar navbar-dark primary-color"
  )
}

md_row = function(...) {
  div(..., class="row")
}

md_column = function(...) {
  div(..., class="col")
}

md_stepper_vertical = function(..., id, selected) {
  steps = list(...)
  tags$ul(
    lapply(seq_along(steps), function(idx) {
      step = steps[[idx]]
      tags$li(
        a(
          span(as.character(idx), class="circle"),
          span(step$title, class="label"),
          span(step$summary, class="label summary"),
          href="#!"
        ),
        div(
          class="step-body",
          div(
            step$content,
            class="step-content"
          )
        ),
        id=step$value,
        class=ifelse(step$enabled, "completed", "")
      )
    }),
    class="stepper stepper-vertical",
    id=id,
    "data-stepper-selected"=selected
  )
}

md_stepper_step = function(title, ..., value, summary=NULL, enabled=FALSE) {
  list(title=title, value=value, content=list(...), summary=summary, enabled=enabled)
}

md_update_stepper = function(session, stepper, value=NULL) {
  session$sendCustomMessage("md_update_stepper", list(stepper=stepper, value=value) %>% compact())
}

md_update_stepper_step = function(session, stepper, step, enabled=NULL) {
  session$sendCustomMessage("md_update_stepper_step", list(
    stepper=stepper, step=step, enabled=enabled
  ) %>% compact())
}

# All unnamed arguments except for the first one are treated as content
md_button = function(id, ...) {
  args = list(...)
  named = args[names(args) != ""]
  unnamed = args[names(args) == ""]
  do.call(bsButton, c(list(inputId=id, label=tagList(unnamed)), named))
}

md_spinner = function(id) {
  div(
    class="spinner-border", role="status", id=id,
    span(class="sr-only", "Loading…")
  )
}

md_button_spinner = function(id, visible=FALSE) {
  span(
    class="spinner-border spinner-border-sm", role="status", id=id, "aria-hidden"="true"
  ) %>% tagAppendAttributes(class=ifelse(visible, "", "invisible"))
}

# hidden vs visible = CSS display vs CSS visible
md_update_spinner = function(session, spinner, hidden=NULL, visible=NULL) {
  session$sendCustomMessage("md_update_spinner", list(spinner=spinner, hidden=hidden, visible=visible) %>% compact())
}

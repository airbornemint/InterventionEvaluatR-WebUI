import::from(plyr, compact)

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
          step$content,
          class="step-content"
        ),
        id=step$value
      )
    }),
    class="stepper stepper-vertical",
    id=id,
    "data-stepper-selected"=selected
  )
}

md_stepper_step = function(title, ..., value, summary=NULL) {
  list(title=title, value=value, content=list(...), summary=summary)
}

md_update_stepper = function(session, stepper, value=NULL) {
  session$sendCustomMessage("md_update_stepper", list(stepper=stepper, value=value) %>% compact())
}

md_update_stepper_step = function(session, stepper, step, completed=NULL) {
  session$sendCustomMessage("md_update_stepper_step", list(
    stepper=stepper, step=step, completed=completed
  ) %>% compact())
}
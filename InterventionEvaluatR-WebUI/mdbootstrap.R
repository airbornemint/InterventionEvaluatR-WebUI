md_page = function(...) {
  tags$html(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/mdb.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css")
    ),
    tags$body(
      div(..., class="container")
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

md_stepper_vertical = function(..., id=NULL) {
  steps = list(...)
  tags$ul(
    lapply(seq_along(steps), function(idx) {
      step = steps[[idx]]
      tags$li(
        a(
          span(as.character(idx), class="circle"),
          span(step$title, class="label"),
          href="#!"
        ),
        div(
          step$content,
          class="step-content"
        ),
        id=step$id,
        class="completed"
      )
    }),
    class="stepper stepper-vertical",
    id=id
  )
}

md_stepper_step = function(title, ..., id=NULL) {
  list(title=title, id=id, content=list(...))
}
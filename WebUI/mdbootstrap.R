import::from(plyr, compact)
import::from(shinyBS, bsButton)
import::from(magrittr, "%<>%")

md_page = function(...) {
  tags$html(
    singleton(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/bootstrap.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mdb/css/mdb.css"),
      tags$script(src = "mdb/js/mdb.js"),
      tags$script(src = "mdb/js/bootstrap.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/mdbootstrap.css"),
      tags$script(src = "js/mdbootstrap.js")
    )),
    tags$body(
      div(...)
    )
  )
}

md_navbar = function(..., title=NULL, class=NULL) {
  tags$nav(
    tags$a(
      title,
      class="navbar-brand",
      href="#"
    ),
    ...,
    class="navbar navbar-dark primary-color"
  ) %>% tagAppendAttributes(class=class)
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

md_carousel = function(id, slides) {
  div(
    id=id,
    class="carousel slide",
    tags$ol(
      class="carousel-indicators",
      tagList(llply(seq_along(slides), function(idx) {
        item = tags$li(
          class="primary-color",
          "data-target"=sprintf("#%s", id),
          "data-slide-to"=idx-1
        )
        if (idx == 1) {
          item %<>% tagAppendAttributes(class="active")
        }
        item
      }))
    ),
    div(
      class="carousel-inner",
      role="listbox",
      tagList(llply(seq_along(slides), function(idx) {
        item = div(
          class="carousel-item",
          div(
            class="d-block w-100",
            slides[[idx]]
          )
        )
        if (idx == 1) {
          item %<>% tagAppendAttributes(class="active")
        }
        item
      }))
    ),
    tags$a(
      class="carousel-control-prev",
      href=sprintf("#%s", id),
      role="button",
      "data-slide"="prev",
      span(
        class="carousel-control-prev-icon primary-color",
        "aria-hidden"="true"
      ),
      span(
        class="sr-only",
        "Previous"
      )
    ),
    tags$a(
      class="carousel-control-next",
      href=sprintf("#%s", id),
      role="button",
      "data-slide"="next",
      span(
        class="carousel-control-next-icon primary-color",
        "aria-hidden"="true"
      ),
      span(
        class="sr-only",
        "Next"
      )
    )
  )
}
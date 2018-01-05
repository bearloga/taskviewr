library(shiny)
library(magrittr)

packages <- dplyr::distinct(
  read.csv("www/packages.csv", stringsAsFactors = FALSE),
  view, package,
  .keep_all = TRUE
)

packages$license <- sub("( [+|] )?file LICEN[SC]E", "", packages$license, ignore.case = FALSE)

decorate_url <- function(url) {
  return(paste0("<a href=\"", url, "\">", sub("^(https?://)?(www.)?(.*)", "\\3", url), "</a>"))
}

make_url <- function(url) {
  regex_url <- "(https?://)?[-[:alnum:]]+\\.[^\\s,]+"
  url <- stringr::str_replace_all(url, ",\\s", "\n")
  return(stringr::str_replace_all(url, regex_url, decorate_url))
}

shinyServer(function(input, output) {
  
  dt <- reactiveVal()
  observe({
    if (input$task) {
      drop_columns <- NULL
      if (length(input$view) == 1) drop_columns <- 1
      if (!"Description" %in% input$fields) drop_columns <- c(drop_columns, 6)
      if (!"URL" %in% input$fields) drop_columns <- c(drop_columns, 7)
      if (!"Authors" %in% input$fields) drop_columns <- c(drop_columns, 4)
      if (length(input$view) > 1) {
        temporary <- packages[packages$view %in% input$view, ]
        if (any(duplicated(temporary$package))) {
          # Some packages appear under multiple views, let's consolidate:
          temporary <- temporary %>%
            dplyr::group_by(package, title, license, description, url, authors) %>%
            dplyr::summarize(
              view = paste0(view, collapse = ", "),
              views = n()
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(view, views, package, title, authors, license, description, url)
          if (!is.null(drop_columns)) {
            drop_columns <- drop_columns + 1
          }
        }
      } else {
        temporary <- packages[packages$view == input$view, ]
      }
      temporary$url <- make_url(temporary$url)
      if (is.null(drop_columns)) {
        dt(temporary)
      } else {
        dt(temporary[, -drop_columns])
      }
    } else {
      drop_columns <- NULL
      if (!"Description" %in% input$fields) drop_columns <- c(drop_columns, 7)
      if (!"URL" %in% input$fields) drop_columns <- c(drop_columns, 8)
      if (!"Authors" %in% input$fields) drop_columns <- c(drop_columns, 5)
      temporary <- packages %>%
        dplyr::group_by(package, title, license, description, url, authors) %>%
        dplyr::summarize(
          view = paste0(view, collapse = ", "),
          views = n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(view, views, package, title, authors, license, description, url) %>%
        dplyr::arrange(package)
      temporary$url <- make_url(temporary$url)
      if (is.null(drop_columns)) {
        dt(temporary)
      } else {
        dt(temporary[, -drop_columns])
      }
    }
  })
  
  output$packages <- DT::renderDataTable({
    DT::datatable(
      dt(),
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE, escape = FALSE,
      options = list(
        search = list(regex = TRUE, caseInsensitive = TRUE),
        language = list(search = "Filter:"),
        pageLength = 5, lengthMenu = c(5, 10, 25, 50, 100),
        order = list(list(2, "desc"))
      )
    )
  })
  
})

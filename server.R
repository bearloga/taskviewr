library(shiny)
library(magrittr)

packages <- dplyr::distinct(
  read.csv("www/packages.csv", stringsAsFactors = FALSE),
  view, package,
  .keep_all = TRUE
)
packages$license <- sub("( [+|] )?file LICEN[SC]E", "", packages$license, ignore.case = FALSE)

decorate_url <- function(url)
    paste0("<a href=\"", url, "\">", url, "</a>")

make_url <- function(url) {
  regex_url <- "(https?://)?[-[:alnum:]]+\\.[^\\s,]+"
  url <- stringr::str_replace_all(url, ",\\s", "\n")
  stringr::str_replace_all(url, regex_url, decorate_url)
}

shinyServer(function(input, output) {
  
  dt <- reactiveVal()
  observe({
    if (input$task) {
      drop_columns <- NULL
      if (length(input$view) == 1) drop_columns <- 1
      if (!input$description) drop_columns <- c(drop_columns, 5)
      if (!input$url) drop_columns <- c(drop_columns, 6)
      if (length(input$view) > 1) {
        temporary <- packages[packages$view %in% input$view, ]
        if (any(duplicated(temporary$package))) {
          # Some packages appear under multiple views, let's consolidate:
          temporary <- temporary %>%
            dplyr::group_by(package, title, license, description, url) %>%
            dplyr::summarize(view = paste0(view, collapse = ", "),
                             views = n()) %>%
            dplyr::ungroup() %>%
            dplyr::select(view, views, package, title, license, description, url)
          drop_columns <- drop_columns + 1
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
      if (!input$description) drop_columns <- c(drop_columns, 6)
      if (!input$url) drop_columns <- c(drop_columns, 7)
      temporary <- packages %>%
        dplyr::group_by(package, title, license, description, url) %>%
        dplyr::summarize(view = paste0(view, collapse = ", "),
                         views = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(url = make_url(url)) %>% 
        dplyr::select(view, views, package, title, license, description, url) %>%
        dplyr::arrange(package)
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
      options = list(
        search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength = 5, lengthMenu = c(5, 10, 25, 50, 100)
      ),
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE,
      escape = FALSE
    )
  })

})

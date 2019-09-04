library(shiny)
library(magrittr)

packages <- "www/packages.csv" %>%
  readr::read_csv(col_types = "ccccccc") %>%
  dplyr::distinct(view, package, .keep_all = TRUE) %>%
  dplyr::mutate(license = sub("( [+|] )?file LICEN[SC]E", "", license, ignore.case = FALSE)) %>%
  dplyr::select(view, package, title, license, description, url, authors)

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
      if (length(input$view) > 1) {
        temporary <- packages[packages$view %in% input$view, ]
        if (any(duplicated(temporary$package))) {
          # Some packages appear under multiple views, let's consolidate:
          temporary <- temporary %>%
            dplyr::group_by(package, title, description, url, authors, license) %>%
            dplyr::summarize(view = paste0(view, collapse = ", "), views = dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::select(view, views, package, title, license, authors, description, url)
        }
      } else if (length(input$view) == 1) {
        temporary <- packages %>%
          dplyr::filter(view == input$view) %>%
          dplyr::select(-view)
      } else {
        temporary <- NULL
      }
    } else {
      temporary <- packages %>%
        dplyr::group_by(package, title, license, authors, description, url) %>%
        dplyr::summarize(view = paste0(view, collapse = ", "), views = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(view, views, package, title, license, authors, description, url) %>%
        dplyr::arrange(package)
    }
    if (!is.null(temporary)) {
      if (!"Authors" %in% input$fields) {
        temporary %<>% dplyr::select(-authors)
      }
      if (!"Description" %in% input$fields) {
        temporary %<>% dplyr::select(-description)
      }
      if ("URL" %in% input$fields) {
        temporary$url <- make_url(temporary$url)
      } else {
        temporary %<>% dplyr::select(-url)
      }
    }
    dt(temporary)
  })
  
  output$packages <- DT::renderDataTable({
    req(dt())
    DT::datatable(
      dt(),
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE, escape = FALSE,
      options = list(
        search = list(regex = TRUE, caseInsensitive = TRUE),
        language = list(search = "Filter:"),
        pageLength = 10, lengthMenu = c(5, 10, 25, 50, 100),
        order = list(list(2, "desc"))
      )
    )
  })
  
})

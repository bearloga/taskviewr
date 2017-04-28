## Script for acquiring details for packages listed in CRAN's Task Views
## Its filename starts with a dot so it's not uploaded to shinyapps.io

## Dependencies:
# install.packages(c("ctv", "tidyverse", "devtools"))
# devtools::install_github("metacran/crandb")
library(tidyverse)

message("Downloading CRAN Task Views...")
available_views <- ctv::available.views(repos = "https://cran.rstudio.com/")

# Rename the list's elements:
names(available_views) <- available_views %>%
  map(~ .$name) %>%
  unlist

## For ui.R:
# cat(paste0(unlist(map(available_views, ~ .$topic)), "\" = \"", names(available_views), collapse = "\",\n\""))

## Extract package list for each view:
message("Extracting package list for each View...")
views <- available_views %>%
  map(~ .$packagelist$name) %>%
  map(~ data.frame(package = ., stringsAsFactors = FALSE)) %>%
  bind_rows(.id = "view")

## Obtain licensing and other details for each package:
need_to_write <- FALSE
foo <- function(x) {
  if (is.null(x)) {
    return(NA)
  } else if (x == "") {
    return(NA)
  } else {
    return(x)
  }
}
if (file.exists("www/packages.csv")) {
  message("Found existing data for packages...")
  existing_data <- read.csv("www/packages.csv", stringsAsFactors = FALSE)
  # Check if there have been any packages added to the task views that we
  # do not already have details for:
  missing_pkgs <- anti_join(views, existing_data, by = c("view", "package"))
  if (nrow(missing_pkgs) > 0) {
    message("Found packages in Task Views that are not in existing dataset...")
    # Let's get those missing packages' details!
    pkgs <- unique(missing_pkgs$package)
    message("Acquiring licensing and other data...")
    details <- bind_rows(lapply(pkgs, function(pkg) {
      deets <- crandb::package(pkg)
      return(data.frame(title = deets$Title, license = foo(deets$License), description = deets$Description, url = foo(deets$URL), stringsAsFactors = FALSE))
    }), .id = "package")
    details$package <- pkgs
    packages <- missing_pkgs %>%
      left_join(details, by = "package") %>%
      rbind(existing_data, .) %>%
      arrange(view, package)
    need_to_write <- TRUE
  } else {
    message("No further actions need to be taken.")
  }
} else {
  pkgs <- unique(views$package)
  message("Acquiring licensing and other details for ", length(pkgs), " package(s)...")
  pb <- progress::progress_bar$new(total = length(pkgs))
  details <- bind_rows(lapply(pkgs, function(pkg) {
    pb$tick()
    deets <- crandb::package(pkg)
    return(data.frame(title = deets$Title, license = foo(deets$License), description = deets$Description, url = foo(deets$URL), stringsAsFactors = FALSE))
  }), .id = "package")
  details$package <- pkgs
  packages <- left_join(views, details)
  need_to_write <- TRUE
}

## Output:
if (need_to_write) {
  message("Writing a CSV of package details...")
  packages %>%
    dplyr::arrange(view, package) %>%
    dplyr::distinct(view, package, .keep_all = TRUE) %>%
    write_csv("www/packages.csv")
}
message("Done.")

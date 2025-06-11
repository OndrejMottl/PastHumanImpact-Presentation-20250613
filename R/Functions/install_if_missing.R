install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) renv::install(package_name)
  library(package_name, character.only = TRUE)
}
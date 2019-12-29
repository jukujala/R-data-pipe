
# set all R options, load libraries etc

# Load necessary packages
if (!suppressWarnings(require(pacman))) {
  install.packages("pacman")
  require(pacman)
}
suppressWarnings(
pacman::p_load(
  data.table,
  logging
)
)


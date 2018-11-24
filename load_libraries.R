
# set all R options, load libraries etc

# Load necessary packages
if (!suppressWarnings(require(pacman))) {
  install.packages("pacman")
  require(pacman)
}
suppressWarnings(
pacman::p_load(
  tidyverse,
  data.table,
  logging,
  fasttime,
  caret,
  e1071, #broken dependency in caret LibLinear
  Boruta
  #LiblineaR
)
)


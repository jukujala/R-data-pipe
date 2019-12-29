# install devtools and roxygen2 if not yet installed
# install.packages(c("devtools", "roxygen2"))
library(devtools)
library(roxygen2)

# install dependencies, document, and install this package
# devtools can fail to install dependencies :/
devtools::install_deps(dependencies = TRUE)
devtools::document()
devtools::install()

# run tests:
devtools::load_all()
devtools::test()

# build TAR too
devtools::build()


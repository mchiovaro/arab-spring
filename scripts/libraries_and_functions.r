#### Load necessary packages ####
#
#
#
# Last updated:

# list of required packages
required_packages = c(
  'dplyr',
  'ggplot2',
  'crqa',
  'tidyr',
  'modeest',
  'papaja',
  'pander',
  'gtools',
  'knitr'
)

# load required packages
invisible(lapply(required_packages, require, character.only = TRUE))

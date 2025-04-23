# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#####################
## libraries
#####################
library(shiny)
library(shinyjs)
library(cli)
library(data.table)
library(bslib)
library(bsicons)
library(DT)
library(ggplot2)
library(Rnssp)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(MMWRweek)
library(shinycssloaders)

#####################
## profile
#####################
source("src/01_credentials.R")
CREDENTIALS = get_profile(title = "Bayesian Spatiotemporal Modeling")

#####################
## Helpers and modules
#####################


for(grp in c("helpers", "modules")) {
  for(f in list.files(paste0("src/", grp),pattern=".R$",full.names = T,recursive = T)) {
    source(f)
  }
}
rm(list=c("f", "grp"))


#####################
## constants
#####################
BOOT_PRESET = "pulse"
THEME = bs_theme(version = 5, preset = BOOT_PRESET)
SIDEBAR_WIDTH = 300
DEFAULT_STATES = c("MD")

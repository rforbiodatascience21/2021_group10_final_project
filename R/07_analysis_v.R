rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/02_clean_data.tsv")



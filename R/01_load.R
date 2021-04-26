# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
download.file(url = "https://doi.org/10.1371/journal.pone.0210064.s006",
              destfile = "data/_raw/raw_sanctuary_data")

# Loading all sheets into separate tables
sanctuary_data_GI_behavior <- read_excel("data/_raw/raw_sanctuary_data",
                                         sheet = "GI&Behavior")
sanctuary_data_immune_microbiota <- read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "Immune&Microbiota")
sanctuary_data_fecal_metabolites <- read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "FecalMetabolites")
sanctuary_data_urine_metabolites <- read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "UrineMetabolites")
sanctuary_data_serum_metabolites <- read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "SerumMetabolites")
sanctuary_data_microbiota_ras <- read_excel("data/_raw/raw_sanctuary_data",
                                            sheet = "MicrobiotaRAs")

# Wrangle data ------------------------------------------------------------
my_data <- my_data_raw # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")
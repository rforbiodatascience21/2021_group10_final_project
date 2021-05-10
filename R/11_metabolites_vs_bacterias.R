# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(corrr)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")
any_over_80 = function(x) any(x > .8, na.rm = TRUE)

# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/02_clean_data.tsv")

# Wrangle data ------------------------------------------------------------

# Subsetting microbiome and metabolites data
micro_meta_data <- sanctuary_data %>% 
  select(starts_with("k__"),
         ends_with("_fecal"),
         ends_with("_urine"),
         ends_with("_serum")) %>%
  drop_na()
    

# Creating the long correlation table
res.cor = correlate(micro_meta_data) %>%
  #  focus_if(any_over_80, mirror = TRUE) %>%
  shave() %>%
  stretch(na.rm = TRUE)

# Looking for the correlation superior to 80% among gut vs bacteria

# all
all_data <- res.cor %>%
  filter((str_detect(x,"_fecal") | str_detect(y,"_fecal")) |
         (str_detect(x,"_urine") | str_detect(y,"_urine")) |
         (str_detect(x,"_serum") | str_detect(y,"_serum")),
         !(str_detect(x,"_fecal") & str_detect(y,"_fecal")),
         !(str_detect(x,"_fecal") & str_detect(y,"_urine")),
         !(str_detect(x,"_fecal") & str_detect(y,"_serum")),
         !(str_detect(x,"_serum") & str_detect(y,"_serum")),
         !(str_detect(x,"_serum") & str_detect(y,"_fecal")),
         !(str_detect(x,"_serum") & str_detect(y,"_urine")),
         !(str_detect(x,"_urine") & str_detect(y,"_urine")),
         !(str_detect(x,"_urine") & str_detect(y,"_fecal")),
         !(str_detect(x,"_urine") & str_detect(y,"_serum")),
         !(startsWith(x,"k") & startsWith(y,"k"))
  )


# Correlations of gut metabolites vs bacterias superior to 0.8
high_corr = all_data %>%
  filter((r > 0.8 | r < -0.8))

# fecal
high_cor_fecal = high_corr %>%
  filter((str_detect(x,"_fecal") | str_detect(y,"_fecal")),
         !(str_detect(x,"_fecal") & str_detect(y,"_fecal")))

# urine
high_cor_urine = high_corr %>%
  filter((str_detect(x,"_urine") | str_detect(y,"_urine")),
         !(str_detect(x,"_urine") & str_detect(y,"_urine")))

# serum
high_cor_serum = high_corr %>%
  filter((str_detect(x,"_serum") | str_detect(y,"_serum")),
         !(str_detect(x,"_serum") & str_detect(y,"_serum")))

# Some tries
# butyrate (+ acetate, proprionate) vs all bacterias

res.cor2 = correlate(micro_meta_data)

res.cor2 %>%
  focus(Butyrate_fecal) %>%
  filter(Butyrate_fecal > 0.5)

res.cor2 %>%
  focus(Acetate_fecal,Acetate_urine,Acetate_serum) %>%
  filter(Acetate_fecal > 0.5)


# Visualize data ----------------------------------------------------------

# Distribution of the correlation of the overall data
distribution = all_data %>%
  ggplot(aes(r)) +
  geom_histogram(bins = 10)

# Correlation plot 
correlate(micro_meta_data) %>%
  focus_if(any_over_80, mirror = TRUE) %>%
  rearrange() %>%
  #  shave() %>%
  head() %>%
  rplot()

# Some tries

correlate(micro_meta_data) %>%
  focus("k__Archaea;p__Euryarchaeota",
        Butyrate_fecal,
        Acetate_fecal,
        Mannitol_urine,
        Taurine_urine,
        Galactose_urine,
        Mannitol_urine,
        mirror = TRUE) %>%
  rplot()

# Write data --------------------------------------------------------------
write_tsv()
ggsave()
  rm(list = ls())
  
  
  # Load libraries ----------------------------------------------------------
  library(tidyverse)
  library(patchwork)
  
  # Define functions --------------------------------------------------------
  source(file = "R/99_project_functions.R")
  
  
  # Load data ---------------------------------------------------------------
  data <- read_tsv(file = "data/02_clean_data.tsv")
  
  
  # Wrangle data ------------------------------------------------------------
  # Subsetting the table for analysis
  Gut_metabolites_immune_cells_cytokines_data <- data %>% 
    select(Subject,
           Sample,
           Butyrate_fecal,
           Propionate_fecal,
           Acetate_serum,
           Acetate_urine,
           IL10_CD4,
           TNFa_CD8,
           IL13_CD4)
  
  
  # Visualize data ----------------------------------------------------------
  # Scatterplot visualization of correlation between
  # certain gut metabolites and immune cells or cytokines
  # fecal butyrate and IL10_CD4
  
  
  p1 <- ggplot(data=Gut_metabolites_immune_cells_cytokines_data, 
               mapping=aes(x=Butyrate_fecal, y=IL10_CD4)) + 
    geom_point(colour="blue", size=3, shape=23)+
    ggtitle('Correlation 1')
  
  # Visualize data ----------------------------------------------------------
  # Scatterplot visualization of correlation between
  # certain gut metabolites and immune cells or cytokines
  # Serum acetate and IL13_CD4
  
  p2 <- ggplot(data=Gut_metabolites_immune_cells_cytokines_data, 
               mapping=aes(x=Acetate_serum, y=IL13_CD4)) + 
    geom_point(colour="red", size=3, shape=20)+
    ggtitle('Correlation 2')
  
  # Visualize data ----------------------------------------------------------
  # Scatterplot visualization of correlation between
  # certain gut metabolites and immune cells or cytokines
  # Urine acetate and TNFa_CD8
  
  p3 <- ggplot(data=Gut_metabolites_immune_cells_cytokines_data, 
               mapping=aes(x=Acetate_urine, y=TNFa_CD8)) + 
    geom_point(colour="green", size=3, shape=10)+
    ggtitle('Correlation 3')
  
 
  
    
    


           
    

 

  
  
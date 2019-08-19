library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(feather)
library(ggplot2)
library(plotly)
library(ggbeeswarm)
library(highcharter)
library(shinycssloaders)
library(DT)

mp_dat <- read_feather("filt_nf_mp_res.feather")
plier_loadings <- read_feather("mp_loadings_tidy.feather")

drug_targets <- read_feather('dtex_targets.feather') 

drug_targets <- drug_targets %>% 
  filter(mean_pchembl > 8) %>% 
  mutate(gene= hugo_gene) %>% 
  group_by(gene) %>% 
  top_n(20, mean_pchembl) %>% 
  select(gene, std_name) 
  
plier_loadings_individual_drugs <- plier_loadings %>% 
  left_join(drug_targets) %>% 
  dplyr::group_by(lv) %>% 
  filter(quantile(weight, 0.95)<weight) %>% 
  filter(!is.na(std_name)) %>% 
  mutate(druggable_targets_in_lv = sum(!is.na(std_name))) %>% 
  ungroup() %>% 
  select(lv, gene, druggable_targets_in_lv, std_name) %>% 
  distinct()
  
drug_targets <- drug_targets %>% 
  summarise(std_name = paste(std_name, collapse=" ")) %>% 
  mutate(`Drug Name` = stringr::str_wrap(std_name, width = 20)) %>% 
  mutate(`Drug Name` = case_when(is.na(`Drug Name`) ~ "",
                                 !is.na(`Drug Name`) ~ `Drug Name`)) %>% 
  ungroup()

plier_loadings <- plier_loadings %>% left_join(drug_targets)

thm <- hc_theme(
  chart = list(
    backgroundColor = NULL,
    divBackgroundImage = "insert here"
  # ),
  # title = list(
  #   style = list(
  #     color = '#333333',
  #     fontFamily = "Lato"
  #   )
  # ),
  # subtitle = list(
  #   style = list(
  #     color = '#666666',
  #     fontFamily = "Shadows Into Light"
  #   )
  # ),
  # legend = list(
  #   itemStyle = list(
  #     fontFamily = 'Tangerine',
  #     color = 'black'
  #   ),
  #   itemHoverStyle = list(
  #     color = 'gray'
  #   )   
   )
)

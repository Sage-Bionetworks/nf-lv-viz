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
library(textshape)
library(emojifont)
library(DT)


.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") {
  	venv_folder<-'./python3_env'
  	if (!file.exists(venv_folder)) {
    	# Install Python and the Synapse Python client
    	# Ideally this would be done prior to deploying the app' to ShinyApps
    	# but the huge number of installed files causes the deployable artifact
    	# to exceed the 10,000 file limit.  The effect of doing it here is a slow
    	# start up the first time the app' is run.
    	# From https://stackoverflow.com/questions/54651700/use-python-3-in-reticulate-on-shinyapps-io
    	reticulate::virtualenv_create(envname = venv_folder, python = '/usr/bin/python3')
    	reticulate::virtualenv_install(venv_folder, packages = c('synapseclient', 'pandas'))
     }
     reticulate::use_virtualenv(venv_folder, required = T)
  }
  synapse_package <<- reticulate::import("synapseclient", delay_load = TRUE)
  synapse <<- synapse_package$Synapse()
}

mp_dat <- read_feather("data/filt_nf_mp_res.feather") %>% 
  group_by(specimenID, latent_var) %>%  ##short term fix for duplicated analyses
  slice(1)

co_dat <- read_feather("data/filt_nf_cogaps.feather")

pr_dat <- read_feather("data/filt_nf_prmf.feather")

grouping_var_options <- c("tumorType", "diagnosis", "species", 
                          "isCellLine", 'nf1Genotype', 'nf2Genotype',
                          "studyName", "cellType", "modelOf")

method_options <- c("MultiPLIER", "CoGAPS", "PRMF")

plier_loadings <- read_feather("data/mp_loadings_tidy.feather") %>% 
  dplyr::group_by(lv) %>% 
  filter(quantile(weight, 0.95)<weight) %>% 
  ungroup()

cogaps_loadings <- read_feather("data/cogaps_loadings_tidy.feather") %>% 
  dplyr::group_by(lv) %>% 
  filter(quantile(weight, 0.95)<weight) %>% 
  ungroup()

prmf_loadings <- read_feather("data/prmf_loadings_tidy.feather") %>% 
  dplyr::group_by(lv) %>% 
  filter(quantile(weight, 0.95)<weight) %>% 
  ungroup()

drug_targets <- read_feather('data/dtex_targets.feather') 

drug_targets <- drug_targets %>% 
  filter(mean_pchembl > 8) %>% 
  mutate(gene= hugo_gene) %>% 
  group_by(gene) %>% 
  top_n(20, mean_pchembl) %>% 
  select(gene, std_name) 
  
plier_loadings_individual_drugs <- plier_loadings %>% 
  left_join(drug_targets) %>% 
  dplyr::group_by(lv) %>% 
  filter(!is.na(std_name)) %>% 
  mutate(druggable_targets_in_lv = sum(!is.na(std_name))) %>% 
  ungroup() %>% 
  select(lv, gene, druggable_targets_in_lv, std_name) %>% 
  distinct()

cogaps_loadings_individual_drugs <- cogaps_loadings %>% 
  left_join(drug_targets) %>% 
  dplyr::group_by(lv) %>% 
  filter(!is.na(std_name)) %>% 
  mutate(druggable_targets_in_lv = sum(!is.na(std_name))) %>% 
  ungroup() %>% 
  select(lv, gene, druggable_targets_in_lv, std_name) %>% 
  distinct()

prmf_loadings_individual_drugs <- prmf_loadings %>% 
  left_join(drug_targets) %>% 
  dplyr::group_by(lv) %>% 
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
cogaps_loadings <- cogaps_loadings %>% left_join(drug_targets)
prmf_loadings <- prmf_loadings %>% left_join(drug_targets)

plier <- list(mp_dat, plier_loadings, plier_loadings_individual_drugs)
cogaps <- list(co_dat, cogaps_loadings, cogaps_loadings_individual_drugs)
prmf <- list(pr_dat, prmf_loadings, prmf_loadings_individual_drugs)
  
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


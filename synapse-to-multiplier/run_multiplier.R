
library(tximport)
library(dplyr)

synapse$login()

library(org.Hs.eg.db)
source('https://raw.githubusercontent.com/greenelab/multi-plier/7c56a2867f9f08f7bcd3617875c7c2dbe886cbeb/util/plier_util.R')

plier_model <- readr::read_rds(synapse$get("syn18689545")$path)

`%>%` <- magrittr::`%>%`

tx2gene_df <- synapse$get('syn18482848')$path %>% readr::read_tsv()


metadata <- synapse$tableQuery("SELECT * FROM syn16858331 where fileFormat = 'sf'", 
                          includeRowIdAndRowVersion=F)$asDataFrame()

metadata$isCellLine <- toupper(metadata$isCellLine)

rownames(metadata) <- metadata$id


salmon_entities <- sapply(metadata$id, synapse$get)

salmon_filepaths <- sapply(salmon_entities, function(x){
  x$path
})

tx_data <- tximport(salmon_filepaths, type = 'salmon', tx2gene = tx2gene_df,
                    countsFromAbundance = "no", ignoreTxVersion = TRUE)

abundance <- tx_data$abundance %>% as.data.frame() 

mapped_list <- mapIds(org.Hs.eg.db, keys = rownames(abundance), column = "SYMBOL", 
                      keytype = "ENSEMBL", multiVals = "list")

mapped_df <- reshape2::melt(mapped_list)

colnames(mapped_df) <- c("SYMBOL", "ENSEMBL")

abundance$gene_id <- rownames(abundance)

# let's use the mappings to reannotate our data
abundance <- mapped_df %>%
  # removing anything in the SYMBOL column that is NA
  filter(!is.na(SYMBOL)) %>%
  # join by Ensembl gene ids, retaining only Ensembl gene ids in both
  inner_join(y = abundance, by = c("ENSEMBL" = "gene_id")) %>% 
  dplyr::select(-ENSEMBL) %>% 
  add_count(SYMBOL)

duplicate_genes <- abundance %>% filter(n > 1) %>% dplyr::select(-n)

abundance_filt <- abundance  %>% 
  filter(n == 1) %>% 
  tibble::column_to_rownames("SYMBOL") %>% 
  dplyr::select(-n)  %>%
  as.matrix()

abundance_filt_cutoff <- abundance_filt[rowSums(abundance_filt) > 10,]

mp_res <- GetNewDataB(abundance_filt_cutoff, plier_model)

mp_res_tidy <- mp_res %>% 
  as.data.frame(.) %>% 
  tibble::rownames_to_column('latent_var') %>% 
  tidyr::gather("id","value",-latent_var) %>% 
  group_by(latent_var) %>% 
  mutate(sd_value = sd(value)) %>% 
  ungroup() %>% 
  mutate(trunc_label = stringr::str_trunc(latent_var, 10, "right")) %>% 
  left_join(metadata)

mp_res_tidy_filt <- mp_res_tidy %>% 
  filter(sd_value > 0.05)

mp_res_filt <- mp_res[rownames(mp_res) %in% unique(mp_res_tidy_filt$latent_var),]

plier_loadings <- plier_model$Z 
colnames(plier_loadings) <- rownames(mp_res)

plier_cor <- cor(plier_loadings) 

interesting_lvs <- mp_res_tidy_filt$latent_var 

plier_cor_tidy <- plier_cor %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column('lv1') %>% 
  tidyr::gather("lv2","cor",-lv1) %>% 
  filter(lv1 != lv2) %>% 
  distinct() %>%
  mutate(high_sd=case_when((lv1 %in% interesting_lvs & lv2 %in% interesting_lvs) ~ TRUE,
                           (!lv1 %in% interesting_lvs | !lv2 %in% interesting_lvs) ~ FALSE))

high_cor_rm <- plier_cor_tidy %>% filter(cor > 0.5 & high_sd == TRUE) %>% group_by(cor) %>% dplyr::slice(2) %>% ungroup()

mp_res_filt <- mp_res_filt[!rownames(mp_res_filt) %in% high_cor_rm$lv1,]

mp_res_tidy_filt <- filter(mp_res_tidy_filt, !latent_var %in% high_cor_rm$lv1)

library(feather)

mp_res_tidy_filt %>% write_feather("filt_nf_mp_res.feather")

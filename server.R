shinyServer(function(session, input, output) {
  
  lv_dat_rct <- eventReactive({input$method_var}, {
    if(input$method_var=="MultiPLIER"){
      foo <- plier
    }
    if(input$method_var=="CoGAPS"){
      foo <- cogaps
    }
    if(input$method_var=="PRMF"){
      foo <- prmf
    }
    foo
  })
  
  output$app_basics <- renderUI(h6("The purpose of this tool is to explore expression profiles of latent variables in neurofibromatosis and schwannomatosis tumors and cell line models. Generally speaking, a latent variable
                                   is a weighted combination of genes that in combination may represent known biological pathways, novel biological mechanisms, or other complex gene-gene relationships. For this analysis, all 
                                   latent variables were identified using a large RNAseq dataset representative of many types of human tissues and conditions. The expression of these variables was then assessed in harmonized NF expression data. 
                                   To perform your own analysis, first select a method (MultiPLIER, CoGAPS, or PRMF), and then select a grouping variable. This variable will be the basis for building a cohort and comparing expression across groups.
                                   Then, begin typing in the 'Select groups:' box to add groups of samples to your cohort. Then hit Plot! On the global tab, this will generate a heatmap of latent variables by sample and a principal components analysis
                                   of these data. On the Individual LV's tab, you can dive into the expression of individual latent variables in the data,
                                   and explore which genes in the latent variables are druggable. "))
 
  output$data_ctb <- renderUI(h6(HTML("This analysis used RNASeq data from these contributors: <br>
    -The Synodos for NF2 Consortium (James Gusella, MGH; Gary Johnson, UNC) <br>
    -David Largaespada, Masonic Cancer Center, University of Minnesota <br>
    -Angela Hirbe, Washington University School of Medicine in St Louis <br>
    -Christine Pratilas, Johns Hopkins <br>
    -Antonio Iaverone, Columbia University <br>
    -Margaret (Peggy) Wallace, University of Florida  <br>
    -Adam Resnick, University of Pennsylvania <br>")))
  
  shape_var <- "isCellLine"
  
  samp_metadata <- eventReactive({
    input$group_var
    input$method_var
    }, {
    grouping_var <- input$group_var
    lv_dat <- lv_dat_rct()
    samp_metadata <- select(lv_dat[[1]], specimenID, {{ grouping_var }},  {{ shape_var }}) %>% distinct()
  })
  
  
  observeEvent({
    input$group_var
    input$method_var
  }, {
    
    lv_dat <- lv_dat_rct()
    grouping_var <- input$group_var
    choices <- lv_dat[[1]] %>% 
      purrr::pluck(grouping_var) %>% 
      unique()
    updateSelectInput(session, "grp_opts",
                      label = NULL,
                      selected = choices[1],
                      choices = choices)
  })
  
  observeEvent({
    input$group_var
    input$method_var
  }, {
    lv_dat <- lv_dat_rct()
    updateSelectInput(session, "lv_view",
                      label = NULL,
                      selected = NULL,
                      choices = lv_dat[[1]] %>% 
                        purrr::pluck('latent_var') %>% 
                        unique())
  })
  
  heatmap_dat <- eventReactive({
    input$goButton
    }, {
    lv_dat <- lv_dat_rct()
    grouping_var <- input$group_var
    dat <- lv_dat[[1]] %>%
      dplyr::filter(!!rlang::sym(grouping_var) %in% input$grp_opts) %>% 
      dplyr::select(latent_var, specimenID, value) %>% 
      tidyr::spread(latent_var, value) %>% 
      tibble::column_to_rownames("specimenID")
    
    dat
    
  })
  
    output$lv_heatmap <- renderHighchart2({
       chart<- hchart(heatmap_dat() %>% as.matrix() %>% textshape::cluster_matrix(., dim = "both")) %>% 
          hc_chart(
            zoomType = "xy"
          )
       enable("goButton")
       chart
    })
    
    output$lv_pca <- renderPlot({
      grouping_var <- input$group_var
      metadata <- samp_metadata()
      
     pca <- prcomp(heatmap_dat())
     pca <- as.data.frame(pca$x) %>% 
       tibble::rownames_to_column("specimenID") %>% 
       left_join(metadata) %>% 
       select(PC1, PC2, {{ grouping_var }},  {{ shape_var }}, specimenID)
     
     p <- ggplot(pca) + 
       geom_point(aes(x = PC1, y = PC2, color = get(grouping_var), shape = get(shape_var))) +
       theme_bw() +
       scale_color_discrete(name = {{grouping_var}}) +
       scale_shape_discrete(name = {{shape_var}})
       
     p
     })
    
    plot_dat <- eventReactive({
      input$goButton
      }, {
      lv_dat <- lv_dat_rct()
      grouping_var <- input$group_var
      dat <- lv_dat[[1]] %>%
        dplyr::filter(!!rlang::sym(grouping_var) %in% input$grp_opts) 
      dat
    })
    
    output$individual_lv_dotplot <- renderPlot({
      grouping_var <- input$group_var
      
      dotplot_data <- plot_dat() %>%
        dplyr::filter(latent_var == input$lv_view)
      
      anova_res <- "add 2 or more groups to perform anova"

      if(length(input$grp_opts) > 1){
      foo <- aov(as.formula(sprintf("value ~ %s", grouping_var)), data = dotplot_data) 
        res <- summary(foo)[[1]][[1,"Pr(>F)"]] %>% 
          signif(., digits = 3)
        anova_res <- paste0("ANOVA p-value: ", res)
      }
      
      p1 <- ggplot(data = dotplot_data %>%
                     filter(latent_var == input$lv_view) %>% 
                     filter(!!rlang::sym(grouping_var) %in% input$grp_opts)) +
        ggbeeswarm::geom_quasirandom(aes(x=reorder(latent_var, -sd_value), y = value , color = get(grouping_var), 
                                         group = get(grouping_var), 
                                         shape = get(shape_var)), dodge.width = 0.75) +
        theme_bw() +
        scale_color_discrete(name = {{grouping_var}}) +
        scale_shape_discrete(name = {{shape_var}}) +
        theme(axis.text.x = element_text(size = 10, angle = 0)) +
        theme(plot.title = element_text(face = "bold", color = "red")) +
        labs(x = "", y = "LV expression", title = anova_res)
    
      p1
      
    })
  
    output$lv_loading_barplot <- renderPlotly({
        
      lv_dat <- lv_dat_rct()
      
        loadings <- lv_dat[[2]] %>%
            dplyr::filter(lv == input$lv_view)  
        
  
         p2 <- ggplot(loadings) +
                geom_bar(aes(x=reorder(gene, -weight), y=weight, fill = gene %in% drug_targets$gene, label = `Drug Name`), stat = "identity") +
                scale_fill_manual(name = "Is Drug\nTarget", values= c("TRUE" = "#28AFB0", "FALSE" = "#004BA8")) +
                labs(x = "Gene", y = "LV Loading") +
             theme_bw() +
             theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))  
         
        plotly::ggplotly(p2, tooltip = c("x", "label"), dynamicTicks = T)    
        
        })
    
     output$druggable_lvs <- renderHighchart({
       
       lv_dat <- lv_dat_rct()
    
       foo <- lv_dat[[3]] %>% 
         dplyr::filter(lv == input$lv_view) %>% 
         dplyr::count(std_name)
       
       hchart(foo, "treemap", hcaes(x = std_name, value = n))
       
       })
})

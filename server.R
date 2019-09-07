shinyServer(function(session, input, output) {
  
  lv_dat_rct <- eventReactive({input$method_var}, {
    if(input$method_var=="MultiPLIER"){
      foo <- plier
    }
    if(input$method_var=="CoGAPS"){
      foo <- cogaps
    }
    foo
  })
  
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
    input$method_var
    input$group_var
    input$grp_opts
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
      input$lv_view
      input$grp_opts
      input$group_var
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
    
       foo <- plier_loadings_individual_drugs %>% 
         dplyr::filter(lv == input$lv_view) %>% 
         dplyr::count(std_name)
       
       hchart(foo, "treemap", hcaes(x = std_name, value = n))
       
       })
})

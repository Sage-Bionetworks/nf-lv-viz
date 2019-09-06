dashboardPage(
    # Application title
    dashboardHeader(title = "braiNFood"),

    dashboardSidebar(
        h5("Select transfer learning method:", align = 'center'),
        selectInput("method_var",
                    label = NULL,
                    selected = "MultiPLIER",
                    choices = grouping_var_options),
        h5("Select grouping variable:", align = 'center'),
        selectInput("group_var",
                       label = NULL,
                       selected = "tumorType",
                       choices = grouping_var_options),
            h5("Select groups:", align = 'center'),
            selectizeInput("grp_opts",
                           label = NULL,
                           selected = NULL,
                           choices =NULL,
                           options = list(maxItems = 5, 
                                          placeholder = 'type in some categories')),
            actionButton("goButton", "Plot!")
        ),

            dashboardBody( 
                shinyjs::useShinyjs(),
            tabsetPanel(
                tabPanel(title = "Global Summary",
                         fluidRow(box("Sample by LV Heatmap", status = 'primary',
                         highchartOutput2("lv_heatmap") %>% withSpinner()),
                         box("PCA of LVs", status = 'primary',
                         plotOutput("lv_pca") %>% withSpinner()
                         ))),
                tabPanel(title = "Individual LVs",
                         h5("Select a Latent Variable to Explore:"),
                         selectInput("lv_view", 
                        label = NULL,
                        choices = NULL,
                        multiple = FALSE),
                        fluidRow(
                            box("LV Expression by Sample: This module plots the expression of each LV grouped by biological sample." ,status = 'primary',
                                plotOutput("individual_lv_dotplot") %>% withSpinner(),
                                width = 6),
                            box("LV-Gene Loading: This module plots the 5% highest-loading genes in the selected latent variable.", status = 'primary',
                             plotlyOutput("lv_loading_barplot") %>% withSpinner(),
                        width = 6)),
                       fluidRow(
                           box("Drug Target Overlap: This module identifies molecules with molecular targets that overlap with in the top 5% of gene loadings in the selected latent variable. Block size indicates the relative number of targets (hover to see exact number).", status = 'primary',
                               highchartOutput("druggable_lvs") %>% withSpinner(),
                        width = 12))
            ))
    )
)

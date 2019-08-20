dashboardPage(
    # Application title
    dashboardHeader(title = "braiNFood"),

    dashboardSidebar(
            h5("Select tumor types:", align = 'center'),
            selectizeInput("tums",
                           label = NULL,
                           selected = NULL,
                           choices = unique(unique(mp_dat$tumorType)),
                           options = list(maxItems = 5, 
                                          placeholder = 'type in a tumor type')),
            actionButton("goButton", "Plot!")
        ),

            dashboardBody( 
                shinyjs::useShinyjs(),
            tabsetPanel(
                tabPanel(title = "Global Summary",
                         fluidRow(box("Sample by LV Heatmap", status = 'primary',
                         highchartOutput2("lv_heatmap") %>% withSpinner()),
                         box("PCA of LVs", status = 'primary',
                         highchartOutput("lv_pca") %>% withSpinner()
                         ))),
                tabPanel(title = "Individual LVs",
                         h5("Select a Latent Variable to Explore:"),
                         selectInput("lv_view", 
                        label = NULL,
                        choices = unique(mp_dat$latent_var),
                        multiple = FALSE),
                        fluidRow(
                       box("LV Expression by Sample" ,status = 'primary',
                         plotOutput("individual_lv_dotplot") %>% withSpinner(),
                         width = 6),
                       box("LV-Gene Loading", status = 'primary',
                        plotlyOutput("lv_loading_barplot") %>% withSpinner(),
                        width = 6)),
                       fluidRow(
                       box("LV-Drug Target Overlap", status = 'primary',
                        highchartOutput("druggable_lvs") %>% withSpinner(),
                        width = 12))
            ))
    )
)

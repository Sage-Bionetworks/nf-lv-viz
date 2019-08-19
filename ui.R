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
                         fluidRow(box(
                         highchartOutput2("lv_heatmap") %>% withSpinner()),
                         box(
                         highchartOutput("lv_pca") %>% withSpinner()
                         ))),
                tabPanel(title = "Individual LVs",
                         h5("Select a Latent Variable to Explore:"),
                         selectInput("lv_view", 
                        label = NULL,
                        choices = unique(mp_dat$latent_var),
                        multiple = FALSE),
                        fluidRow(
                       box(
                         plotOutput("individual_lv_dotplot"),
                         width = 6),
                       box(
                        plotlyOutput("lv_loading_barplot"),
                        width = 6)),
                       fluidRow(
                       box(
                        highchartOutput("druggable_lvs"),
                        width = 12))
            ))
    )
)

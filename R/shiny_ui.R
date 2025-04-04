shiny_ui <- bslib::page_navbar(
    title = "Bayesian Divide and Conquer",

    # Add bslib theme
    theme = bslib::bs_theme(
        version = 4,
        bootswatch = "minty", # Use a clean, professional theme
        primary = "#007BFF",  # Blue for primary theme
        secondary = "#6C757D", # Gray for secondary theme
        success = "#28A745",
        info = "#17A2B8",
        warning = "#FFC107",
        danger = "#DC3545",
        base_font = bslib::font_google("Roboto"),
        heading_font = bslib::font_google("Roboto Slab")
    ),

    # Tab 1: Data Input
    bslib::nav_panel(
        "Data Input",
        icon = icon("upload"),
        sidebarLayout(
            sidebarPanel(
                class = "sidebar",  # Add styling class for sidebar
                fileInput("current_trial", "Upload Current Trial Data (CSV):"),
                fileInput("external_trial", "Upload External Trial Data (CSV):"),
                textInput("response", "Response Variable:", value = "Y"),
                textInput("covariates", "Covariates (comma-separated):", value = "x1,x2,x3,x4,x5,x6,x7,x8,x9"),
                textInput("group", "Group Variable:", value = "group"),
                textInput("group_levels", "Group Levels (comma-separated, order: Treatment, Control, External):",
                          value = "Treatment, Control, External"),
                actionButton("combine_btn", "Combine Trials", class = "btn-primary"),
                textInput("formula", "Propensity Score Formula (RHS):",
                          value = "x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9"),
                actionButton("propensity_btn", "Calculate Propensity Scores", class = "btn-success"),
                downloadButton("download_data", "Download Combined Data", class = "btn-success"),
                uiOutput("home_message")
            ),
            mainPanel(
                div(
                    class = "block",
                    h3("Combined Data Table"),
                    div(class = "table-container", DT::DTOutput("main_combined_table"))
                )
            )
        )
    ),

    # Tab 2: Plots
    bslib::nav_panel(
        "Plots",
        icon = icon("chart-line"),
        sidebarLayout(
            sidebarPanel(
                class = "sidebar",
                h4("Plot Options"),
                selectInput("plot_var", "Select Variable:", choices = NULL),
                selectInput("plot_device", "Select File Format:",
                            choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg")),
                numericInput("plot_width", "Plot Width (inches):", value = 8, min = 1),
                numericInput("plot_height", "Plot Height (inches):", value = 6, min = 1),
                downloadButton("download_plot", "Download Plot", class = "btn-success")
            ),
            mainPanel(
                div(
                    class = "block",
                    h3("Generated Plot"),
                    plotOutput("combined_plot", height = "500px")
                )
            )
        )
    ),

    # Tab 3: Divide
    bslib::nav_panel(
        "Divide",
        icon = icon("layer-group"),
        sidebarLayout(
            sidebarPanel(
                class = "sidebar",
                h4("Divide Method"),
                numericInput("num_strata", "Number of Strata:", value = 5, min = 2),
                actionButton("divide_btn", "Divide!", class = "btn-primary")
            ),
            mainPanel(
                fluidRow(
                    column(
                        width = 12,
                        div(
                            class = "block",
                            h3("Summary Table"),
                            div(class = "table-container", DT::DTOutput("ps_summary_table"))
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        div(
                            class = "block",
                            h3("PS Density Plot"),
                            div(class = "table-container", plotOutput("ps_density_plot", height = "500px"))
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        div(
                            class = "block",
                            h3("Response Density Plot"),
                            div(class = "table-container", plotOutput("response_density_plot", height = "500px"))
                        )
                    )
                )
            )
        )
    ),


    # Tab 4: Discounting Coefficients
    bslib::nav_panel(
        "Minimal Plausibility Index",
        icon = icon("calculator"),
        sidebarLayout(
            sidebarPanel(
                class = "sidebar",
                h4("Number of Strata:"),
                textOutput("mPI_num_strata"),
                actionButton("mPI_btn", "Calculate mPI", class = "btn-primary")
            ),
            mainPanel(
                div(
                    class = "block",
                    h3("mPI Table"),
                    div(class = "table-container", DT::DTOutput("mPI_table"))
                ),
                div(
                    class = "block",
                    plotOutput("mpi_plot_grid")
                )
            )
        )
    ),

    # Tab 5: Conquer
    bslib::nav_panel(
        "Conquer!",
        icon = icon("flag-checkered"),
        sidebarLayout(
            sidebarPanel(
                class = "sidebar",
                actionButton("conquer_btn", "Conquer!", class = "btn-primary")
            ),
            mainPanel(
                div(
                    class = "block",
                    h3("Results Table"),
                    div(class = "table-container", DT::DTOutput("conquer_table")),
                    downloadButton("download_conquer", "Download Results", class = "btn-success")
                ),
                div(
                    class = "block",
                    h3("Forest Plot"),
                    plotOutput("forest_plot", height = "500px")
                )
            )
        )
    )
)

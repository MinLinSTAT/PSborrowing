# Function to read CSV from the package directory
read_default_csv <- function(filename) {
    file_path <- system.file("extdata", filename, package = "PSborrowing")
    if (file_path == "") {
        stop(paste("File not found:", filename))
    }
    readr::read_csv(file_path)
}

# Unified function to read either user-uploaded or default data
read_trial_data <- function(input_data, default_filename) {
    if (!is.null(input_data)) {
        # If uploaded, use the uploaded file
        return(readr::read_csv(input_data$datapath))
    } else {
        # Otherwise, use the default file from the package
        return(read_default_csv(default_filename))
    }
}


shiny_server <- function(input, output, session) {
    # Reactive values to store combined data
    values <- reactiveValues(combined = NULL, stratified = NULL)
    home_message <- reactiveVal("Please upload trial data or use default data.")

    # Observe combine button click
    observeEvent(input$combine_btn, {
        home_message("Combining trials... Please wait.")

        tryCatch({
            # Load trial data
            current_trial <- read_trial_data(input$current_trial, "current1.csv")
            external_trial <- read_trial_data(input$external_trial, "external1.csv")

            # Combine trials
            print("Debug: Before combining trials")
            values$combined <- combine_trials(
                current_trial = current_trial,
                external_trial = external_trial,
                response = input$response,
                covariates = strsplit(input$covariates, ",")[[1]] %>% trimws(),
                group = input$group,
                group_levels = strsplit(input$group_levels, ",")[[1]] %>% trimws()
            )

            home_message("Trials combined successfully! Check the table below.")
        }, error = function(e) {
            home_message(paste("Error:", e$message))
            print(paste("Error in combine_trials:", e$message))
        })
    })

    # Render the dynamic home message
    output$home_message <- renderUI({
        h4(home_message())
    })

    # Render combined table or placeholder
    output$main_combined_table <- DT::renderDT({
        if (is.null(values$combined)) {
            DT::datatable(
                tibble::tibble(Message = "No data available. Please upload files and click 'Combine Trials'."),
                options = list(dom = "t"),
                rownames = FALSE
            )
        } else {
            combined_data <- values$combined

            # Check if "PS" column exists
            if ("ps" %in% names(combined_data)) {
                DT::datatable(
                    combined_data,
                    options = list(
                        pageLength = 20,
                        autoWidth = TRUE,
                        scrollX = TRUE
                    ),
                    rownames = FALSE
                ) %>%
                DT::formatStyle(
                    'ps',
                    backgroundColor = DT::styleInterval(0.5, c('lightblue', 'lightgreen'))
                )
            } else {
                DT::datatable(
                    combined_data,
                    options = list(
                        pageLength = 20,
                        autoWidth = TRUE,
                        scrollX = TRUE
                    ),
                    rownames = FALSE
                )
            }
        }
    })

    # Observe propensity score calculation
    observeEvent(input$propensity_btn, {
        req(values$combined, input$formula)

        home_message("Calculating propensity scores... Please wait.")

        tryCatch({
            # Run summary() to calculate propensity scores
            combined_ps <- estimate_ps(
                values$combined,
                formula = as.formula(paste("code ~", input$formula))
            )

            # Replace the original combined data with the updated one
            values$combined <- combined_ps

            showNotification("Data table updated successfully!", type = "message", duration = 3)
            home_message("Propensity scores calculated successfully!")
        }, error = function(e) {
            home_message(paste("Error calculating propensity scores:", e$message))
        })
    })

    # Download current data
    output$download_data <- downloadHandler(
        filename = function() {
            paste("combined_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(values$combined, file, row.names = FALSE)
        }
    )

    # Dynamically populate plot controls based on combined data
    observe({
        req(values$combined)
        combined <- values$combined

        # Filter out unwanted columns and set default to "ps"
        valid_vars <- setdiff(names(combined), c("Y", "group", "code"))
        default_var <- if ("ps" %in% valid_vars) "ps" else valid_vars[1]


        updateSelectInput(session, "plot_var", choices = valid_vars, selected = default_var)
    })

    combined_plot <- reactiveValues(current_plot = NULL)

    # Generate plots based on user input
    observe({
        req(values$combined, input$plot_var)
        combined <- values$combined

        # Generate plots based on user input
        output$combined_plot <- renderPlot({
            req(values$combined, input$plot_var)

            plot <- NULL
            tryCatch({
                # Check if the variable is binary (0/1)
                is_binary <- all(combined[[input$plot_var]] %in% c(0, 1))
                print(paste("Is binary:", is_binary))  # Debug message

                if (is_binary) {
                    plot_data <- combined %>%
                        dplyr::group_by(group, value = !!rlang::sym(input$plot_var)) %>%
                        dplyr::summarize(count = dplyr::n(), .groups = "drop")

                    # Use Stacked Bar Plot for binary data: each bar represents a group, filled by 0/1 proportion
                    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = count, fill = forcats::as_factor(value))) +
                        ggplot2::geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
                        ggplot2::theme_minimal() +
                        ggplot2::scale_y_continuous(labels = scales::percent) +
                        ggplot2::labs(title = "Stacked Bar Plot for Binary Variable by Group",
                             x = "Group", y = "Proportion", fill = "Value")
                } else {
                    plot <- ggplot2::ggplot(combined, ggplot2::aes(x = !!rlang::sym(input$plot_var), fill = group)) +
                        ggplot2::geom_density(alpha = 0.7) +
                        ggplot2::theme_minimal() +
                        ggplot2::labs(title = "Density Plot for Continuous Variable", x = input$plot_var, y = "Density")
                }

                # Store the generated plot
                if (!is.null(plot)) {
                    combined_plot$current_plot <- plot
                    print("Plot generated successfully.")  # Debug message
                }
                print(plot)
            }, error = function(e) {
                showNotification(paste("Plot Error:", e$message), type = "error")
            })
        })
    })
    # Download current plot
    output$download_plot <- downloadHandler(
        filename = function() {
            paste0("combined_plot_", Sys.Date(), ".", input$plot_device)
        },
        content = function(file) {
            ggplot2::ggsave(
                file,
                plot = combined_plot$current_plot,
                device = input$plot_device,
                width = input$plot_width,
                height = input$plot_height
            )
        }
    )

    for_res <- reactiveValues(summ = NULL, alpha = NULL, K = NULL, conquer = NULL)

    # PS Stratification logic
    observeEvent(input$divide_btn, {
        req(values$combined, input$num_strata)

        tryCatch({
            # Stratify data
            values$stratified <- ps_stratify(values$combined, input$num_strata)

            # Generate summary
            summ <- summary(values$stratified)

            # Store results in reactive values
            for_res$summ <- summ
            for_res$K <- input$num_strata

            # Notification of success
            showNotification("PS stratification successful!", type = "message")
            print("PS stratification completed.")
        }, error = function(e) {
            showNotification(paste("Error in PS stratification:", e$message), type = "error")
            print(paste("Error:", e$message))
        })
    })

    # Function to render PS density plot
    render_ps_density_plot <- function() {
        req(values$stratified, for_res$K)
        plot_data <- dplyr::bind_rows(
            values$stratified %>% dplyr::mutate(stratum = "overall"),
            values$stratified %>% dplyr::mutate(stratum = as.character(stratum))
        ) %>% dplyr::mutate(
            stratum = factor(stratum, levels = c("overall", as.character(1:for_res$K)))
        )
        ggplot2::ggplot(plot_data, ggplot2::aes(x = ps, fill = group)) +
            ggplot2::facet_wrap(~ stratum, scales = "free") +
            ggplot2::geom_density(alpha = 0.7) +
            ggplot2::theme_minimal() +
            ggplot2::labs(
                title = "PS Distribution by Stratum",
                x = "Propensity Score",
                fill = "Group"
            )
    }


    # Function to render response boxplot
    render_response_density_plot <- function() {
        req(values$stratified, for_res$K)
        plot_data <- dplyr::bind_rows(
            values$stratified %>% dplyr::mutate(stratum = "overall"),
            values$stratified %>% dplyr::mutate(stratum = as.character(stratum))
        ) %>% dplyr::mutate(
            stratum = factor(stratum, levels = c("overall", as.character(1:for_res$K)))
        )
        ggplot2::ggplot(plot_data, ggplot2::aes(x = stratum, y = Y, fill = group)) +
            ggplot2::geom_boxplot() +
            ggplot2::theme_minimal() +
            ggplot2::labs(
                title = "Response Boxplots by Stratum",
                x = "Stratum",
                fill = "Group"
            )
    }

    # Render the summary table
    output$ps_summary_table <- DT::renderDT({
        req(for_res$summ)
        DT::datatable(
            for_res$summ,
            options = list(
                pageLength = 20,
                dom = 't',
                autoWidth = TRUE,
                scrollX = TRUE
            ),
            rownames = FALSE
        ) %>%
            DT::formatRound(columns = c('Y_bar', 'S_sq'), digits = 3)
    })

    # Render the PS density plot
    output$ps_density_plot <- renderPlot({
        render_ps_density_plot()
    })

    # Render the response density plot
    output$response_density_plot <- renderPlot({
        render_response_density_plot()
    })


    output$mPI_num_strata <- renderText({
        req(for_res$K)
        paste(for_res$K, "strata were used in PS Stratification.")
    })


    observeEvent(input$mPI_btn, {
        if (is.null(for_res$summ) || is.null(for_res$K)) {
            # Show default message if PS stratification hasn't been carried out
            output$mPI_table <- DT::renderDT({
                DT::datatable(
                    data.frame(Message = "Please carry out PS stratification first on the 'Divide' tab."),
                    options = list(
                        dom = 't',  # Minimal layout without search/pagination
                        autoWidth = TRUE
                    ),
                    rownames = FALSE
                )
            })

            output$mpi_plot_grid <- renderPlot({
                ggplot2::ggplot() + ggplot2::theme_void() +
                    ggplot2::labs(title = "No Data Available: Perform PS Stratification First")
            })

            return()  # Exit the observeEvent early
        }

        tryCatch({
            K <- for_res$K
            summ <- for_res$summ
            alpha_tb <- find_alpha(summ, details = TRUE)
            alpha <- alpha_tb %>% dplyr::select(stratum, mPI_mean, mPI_variance)
            showNotification("mPI calculation completed!", type = "message")
            output$mPI_table <- DT::renderDT({
                DT::datatable(
                    alpha,
                    options = list(
                        pageLength = 10,
                        dom = 't',
                        autoWidth = TRUE,
                        scrollX = TRUE
                    ),
                    rownames = FALSE
                ) %>%
                    DT::formatRound(columns = names(alpha)[sapply(alpha, is.numeric)], digits = 3)
            })

            output$mpi_plot_grid <- renderPlot(
                alpha_tb %>%
                    dplyr::select(stratum, samples) %>%
                    tidyr::unnest(samples) %>%
                    tidyr::pivot_longer(
                        cols = mean:var, names_to = "type", values_to = "value"
                    ) %>%
                    ggplot2::ggplot(ggplot2::aes(x = value, fill = group)) +
                    ggplot2::geom_density(alpha = 0.7) +
                    ggplot2::facet_wrap(stratum ~ type, scales = "free") +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(
                        title = "Posterior Density Plots for Mean Parameters and Variance Parameters"
                    ) +
                    ggplot2::theme(
                        legend.position = "top"
                    )
            )


            for_res$alpha <- alpha
        }, error = function(e) {
            showNotification(paste("Error in mPI calculation:", e$message), type = "error")
        })
    })

    # Conquer logic triggered by button
    observeEvent(input$conquer_btn, {
        req(for_res$summ, for_res$alpha)

        tryCatch({
            # Compute results and store
            res <- find_posterior(for_res$summ, for_res$alpha)$combined
            for_res$conquer <- res

            # Notification of success
            showNotification("Conquer analysis completed!", type = "message")
            print("Conquer analysis completed.")
        }, error = function(e) {
            showNotification(paste("Error in Conquer step:", e$message), type = "error")
            print(paste("Error:", e$message))
        })
    })

    # Render the results table
    output$conquer_table <- DT::renderDT({
        req(for_res$conquer)
        DT::datatable(
            for_res$conquer,
            options = list(
                pageLength = 20,
                dom = 't',
                autoWidth = TRUE,
                scrollX = TRUE
            ),
            rownames = FALSE
        ) %>%
            DT::formatRound(
                columns = c("mean", "var", "sd", "lower_bound", "upper_bound", "n_eff"),
                digits = 3
            )
    })

    # Render the forest plot
    output$forest_plot <- renderPlot({
        req(for_res$conquer)

        for_res$conquer %>%
            dplyr::mutate(Method = dplyr::if_else(
                borrow, as.character(weight), paste0(as.character(weight), "_nb")
            ) %>% forcats::as_factor()) %>%
            ggplot2::ggplot(ggplot2::aes(x = mean, y = Method)) +
            ggplot2::geom_pointrange(ggplot2::aes(xmin = lower_bound, xmax = upper_bound), color = "darkred", size = 0.8) +
            ggplot2::geom_point(size = 3, color = "blue") +
            ggplot2::geom_text(ggplot2::aes(label = paste0("n_eff: ", round(n_eff, 1))),
                      hjust = -0.3, vjust = 1, size = 5, color = "black", fontface = "italic") +
            ggplot2::facet_wrap(~ variable, scales = "free_x") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
                title = "Forest Plot of Conquer Results with Effective Sample Sizes",
                x = "Estimate (95% CI)",
                y = "Method"
            ) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 12, face = "bold"),
                axis.title = ggplot2::element_text(size = 12),
                axis.text = ggplot2::element_text(size = 10),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.x = ggplot2::element_blank()
            )
    })

    # Download Conquer results
    output$download_conquer <- downloadHandler(
        filename = function() {
            paste("pssBPP_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(for_res$conquer)
            # Save as CSV, not HTML
            write.csv(for_res$conquer, file, row.names = FALSE)
        },
        contentType = "text/csv"
    )

}

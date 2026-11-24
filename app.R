# Shiny app for Australian wine sales forecasting

library(shiny)
library(bslib)
library(bsicons)
library(fpp3)

# initialize constants/settings
wines     <- load_wines()
VARIETALS <- sort(unique(wines$Varietal))
DATE_MIN  <- as.Date(min(wines$Month))
DATE_MAX  <- as.Date(max(wines$Month))
MIN_TRAIN <- 36   # months of training data needed (3 seasonal cycles)
MIN_VALID <- 6    # months held out for validation

# helper function to add n months to a date (used for sliders)
add_months <- function(date, n) as.Date(yearmonth(date) + n)

# helper function to add a tooltip to a label
with_tip <- function(label, text) {
    tagList(label, tooltip(bs_icon("info-circle"), text))
}

# UI section
ui <- page_navbar(
    title  = "Australian Wine Forecaster",
    theme  = bs_theme(bootswatch = "flatly"),
    fillable = FALSE, # let cards grow to fit their plots
    header = useBusyIndicators(), # spinner for loading

    sidebar = sidebar(
        width = 300,
        # varietals selector
        selectizeInput("varietals",
                       with_tip("Varietals", "Pick one or more wine types (each is modeled separately)."),
                       choices = VARIETALS, selected = c("Fortified", "Rose"), multiple = TRUE),
        # date range slider
        sliderInput("dates",
                    with_tip("Date Range", "Only data in this window is plotted, modeled and validated."),
                    min = DATE_MIN, max = DATE_MAX, value = c(DATE_MIN, DATE_MAX),
                    timeFormat = "%b %Y"),
        # training end slider (date)
        sliderInput("train_end",
                    with_tip("Training Ends", "Models learn from data up to this month. Months after are used for the validation set."),
                    min = add_months(DATE_MIN, MIN_TRAIN - 1), max = add_months(DATE_MAX, -MIN_VALID),
                    value = as.Date("1992-12-01"), timeFormat = "%b %Y"),
        # forecast horizon slider (months)
        sliderInput("h",
                    with_tip("Forecast Horizon (months)", "How far past the end of the date range to forecast."),
                    min = 1, max = 36, value = 12),
        # model checkboxes
        checkboxGroupInput("models",
                           with_tip("Models", "TSLM = linear trend + monthly seasonality. ETS = exponential smoothing. ARIMA = autoregressive integrated moving average. ETS and ARIMA forms are chosen automatically."),
                           choices = c("TSLM", "ETS", "ARIMA"),
                           selected = c("TSLM", "ETS", "ARIMA"), inline = TRUE),
        # checkbox for log-transforming sales
        checkboxInput("log_transform",
                      with_tip("Log-Transform Data", "Fit models to log(Sales). This can improve accuracy when seasonal swings scale with the sales level."),
                      value = FALSE),
        # radio buttons for prediction interval
        radioButtons("level",
                     with_tip("Prediction Interval", "Width of the shaded band around each forecast."),
                     choices = c("80%" = 80, "95%" = 95), selected = 95, inline = TRUE),
        # action button to fit models
        actionButton("fit", "Fit Models", icon = icon("play"), class = "btn-primary"),
        helpText("Change the settings, then click Fit Models to update the Model Specifications, Accuracy Metrics, and Forecasts tabs.")
    ),

    # Explore Tab
    nav_panel("Explore",
        card(
            card_header("Sales Over Time"),
            plotOutput("ts_plot", height = "450px"),
            card_footer("Dashed line = End of training. Everything after it is the validation window.")
        )
    ),

    # Model Specs Tab
    nav_panel("Model Specifications",
        card(
            card_header("Model Information"),
            tableOutput("spec_table")
        )
    ),

    # Accuracy Metrics Tab
    nav_panel("Accuracy Metrics",
        layout_columns(
            col_widths = c(9, 3),
            uiOutput("best_boxes"),
            # add a selector for the metric to rank models by
            selectInput("rank_by", "Rank models by", choices = c("RMSE", "MAE", "MAPE"), selected = "RMSE")
        ),
        layout_columns(
            card(
                card_header("Validation Metrics"),
                tableOutput("valid_table"),
            ),
            card(
                card_header("Training Metrics"),
                tableOutput("train_table"),
            )
        ),
        p(class = "small text-muted",
          "RMSE and MAE are in thousands of litres, MAPE is in %.")
    ),

    # Forecasts Tab
    nav_panel("Forecasts",
        card(
            card_header("Validation: training forecasts vs actuals"),
            plotOutput("valid_plot", height = "auto"),
            card_footer("Dashed line = End of training. Black line = Actuals")
        ),
        card(
            card_header(textOutput("future_title", inline = TRUE)),
            plotOutput("future_plot", height = "auto"),
        )
    ),

    # About Tab
    nav_panel("About",
        card(markdown("
        ## About

        **Data Source**: The data comes from data/AustralianWines.csv. It contains monthly sales (thousands of litres) of six varietals, from January 1980 to December 1994.

        **Modeling Choices**: 
        - Imputation was performed on missing values using left right averages.
        - TSLM: a linear regression with a straight-line trend plus a separate effect for each calendar month, `TSLM(Sales ~ trend() + season())`. A simple baseline that assumes the trend and seasonal pattern stay fixed.
        - ETS: exponential smoothing with the error, trend and seasonal components (none, additive, damped or multiplicative) chosen automatically
        - ARIMA: The non-seasonal and seasonal orders (p,d,q)(P,D,Q)[12] are chosen automatically for each varietal.
        - Each model is fitted separately to every selected varietal on the training window, then scored on the validation window with RMSE, MAE and MAPE.
        - Log Transform: models can be fitted to log(Sales) instead of Sales. Forecasts are converted back to the original scale.
        - Future forecasts: models are refitted on all selected data before forecasting h months ahead.

        **Figures**: Figures are generated using the sidebar settings on the left. Change the settings, then click `Fit Models` to update the figures. They use helper functions from modeling.R to plot.
        "))
    )
)

# Server Section
server <- function(input, output, session) {
    # leave room for MIN_TRAIN training months and MIN_VALID validation months.
    observeEvent(input$dates, {
        lo <- add_months(input$dates[1], MIN_TRAIN - 1)
        hi <- add_months(input$dates[2], -MIN_VALID)
        if (lo > hi) return()   # range too short; the check in results() explains
        current <- as.Date(yearmonth(input$train_end))
        updateSliderInput(session, "train_end", min = lo, max = hi,
                          value = min(max(current, lo), hi))
    }, ignoreInit = TRUE)

    # filter data based on the selected varietals and date range
    selected_data <- reactive({
        req(input$varietals)
        filter_wines(wines, input$varietals, input$dates[1], input$dates[2])
    })

    # get model results when the Fit Models button is clicked
    # ignoreNULL = FALSE also runs it once when the app starts
    results <- eventReactive(input$fit, {
        # validate that at least one varietal and one model are selected
        validate(
            need(length(input$varietals) > 0, "Select at least one varietal, then click Fit Models."),
            need(length(input$models) > 0, "Select at least one model, then click Fit Models.")
        )

        data  <- filter_wines(wines, input$varietals, input$dates[1], input$dates[2])
        parts <- split_wines(data, input$train_end)

        n_train <- n_distinct(parts$train$Month)
        n_valid <- n_distinct(parts$valid$Month)
        # validate that the training and validation windows are long enough
        validate(
            need(n_train >= MIN_TRAIN,
                 sprintf("The training window has %d months; at least %d are needed. Move 'Training ends' later or the start date earlier.", n_train, MIN_TRAIN)),
            need(n_valid >= MIN_VALID,
                 sprintf("The validation window has %d months; at least %d are needed. Move 'Training ends' earlier or the end date later.", n_valid, MIN_VALID))
        )

        withProgress(message = "Fitting models", value = 0, {
            incProgress(0.1, detail = "training window")
            fitted_models <- fit_models(parts$train, input$models, input$log_transform)
            fc_valid      <- forecast_validation(fitted_models, parts$valid)
            metrics       <- evaluate_models(fitted_models, fc_valid, data)
            specs         <- spec_table(fitted_models)

            incProgress(0.5, detail = "future forecast")
            fc_future <- forecast_future(data, input$models, input$log_transform, input$h)
            incProgress(0.4)
        })

        list(
            data        = data,
            train_end   = yearmonth(input$train_end),
            end         = yearmonth(input$dates[2]),
            h           = input$h,
            n_varietals = length(input$varietals),
            fc_valid    = fc_valid,
            fc_future   = fc_future,
            metrics     = metrics,
            specs       = specs
        )
    }, ignoreNULL = FALSE)

    # number of varietals in the last fit
    n_fit <- reactive(tryCatch(results()$n_varietals, error = function(e) 1))

    # Explore Tab - render time series plot
    output$ts_plot <- renderPlot({
        selected_data() |>
            autoplot(Sales) +
            geom_vline(xintercept = yearmonth(input$train_end), linetype = "dashed") +
            facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
            labs(x = NULL, y = "Sales (thousands of litres)") +
            theme_minimal() +
            theme(legend.position = "none")
    }, res = 96)

    # Model Specifications Tab - render model specifications table
    output$spec_table <- renderTable({
        results()$specs
    }, digits = 1, striped = TRUE)

    # Accuracy Metrics Tab - render best model boxes and metrics tables
    output$best_boxes <- renderUI({
        best <- best_models(results()$metrics, input$rank_by)
        boxes <- lapply(seq_len(nrow(best)), function(i) {
            value_box(
                title    = paste(best$Varietal[i], "best model"),
                value    = best$Model[i],
                showcase = bs_icon("trophy"),
                theme    = "primary",
                p(sprintf("Validation %s = %.2f", input$rank_by, best[[input$rank_by]][i]))
            )
        })
        layout_column_wrap(width = 1 / min(3, length(boxes)), !!!boxes)
    })

    # one window of the metrics table, sorted best-first within each varietal
    metrics_for <- function(window) {
        results()$metrics |>
            filter(Window == window) |>
            arrange(Varietal, .data[[input$rank_by]]) |>
            select(Varietal, Model, RMSE, MAE, `MAPE (%)` = MAPE)
    }
    output$valid_table <- renderTable(metrics_for("Validation"), digits = 2, striped = TRUE)
    output$train_table <- renderTable(metrics_for("Training"),   digits = 2, striped = TRUE)

    # Forecasts Tab - render validation and future forecasts
    output$valid_plot <- renderPlot({
        r <- results()
        history <- r$data |> filter(Month > r$train_end - 36)
        plot_forecasts(r$fc_valid, history, as.numeric(input$level)) +
            geom_vline(xintercept = r$train_end, linetype = "dashed")
    }, height = function() 120 + 260 * n_fit(), res = 96)

    output$future_title <- renderText({
        r <- results()
        sprintf("Future: %d-month forecast after %s", r$h, format(r$end))
    })

    output$future_plot <- renderPlot({
        r <- results()
        history <- r$data |> filter(Month > r$end - 36)
        plot_forecasts(r$fc_future, history, as.numeric(input$level))
    }, height = function() 120 + 260 * n_fit(), res = 96)
}

shinyApp(ui, server)
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
            card_footer("Dashed line = end of training. Everything after it is the validation window.")
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
            selectInput("rank_by", "Rank models by", choices = c("RMSE", "MAE", "MAPE"), selected = "RMSE")
        ),
        layout_columns(
            card(
                card_header("Validation window (out-of-sample forecasts)"),
                tableOutput("valid_table"),
                card_footer("Forecasts from the training fit compared with held-out actuals. Use these to choose a model.")
            ),
            card(
                card_header("Training window (in-sample fit)"),
                tableOutput("train_table"),
                card_footer("How well each model fits the data it learned from.")
            )
        ),
        p(class = "small text-muted",
          "Lower is better for every metric. RMSE and MAE are in thousands of litres; MAPE is in %.")
    ),

    # ---- Forecasts ----
    nav_panel("Forecasts",
        card(
            card_header("Validation: training-window forecasts vs. what actually happened"),
            plotOutput("valid_plot", height = "auto"),
            card_footer("Dashed line = end of training. Black line = actual sales.")
        ),
        card(
            card_header(textOutput("future_title", inline = TRUE)),
            plotOutput("future_plot", height = "auto"),
            card_footer("Models are refitted on all data in the selected date range before forecasting, so automatically chosen forms can differ slightly from the Model Specifications tab.")
        )
    ),

    # ---- About ----
    nav_panel("About",
        card(markdown("
## About this app

**Purpose.** Help a planner compare and choose forecasting models for monthly Australian wine sales by varietal.

### Data
* `data/AustralianWines.csv`: monthly sales (thousands of litres) of six varietals, January 1980 to December 1994.
* Read with a project-relative path and reshaped into a **tsibble with index = Month and key = Varietal**.
* Two Rose values (Jul and Aug 1994) are missing in the source file. Each is filled with the average of the
  nearest known months before and after it (Jun and Sep 1994), rounded up.

### Workflow
1. **Filter** to the selected varietals and date range.
2. **Split**: training = start of the range through *Training ends*; validation = the months after that.
3. **Fit** TSLM, ETS and ARIMA to each varietal, with ETS and ARIMA specified automatically (by AICc).
   Optionally fit to log(Sales), which can improve accuracy when seasonal swings scale with the sales level.
4. **Score** each model on the training window (in-sample) and the validation window (out-of-sample) with RMSE, MAE and MAPE.
5. **Forecast** *h* months ahead after refitting on all selected data, with prediction intervals.

### Reproducing a figure
Every figure is determined by the sidebar settings. Set them, click **Fit Models**, and open the tab.

### Built with
R, shiny, bslib, fable, feasts, tsibble and ggplot2.

### Acknowledgements
App structure follows the ADS 506 Module 5 labs and *Mastering Shiny* (Wickham).
Code was drafted with help from an LLM assistant (Claude, Anthropic), then reviewed and tested by the author.
"))
    )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

    # Keep "Training ends" inside the selected date range, leaving room for
    # at least MIN_TRAIN training months and MIN_VALID validation months.
    observeEvent(input$dates, {
        lo <- add_months(input$dates[1], MIN_TRAIN - 1)
        hi <- add_months(input$dates[2], -MIN_VALID)
        if (lo > hi) return()   # range too short; the check in results() explains
        current <- as.Date(yearmonth(input$train_end))
        updateSliderInput(session, "train_end", min = lo, max = hi,
                          value = min(max(current, lo), hi))
    }, ignoreInit = TRUE)

    # ---- Filtered data: updates immediately when varietals or dates change ----
    selected_data <- reactive({
        req(input$varietals)
        filter_wines(wines, input$varietals, input$dates[1], input$dates[2])
    })

    # ---- Model results: recalculated only when "Fit models" is clicked ----
    # (ignoreNULL = FALSE also runs it once when the app starts)
    results <- eventReactive(input$fit, {
        validate(
            need(length(input$varietals) > 0, "Select at least one varietal, then click Fit Models."),
            need(length(input$models) > 0, "Select at least one model, then click Fit Models.")
        )

        data  <- filter_wines(wines, input$varietals, input$dates[1], input$dates[2])
        parts <- split_wines(data, input$train_end)

        n_train <- n_distinct(parts$train$Month)
        n_valid <- n_distinct(parts$valid$Month)
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

    # number of varietals in the last fit (used to size the forecast plots)
    n_fit <- reactive(tryCatch(results()$n_varietals, error = function(e) 1))

    # ---- Explore tab ----
    output$ts_plot <- renderPlot({
        selected_data() |>
            autoplot(Sales) +
            geom_vline(xintercept = yearmonth(input$train_end), linetype = "dashed") +
            facet_wrap(~ Varietal, ncol = 1, scales = "free_y") +
            labs(x = NULL, y = "Sales (thousands of litres)") +
            theme_minimal() +
            theme(legend.position = "none")
    }, res = 96)

    # ---- Model specs tab ----
    output$spec_table <- renderTable({
        results()$specs
    }, digits = 1, striped = TRUE)

    # ---- Accuracy tab ----
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

    # ---- Forecasts tab ----
    output$valid_plot <- renderPlot({
        r <- results()
        history <- r$data |> filter(Month > r$train_end - 36)   # last 3 years before the split onward
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
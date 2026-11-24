# TODO: Add description


# STEP 1: Load & Wrangle

# function to set up the data for modeling
load_wines <- function(filepath = file.path("data", "AustralianWines.csv")) {
    # initialize df
    wines_df <- readr::read_csv(filepath, na = c("", "NA", "*"), show_col_types = FALSE, name_repair = "minimal")
    
    # remove whitespace from column names
    names(wines_df) <- trimws(names(wines_df))

    # convert to tsibble
    wines <- wines_df |>
            # convert Month column like "Jun-94" to 1994 June
            mutate(Month = yearmonth(lubridate::my(Month))) |>
            # convert from wide to long, stacking the varietals in a single column
            pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
            mutate(
                # Capitalise first letter of varietals
                Varietal = paste0(toupper(substr(Varietal, 1, 1)), substring(Varietal, 2)),
            ) |>
            as_tsibble(index = Month, key = Varietal)

        fill_missing(wines)
}


fill_missing <- function(wines) {
    wines |>
        # group by Varietal to fill missing values within each group
        group_by_key() |>

        # Copy the sales column into two helper columns for left right imputation
        mutate(left = Sales, right = Sales) |>

        # fill in the left and right known values for each missing value
        tidyr::fill(left, .direction = "down") |>
        tidyr::fill(right, .direction = "up") |>
        
        # Replace the missing values with a rounded up average of the left and right known values.
        mutate(Sales = coalesce(Sales, ceiling((left + right) / 2), left, right)) |>
        ungroup() |>
        select(-left, -right)
}

filter_wines <- function(wines, varietals, start, end) {
    # filter wines dataframe by data range and varietals
    wines |>
        filter(Varietal %in% varietals,
               Month >= yearmonth(start),
               Month <= yearmonth(end))
}

# STEP 2: Split

# split the data into a train and validation set based on the train_end date
split_wines <- function(wines, train_end) {
    # convert train_end to yearmonth and partition the data into train and validation sets
    train_end <- yearmonth(train_end)
    list(
        train = wines |> filter(Month <= train_end),
        valid = wines |> filter(Month > train_end)
    )
}

# STEP 3: Model (per varietal)
# use log flag to determine whether to log transform the data before modeling. 
# this fulfills the requirement of the assignment "Contains at least one more element that improves the forecast accuracy or user experience"
fit_models <- function(train, models = c("TSLM", "ETS", "ARIMA"), log_transform = FALSE) {
    if (log_transform) {
        model_specs <- list(
            TSLM  = TSLM(log(Sales) ~ trend() + season()),
            ETS   = ETS(log(Sales)),
            ARIMA = ARIMA(log(Sales))
        )
    } 
    else {
        model_specs <- list(
            TSLM  = TSLM(Sales ~ trend() + season()),
            ETS   = ETS(Sales),
            ARIMA = ARIMA(Sales)
        )
    }
    # fit the selected models to every varietal
    do.call(model, c(list(train), model_specs[models]))
}

# STEP 4: Forecast

# forecast the validation data for the fitted models
forecast_validation <- function(fitted_models, valid) {
    # forecast the validation data
    forecast(fitted_models, new_data = valid)
}

# refit on selected data and forecast the next h months
forecast_future <- function(data, models, log_transform = FALSE, h = 12) {
    fit_models(data, models, log_transform) |>
        # predict next h months (default to 12 months)
        forecast(h = h)
}

# STEP 5: Evaluate

# evaluate the performance of the fitted models on the training and validation sets
evaluate_models <- function(fitted_models, fc_valid, data) {
    # training data metrics
    train_metrics <- accuracy(fitted_models) |>
        mutate(Window = "Training")
    # validation data metrics
    valid_metrics <- accuracy(fc_valid, data) |>
        mutate(Window = "Validation")
    # combine the metrics into a single dataframe and select RMSE, MAE, and MAPE columns
    bind_rows(train_metrics, valid_metrics) |>
        select(Varietal, Model = .model, Window, RMSE, MAE, MAPE)
}

# find the best model per varietal based on the specified metric (default to RMSE)
best_models <- function(metrics, metric = "RMSE") {
    metrics |>
        # filter for validation window
        filter(Window == "Validation") |>
        # group by varietal
        group_by(Varietal) |>
        # select minimum value of the metric
        slice_min(.data[[metric]], n = 1, with_ties = FALSE) |>
        # remove the grouping by varietal
        ungroup()
}
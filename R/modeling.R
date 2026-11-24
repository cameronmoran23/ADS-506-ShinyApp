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

# split the data into a training window (up to and including train_end)
# and a validation window (everything after train_end)
split_wines <- function(wines, train_end) {
    # convert train_end to yearmonth and partition the data into train and validation sets
    train_end <- yearmonth(train_end)
    list(
        train = wines |> filter(Month <= train_end),
        valid = wines |> filter(Month > train_end)
    )
}

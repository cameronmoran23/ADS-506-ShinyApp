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
# Australian Wine Forecaster

This repository contains the code for a Shiny app. It compares and selects forecasting models for monthly Australian wine sales by varietal. It fits TSLM, ETS and ARIMA models to each selected varietal and scores them on a held-out validation window. It also forecasts ahead with prediction intervals (80% or 95%).

Built for ADS 506 (Applied Time Series Analysis) - Module 5

**Live App URL:** https://cameronmoran23.shinyapps.io/aus-wine-forecaster/ 

## Organization

- README.md - this file
- .gitignore - ignores various R filetypes from git commands
- app.R - R file that contains the shiny app logic
- R/modeling.R - contains the backend logic for the shiny app
- data/AustralianWines.csv - contains the dataset
- Week5_Moran_Cameron.qmd - Data Story and app features qmd file

## App Features

- **Explore Tab:** allows for varietal, date range, forecast horizon, and model selection. Also shows the Sales Over Time plots for the varietals selected.
- **Model Specifications Tab:** provides model information including key parameters, model name, and AICc. 
- **Accuracy Metrics Tab:** shows the training and validation metrics for the models trained on the selected varietals. Also highlights the top performing models by the selected metric.
- **Forecasts Tab:** shows the forecasts compared to the actuals for the trained models as well as the forecasts outside the dataset range.
- **About Tab:** provides various information about the app pertaining to the data source, modeling choices, and figure recreation steps.

## Run Locally

Requires R (≥ 4.1). From the project folder:

```r
install.packages(c("shiny", "bslib", "bsicons", "fpp3", "readr", "urca"))
shiny::runApp()
```

All file paths are relative to the project folder, so the app must be run from there.

## Acknowledgements

The code in this repository was developed with the help of Claude Code by Anthropic.
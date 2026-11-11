# Bank Marketing Data Analysis

An R project demonstrating data cleaning, transformation, visualization, descriptive analysis, and feature selection on a bank marketing campaign dataset.

## Project Contents

- `bank_marketing_analysis.R` contains the end-to-end analysis.
- `bank-full.csv` is the semicolon-delimited input dataset (45,211 observations and 17 columns).
- The other `bank_*.csv` files are saved outputs from individual analysis stages.
- `Project.Rproj` opens the project in RStudio.

## Requirements

- R
- R packages: `dplyr`, `ggplot2`, and `corrplot`

Install the required packages from an R console:

```r
install.packages(c("dplyr", "ggplot2", "corrplot"))
```

## Run the Analysis

1. Open `Project.Rproj` in RStudio, or set the working directory to the project folder.
2. Run `bank_marketing_analysis.R` in an interactive R session.
3. When prompted, select `bank-full.csv`.

The script uses `file.choose()` to select the input, so it requires an interactive session. Generated CSV files are written to the current working directory and overwrite existing files with the same names.

## Workflow

The script selects the analysis features, examines missing values, compares row deletion with median/mode imputation, checks duplicate and invalid records, and applies normalization, standardization, categorical encoding, and feature engineering. It also creates plots and correlation analysis, detects outliers using the IQR method, splits the data into training and test sets, and exports a selected-feature dataset. The split uses an 80/20 ratio and a fixed random seed (`123`).

Several cases are deliberately introduced in the script to demonstrate data-quality handling, including missing values, a numeric outlier, inconsistent category values, a duplicate row, and an invalid category. The outputs therefore include pedagogical changes and should not be treated as an unmodified representation of the source data.

## Generated Files

| File | Contents |
| --- | --- |
| `bank_selected.csv` | Selected source columns before the cleaning steps |
| `bank_discard_data.csv` | Rows remaining after rows with missing values are removed |
| `bank_replaced_data.csv` | Data after missing values are imputed with medians and modes |
| `bank_noise_cleaned.csv` | Data after the scripted noise and duplicate checks |
| `bank_invalid_cleaned.csv` | Data after the scripted invalid-category check and correction |
| `bank_transformed_data.csv` | Normalized, standardized, encoded, and engineered data |
| `bank_outlier_checked.csv` | Data snapshot used for IQR-based outlier checks |
| `bank_descriptive_statistics.csv` | Data snapshot used for descriptive statistics and plots; the script does not export a summary table |
| `bank_train_data.csv`, `bank_test_data.csv` | 80/20 training and test split |
| `bank_selected_features.csv` | Selected predictors and the subscription outcome |

## License

The project code is provided under the MIT License; see [LICENSE](LICENSE). The dataset may be subject to separate terms.

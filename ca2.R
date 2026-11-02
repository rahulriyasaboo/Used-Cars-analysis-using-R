# CarDekho Used Car Price Estimation
# Run the complete script in RStudio using Source.
# Required package: rpart
# If needed, run once in the Console: install.packages("rpart")

set.seed(42)

# 1. Load data ---------------------------------------------------------------

data_path <- file.path(
  "/Users/rahulriyasaboo/Documents/Codex/2026-10-27/develop-a-complete-data-science-machine/outputs/cardekho_used_car_price_estimation/data/cardekho_dataset.csv"
)

if (!file.exists(data_path)) {
  stop(paste("Dataset not found:", data_path))
}

# Put all generated files in the project's results folder.
project_dir <- dirname(dirname(data_path))
setwd(project_dir)

results_dir <- file.path(project_dir, "results")
dir.create(results_dir, showWarnings = FALSE)

if (!requireNamespace("rpart", quietly = TRUE)) {
  stop('Package "rpart" is required. Install it in the Console with install.packages("rpart")')
}

raw <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)

# Remove the exported row-number column, if present.
if (ncol(raw) > 0 &&
    (names(raw)[1] == "" || grepl("^Unnamed", names(raw)[1]))) {
  raw <- raw[, -1, drop = FALSE]
}

# Confirm that the dataset contains all required fields.
required <- c(
  "car_name", "brand", "model", "vehicle_age", "km_driven",
  "seller_type", "fuel_type", "transmission_type", "mileage",
  "engine", "max_power", "seats", "selling_price"
)

missing_columns <- setdiff(required, names(raw))

if (length(missing_columns) > 0) {
  stop(paste(
    "These required columns are missing:",
    paste(missing_columns, collapse = ", ")
  ))
}

# Convert numeric fields to numbers.
numeric_cols <- c(
  "vehicle_age", "km_driven", "mileage", "engine",
  "max_power", "seats", "selling_price"
)

for (col in numeric_cols) {
  raw[[col]] <- suppressWarnings(as.numeric(raw[[col]]))
}

# 2. Clean the data ----------------------------------------------------------

rows_initial <- nrow(raw)

# Remove exact duplicate rows.
duplicate_count <- sum(duplicated(raw))
dat <- raw[!duplicated(raw), , drop = FALSE]

# Remove records with impossible price, age, or odometer values.
valid <- is.finite(dat$selling_price) &
  dat$selling_price > 0 &
  is.finite(dat$vehicle_age) &
  dat$vehicle_age >= 0 &
  is.finite(dat$km_driven) &
  dat$km_driven >= 0

removed_invalid <- sum(!valid)
dat <- dat[valid, , drop = FALSE]

# Function to find the most frequent category.
mode_value <- function(x) {
  x <- x[!is.na(x) & trimws(x) != ""]
  
  if (length(x) == 0) {
    return("Unknown")
  }
  
  names(sort(table(x), decreasing = TRUE))[1]
}

# Fill missing numeric values with the median.
for (col in numeric_cols) {
  if (anyNA(dat[[col]])) {
    median_value <- median(dat[[col]], na.rm = TRUE)
    
    if (is.finite(median_value)) {
      dat[[col]][is.na(dat[[col]])] <- median_value
    }
  }
}

# Fill missing categorical values with the most frequent category.
cat_cols <- c(
  "car_name", "brand", "model",
  "seller_type", "fuel_type", "transmission_type"
)

for (col in cat_cols) {
  dat[[col]] <- trimws(as.character(dat[[col]]))
  missing_category <- is.na(dat[[col]]) | dat[[col]] == ""
  
  if (any(missing_category)) {
    dat[[col]][missing_category] <- mode_value(dat[[col]])
  }
}

# Save cleaning summary.
cleaning_summary <- data.frame(
  item = c(
    "rows_read",
    "duplicates_removed",
    "invalid_rows_removed",
    "rows_modelled",
    "columns_after_index_drop"
  ),
  value = c(
    rows_initial,
    duplicate_count,
    removed_invalid,
    nrow(dat),
    ncol(dat)
  )
)

write.csv(
  cleaning_summary,
  file.path(results_dir, "cleaning_summary.csv"),
  row.names = FALSE
)

# Save missing-value counts after cleaning.
missing_summary <- data.frame(
  feature = names(dat),
  missing_after_cleaning = vapply(
    dat,
    function(x) sum(is.na(x)),
    integer(1)
  )
)

write.csv(
  missing_summary,
  file.path(results_dir, "missing_values.csv"),
  row.names = FALSE
)

# Save summaries of the numeric fields.
numeric_summary_cols <- c(
  "vehicle_age", "km_driven", "mileage", "engine",
  "max_power", "seats", "selling_price"
)

numeric_summary <- do.call(
  rbind,
  lapply(numeric_summary_cols, function(col) {
    x <- dat[[col]]
    
    data.frame(
      feature = col,
      minimum = min(x, na.rm = TRUE),
      first_quartile = unname(quantile(x, 0.25, na.rm = TRUE)),
      median = median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      third_quartile = unname(quantile(x, 0.75, na.rm = TRUE)),
      maximum = max(x, na.rm = TRUE)
    )
  })
)

write.csv(
  numeric_summary,
  file.path(results_dir, "numeric_summary.csv"),
  row.names = FALSE
)

# 3. Identify outliers -------------------------------------------------------

# IQR outlier rule: values outside Q1 - 1.5*IQR or Q3 + 1.5*IQR.
# Outliers are counted and reported, not automatically removed.
outlier_count <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr_value <- q[2] - q[1]
  
  sum(
    x < q[1] - 1.5 * iqr_value |
      x > q[2] + 1.5 * iqr_value,
    na.rm = TRUE
  )
}

outlier_cols <- c(
  "vehicle_age", "km_driven", "mileage", "engine",
  "max_power", "seats", "selling_price"
)

outlier_summary <- data.frame(
  feature = outlier_cols,
  iqr_outlier_count = vapply(
    dat[outlier_cols],
    outlier_count,
    integer(1)
  )
)

write.csv(
  outlier_summary,
  file.path(results_dir, "outlier_summary.csv"),
  row.names = FALSE
)

# 4. Plot helper -------------------------------------------------------------

# In RStudio, draw each plot in the Plots pane and also save a PNG copy.
save_and_show <- function(filename, draw_plot) {
  if (interactive()) {
    draw_plot()
  }
  
  png(filename, width = 1000, height = 700)
  draw_plot()
  dev.off()
}

# 5. Exploratory data analysis plots ----------------------------------------

save_and_show(
  file.path(results_dir, "price_distribution.png"),
  function() {
    hist(
      dat$selling_price,
      breaks = 50,
      col = "steelblue",
      border = "white",
      main = "Used Car Selling Price",
      xlab = "Selling price (rupees)"
    )
  }
)

save_and_show(
  file.path(results_dir, "price_by_vehicle_age.png"),
  function() {
    plot(
      dat$vehicle_age,
      dat$selling_price,
      pch = 16,
      cex = 0.45,
      col = rgb(0.1, 0.3, 0.6, 0.3),
      main = "Selling Price vs Vehicle Age",
      xlab = "Vehicle age (years)",
      ylab = "Selling price (rupees)"
    )
  }
)

save_and_show(
  file.path(results_dir, "price_by_fuel.png"),
  function() {
    boxplot(
      selling_price ~ fuel_type,
      data = dat,
      las = 2,
      col = "tan",
      main = "Selling Price by Fuel Type",
      xlab = "Fuel type",
      ylab = "Selling price (rupees)"
    )
  }
)

# Correlation matrix for numeric features.
cor_cols <- c(
  "vehicle_age", "km_driven", "mileage",
  "engine", "max_power", "seats", "selling_price"
)

corr <- cor(dat[, cor_cols], use = "complete.obs")

save_and_show(
  file.path(results_dir, "correlation_matrix.png"),
  function() {
    par(mar = c(8, 8, 4, 2))
    
    image(
      seq_len(ncol(corr)),
      seq_len(nrow(corr)),
      t(corr[nrow(corr):1, ]),
      axes = FALSE,
      col = colorRampPalette(
        c("navy", "white", "firebrick")
      )(100),
      zlim = c(-1, 1),
      main = "Numeric Feature Correlation"
    )
    
    axis(
      1,
      at = seq_len(ncol(corr)),
      labels = colnames(corr),
      las = 2,
      cex.axis = 0.8
    )
    
    axis(
      2,
      at = seq_len(nrow(corr)),
      labels = rev(rownames(corr)),
      las = 2,
      cex.axis = 0.8
    )
  }
)

write.csv(
  corr,
  file.path(results_dir, "correlation_matrix.csv")
)

# 6. Statistical analysis ----------------------------------------------------

# Summarize prices separately for each fuel type.
by_fuel <- aggregate(
  selling_price ~ fuel_type,
  data = dat,
  FUN = function(x) {
    c(
      count = length(x),
      mean = mean(x),
      median = median(x),
      standard_deviation = sd(x)
    )
  }
)

write.csv(
  by_fuel,
  file.path(results_dir, "price_summary_by_fuel.csv"),
  row.names = FALSE
)

# Kruskal-Wallis test checks whether prices differ across fuel groups.
kw <- kruskal.test(selling_price ~ fuel_type, data = dat)

statistical_test <- data.frame(
  test = "Kruskal-Wallis selling price by fuel type",
  statistic = unname(kw$statistic),
  degrees_of_freedom = unname(kw$parameter),
  p_value = kw$p.value
)

write.csv(
  statistical_test,
  file.path(results_dir, "statistical_test.csv"),
  row.names = FALSE
)

# 7. Prepare data for machine learning --------------------------------------

# Features included in the models.
# car_name and model are excluded because they have many unique categories.
features <- c(
  "vehicle_age", "km_driven", "seller_type", "fuel_type",
  "transmission_type", "mileage", "engine", "max_power",
  "seats", "brand"
)

model_data <- dat[, c(features, "selling_price"), drop = FALSE]

# Use log(price) as the model target to reduce price skew.
model_data$log_price <- log(model_data$selling_price)
model_data$selling_price <- NULL

# Tell R which columns contain categories.
factor_cols <- c(
  "seller_type", "fuel_type", "transmission_type", "brand"
)

for (col in factor_cols) {
  model_data[[col]] <- factor(model_data[[col]])
}

# 8. Split into training and testing data -----------------------------------

n <- nrow(model_data)
train_id <- sample(
  seq_len(n),
  size = floor(0.8 * n)
)

train <- model_data[train_id, , drop = FALSE]
test <- model_data[-train_id, , drop = FALSE]

# 9. Train two models --------------------------------------------------------

# Model 1: linear regression.
lm_fit <- lm(log_price ~ ., data = train)

# Model 2: regression tree.
tree_fit <- rpart::rpart(
  log_price ~ .,
  data = train,
  method = "anova",
  control = rpart::rpart.control(
    cp = 0.001,
    minsplit = 30,
    maxdepth = 8
  )
)

# Convert predicted log prices back into rupees.
predict_price <- function(model, newdata) {
  predicted_log_price <- predict(model, newdata = newdata)
  pmax(0, exp(predicted_log_price))
}

actual <- exp(test$log_price)

# 10. Evaluate the models ----------------------------------------------------

metric_row <- function(model_name, predicted_price) {
  data.frame(
    model = model_name,
    RMSE = sqrt(mean((actual - predicted_price)^2)),
    MAE = mean(abs(actual - predicted_price)),
    R_squared = 1 -
      sum((actual - predicted_price)^2) /
      sum((actual - mean(actual))^2)
  )
}

lm_predictions <- predict_price(lm_fit, test)
tree_predictions <- predict_price(tree_fit, test)

model_results <- rbind(
  metric_row(
    "Linear regression (log price)",
    lm_predictions
  ),
  metric_row(
    "Regression tree (log price)",
    tree_predictions
  )
)

write.csv(
  model_results,
  file.path(results_dir, "model_comparison.csv"),
  row.names = FALSE
)

# Select the model with the lower RMSE.
best_name <- model_results$model[
  which.min(model_results$RMSE)
]

if (best_name == "Linear regression (log price)") {
  best_fit <- lm_fit
} else {
  best_fit <- tree_fit
}

saveRDS(
  best_fit,
  file.path(results_dir, "final_model.rds")
)

# Save actual and predicted prices for the test data.
best_predictions <- predict_price(best_fit, test)

test_predictions <- data.frame(
  actual_price = actual,
  predicted_price = best_predictions
)

write.csv(
  test_predictions,
  file.path(results_dir, "test_predictions.csv"),
  row.names = FALSE
)

# 11. Plot actual versus predicted prices -----------------------------------

pred <- best_predictions

save_and_show(
  file.path(results_dir, "actual_vs_predicted.png"),
  function() {
    plot(
      actual,
      pred,
      pch = 16,
      cex = 0.55,
      col = rgb(0.1, 0.3, 0.6, 0.35),
      xlab = "Actual selling price (rupees)",
      ylab = "Predicted selling price (rupees)",
      main = paste("Actual vs Predicted:", best_name)
    )
    
    # The red line represents perfect predictions.
    abline(0, 1, col = "red", lwd = 2)
  }
)

# 12. Save model and test details -------------------------------------------

capture.output(
  summary(lm_fit),
  file = file.path(results_dir, "linear_model_summary.txt")
)

capture.output(
  kw,
  file = file.path(results_dir, "kruskal_wallis.txt")
)

# 13. Print results in the Console ------------------------------------------

cat("Analysis complete.\n")
cat("Rows modeled:", nrow(dat), "\n")
cat("Training rows:", nrow(train), "\n")
cat("Testing rows:", nrow(test), "\n")
cat("Selected model:", best_name, "\n\n")

print(model_results)

cat(
  "\nKruskal-Wallis p-value:",
  format.pval(kw$p.value),
  "\n"
)

cat(
  "\nPlots and result files are saved here:\n",
  results_dir,
  "\n"
)
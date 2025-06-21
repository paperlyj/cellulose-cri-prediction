# --------------------------------------------
# MLP Model for Cellulose Crystallinity Prediction
# Author: [Yong Ju Lee]
# Date: [2025-06-21]
# --------------------------------------------

library(openxlsx)
library(dplyr)
library(prospectr)   # for Savitzky-Golay filtering
library(ANN2)        # for MLP modeling
library(caret)       # for cross-validation folds
library(Metrics)     # for RMSE calculation

# 1. Load data and preprocessing
data <- read.xlsx("IR Summary.xlsx", sheet = "Selected_3")
x_raw <- as.matrix(data[, 4:ncol(data)])
y <- data$CrI

# L2 normalization function
l2_normalize <- function(x) {
  norms <- sqrt(rowSums(x^2))
  x / pmax(norms, 1e-8)
}

# Apply second derivative (Savitzky-Golay) + L2 normalization
apply_diff2_l2 <- function(df) {
  sg <- t(apply(df, 1, function(z) savitzkyGolay(z, m = 2, p = 3, w = 21)))
  l2_normalize(sg)
}
x_sg2 <- apply_diff2_l2(x_raw)

# 2. Random seeds for reproducibility
set.seed(42)
seeds <- sample(100:9999, 50)

# Storage for results and hyperparameters
results_raw <- matrix(NA, 50, 4, dimnames = list(NULL, c("R2_train","RMSE_train","R2_test","RMSE_test")))
results_sg2 <- results_raw
hyp_raw <- vector("list", 50)
hyp_sg2 <- vector("list", 50)

# 3. Hyperparameter grid
tune_grid <- expand.grid(
  hidden = list(c(8), c(16), c(32)),
  learn.rate = c(1e-4, 1e-3, 1e-2, 1e-1),
  optim.type = c("sgd", "adam"),
  stringsAsFactors = FALSE
)

# 4. Function for 3-fold CV hyperparameter tuning
tune_mlp <- function(X, y) {
  cv_r2 <- numeric(nrow(tune_grid))
  folds <- createFolds(y, k = 3)
  
  for (i in seq_len(nrow(tune_grid))) {
    h <- tune_grid$hidden[[i]]
    lr <- tune_grid$learn.rate[i]
    opt <- tune_grid$optim.type[i]
    
    r2_vals <- sapply(folds, function(fold) {
      Xtr <- X[-fold, ]; ytr <- y[-fold]
      Xval <- X[fold, ]; yval <- y[fold]
      
      model <- neuralnetwork(
        X = Xtr, y = ytr,
        hidden.layers = h,
        regression = TRUE,
        standardize = TRUE,
        loss.type = "squared",
        learn.rates = lr,
        optim.type = opt,
        n.epochs = 500,
        verbose = FALSE
      )
      preds <- predict(model, Xval)$predictions
      1 - sum((yval - preds)^2) / sum((yval - mean(yval))^2)
    })
    cv_r2[i] <- mean(r2_vals)
  }
  best_idx <- which.max(cv_r2)
  tune_grid[best_idx, ]
}

# 5. Train and evaluate over 50 random splits
for (i in seq_along(seeds)) {
  set.seed(seeds[i])
  idx <- sample(nrow(x_raw))
  n_train <- floor(0.8 * length(idx))
  train_idx <- idx[1:n_train]
  test_idx <- idx[(n_train + 1):length(idx)]
  
  X_train_raw <- l2_normalize(x_raw[train_idx, ])
  X_test_raw <- l2_normalize(x_raw[test_idx, ])
  X_train_sg2 <- x_sg2[train_idx, ]
  X_test_sg2 <- x_sg2[test_idx, ]
  y_train <- y[train_idx]
  y_test <- y[test_idx]
  
  # Raw + L2
  best_params_raw <- tune_mlp(X_train_raw, y_train)
  hyp_raw[[i]] <- best_params_raw
  model_raw <- neuralnetwork(
    X = X_train_raw, y = y_train,
    hidden.layers = best_params_raw$hidden[[1]],
    regression = TRUE,
    standardize = TRUE,
    loss.type = "squared",
    learn.rates = best_params_raw$learn.rate,
    optim.type = best_params_raw$optim.type,
    n.epochs = 500,
    verbose = FALSE
  )
  preds_train_raw <- predict(model_raw, X_train_raw)$predictions
  preds_test_raw <- predict(model_raw, X_test_raw)$predictions
  
  results_raw[i, ] <- c(
    1 - sum((y_train - preds_train_raw)^2) / sum((y_train - mean(y_train))^2),
    Metrics::rmse(y_train, preds_train_raw),
    1 - sum((y_test - preds_test_raw)^2) / sum((y_test - mean(y_test))^2),
    Metrics::rmse(y_test, preds_test_raw)
  )
  
  # SG2 + L2
  best_params_sg2 <- tune_mlp(X_train_sg2, y_train)
  hyp_sg2[[i]] <- best_params_sg2
  model_sg2 <- neuralnetwork(
    X = X_train_sg2, y = y_train,
    hidden.layers = best_params_sg2$hidden[[1]],
    regression = TRUE,
    standardize = TRUE,
    loss.type = "squared",
    learn.rates = best_params_sg2$learn.rate,
    optim.type = best_params_sg2$optim.type,
    n.epochs = 500,
    verbose = FALSE
  )
  preds_train_sg2 <- predict(model_sg2, X_train_sg2)$predictions
  preds_test_sg2 <- predict(model_sg2, X_test_sg2)$predictions
  
  results_sg2[i, ] <- c(
    1 - sum((y_train - preds_train_sg2)^2) / sum((y_train - mean(y_train))^2),
    Metrics::rmse(y_train, preds_train_sg2),
    1 - sum((y_test - preds_test_sg2)^2) / sum((y_test - mean(y_test))^2),
    Metrics::rmse(y_test, preds_test_sg2)
  )
}

# 6. Summarize results
summarize_results <- function(mat, label) {
  data.frame(
    Model = label,
    Set = rep(c("Train", "Test"), each = 2),
    Metric = rep(c("R2", "RMSE"), 2),
    Mean = c(colMeans(mat[, 1:2]), colMeans(mat[, 3:4])),
    SD = c(apply(mat[, 1:2], 2, sd), apply(mat[, 3:4], 2, sd))
  )
}

performance <- rbind(
  summarize_results(results_raw, "Raw + L2"),
  summarize_results(results_sg2, "SG2 + L2")
)
print(performance)

# 7. Hyperparameter frequency tables
extract_hyp <- function(lst) {
  do.call(rbind, lapply(lst, function(x)
    data.frame(
      hidden = paste(x$hidden[[1]], collapse = "-"),
      learn_rate = x$learn.rate,
      optimizer = x$optim.type
    )
  ))
}

cat("\nRaw + L2 Hyperparameter Frequencies:\n")
print(table(extract_hyp(hyp_raw)))
cat("\nSG2 + L2 Hyperparameter Frequencies:\n")
print(table(extract_hyp(hyp_sg2)))

# 8. Retrain final model on full dataset with best parameters (based on test R2 of SG2 + L2)
best_index <- which.max(results_sg2[, "R2_test"])
best_params <- hyp_sg2[[best_index]]

x_all_sg2 <- apply_diff2_l2(as.matrix(data[, 4:ncol(data)]))
y_all <- data$CrI

final_mlp <- neuralnetwork(
  X = x_all_sg2,
  y = y_all,
  hidden.layers = best_params$hidden[[1]],
  regression = TRUE,
  standardize = TRUE,
  loss.type = "squared",
  learn.rates = best_params$learn.rate,
  optim.type = best_params$optim.type,
  n.epochs = 500,
  verbose = FALSE
)

# 9. Predict new samples example
new_data <- read.xlsx("IR summary.xlsx", sheet = "Application_BM")
x_new_raw <- as.matrix(new_data[, 2:ncol(new_data)])
x_new_sg2 <- apply_diff2_l2(x_new_raw)
predicted_cri <- predict(final_mlp, x_new_sg2)$predictions

prediction_results <- data.frame(
  Sample = new_data$Sample,
  Predicted_CrI = round(predicted_cri, 2)
)
print(prediction_results)

# 10. Y-scrambling test for model robustness
results_scrambled <- matrix(NA, 50, 4,
                            dimnames = list(NULL, c("R2_tr", "RM_tr", "R2_te", "RM_te")))

for (i in seq_along(seeds)) {
  set.seed(seeds[i])
  idx <- sample(nrow(x_sg2))
  n_tr <- floor(0.8 * length(idx))
  train_idx <- idx[1:n_tr]
  test_idx <- idx[(n_tr + 1):length(idx)]
  
  y_scrambled <- sample(y)
  
  

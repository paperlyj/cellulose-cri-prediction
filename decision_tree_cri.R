# ----------------------------------------------------------
# Decision Tree Regression for Cellulose Crystallinity Index
# using IR spectra (Raw + SG2 derivative preprocessing)
# Author: [Your Name]
# Date: [YYYY-MM-DD]
# ----------------------------------------------------------

# 📦 Load packages
library(openxlsx)
library(dplyr)
library(rpart)
library(caret)
library(Metrics)
library(prospectr)

# ----------------------------------------------------------
# 1. Load Data
# ----------------------------------------------------------
data  <- read.xlsx("IR Summary.xlsx", sheet = "Selected_3")
x_raw <- as.matrix(data[, 4:ncol(data)])   # Spectral features
y     <- data$CrI                         # Crystallinity index

# ----------------------------------------------------------
# 2. Preprocessing Functions
# ----------------------------------------------------------
l2_normalize <- function(mat) {
  norms <- sqrt(rowSums(mat^2))
  mat / pmax(norms, 1e-8)
}

apply_sg2 <- function(mat) {
  sg <- t(apply(mat, 1, function(z)
    savitzkyGolay(z, m = 2, p = 3, w = 21)))
  l2_normalize(sg)
}

# ----------------------------------------------------------
# 3. Initialize
# ----------------------------------------------------------
set.seed(42)
seeds      <- sample(1000:9999, 50)
data_types <- c("raw", "sg2")  # preprocessing types

dt_perf   <- list()            # to store performance
chosen_cp <- list()            # to store optimal cp per run

for (d in data_types) {
  dt_perf[[d]]   <- matrix(NA, 50, 4,
                           dimnames = list(NULL, c("R2_tr", "RM_tr", "R2_te", "RM_te")))
  chosen_cp[[d]] <- numeric(50)
}

# ----------------------------------------------------------
# 4. Repeated Training (50 repetitions)
# ----------------------------------------------------------
for (i in seq_along(seeds)) {
  set.seed(seeds[i])
  idx     <- sample(nrow(x_raw))
  n_train <- floor(0.8 * length(idx))
  train_i <- idx[1:n_train]
  test_i  <- idx[(n_train+1):length(idx)]
  
  for (d in data_types) {
    if (d == "raw") {
      Xtr <- l2_normalize(x_raw[train_i, ])
      Xte <- l2_normalize(x_raw[test_i, ])
    } else {
      Xtr <- apply_sg2(x_raw[train_i, ])
      Xte <- apply_sg2(x_raw[test_i, ])
    }
    
    df_tr <- data.frame(CrI = y[train_i], Xtr)
    df_te <- data.frame(CrI = y[test_i],  Xte)
    
    # 3-fold CV for optimal cp
    ctrl <- trainControl(method = "cv", number = 3)
    grid <- expand.grid(cp = c(0.001, 0.005, 0.01, 0.05))
    fit  <- train(
      CrI ~ ., data = df_tr,
      method    = "rpart",
      trControl = ctrl,
      tuneGrid  = grid
    )
    best_cp <- fit$bestTune$cp
    chosen_cp[[d]][i] <- best_cp
    
    # Final model
    final_mod <- rpart(
      CrI ~ ., data = df_tr,
      control = rpart.control(cp = best_cp, minsplit = 20)
    )
    
    # Prediction & Evaluation
    p_tr <- predict(final_mod, df_tr)
    p_te <- predict(final_mod, df_te)
    
    dt_perf[[d]][i, ] <- c(
      1 - sum((df_tr$CrI - p_tr)^2) / sum((df_tr$CrI - mean(df_tr$CrI))^2),
      rmse(df_tr$CrI, p_tr),
      1 - sum((df_te$CrI - p_te)^2) / sum((df_te$CrI - mean(df_te$CrI))^2),
      rmse(df_te$CrI, p_te)
    )
  }
}

# ----------------------------------------------------------
# 5. Performance Summary
# ----------------------------------------------------------
summarize_perf <- function(mat, label) {
  data.frame(
    Model  = label,
    Set    = rep(c("Train", "Test"), each = 2),
    Metric = rep(c("R2", "RMSE"), 2),
    Mean   = c(colMeans(mat[, 1:2]), colMeans(mat[, 3:4])),
    SD     = c(apply(mat[, 1:2], 2, sd), apply(mat[, 3:4], 2, sd))
  )
}

perf_raw <- summarize_perf(dt_perf$raw, "DT Raw + L2")
perf_sg2 <- summarize_perf(dt_perf$sg2, "DT SG2 + L2")
perf_df  <- rbind(perf_raw, perf_sg2)
print(perf_df)

# ----------------------------------------------------------
# 6. Optimal cp frequency
# ----------------------------------------------------------
cat("\n-- Best cp frequency (Raw + L2) --\n")
print(sort(table(chosen_cp$raw), decreasing = TRUE))

cat("\n-- Best cp frequency (SG2 + L2) --\n")
print(sort(table(chosen_cp$sg2), decreasing = TRUE))

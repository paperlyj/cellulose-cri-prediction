# --------------------------------------------
# Random Forest Modeling for Cellulose Crystallinity Prediction
# Author: [Yong Ju Lee]
# Date: [2025-06-21]
# --------------------------------------------

# 1. Set working directory (adjust path as needed)
# setwd("C:/Users/paper/OneDrive/Desktop/Research/Thesis/Part1/Revision/Data")

# 2. Load required libraries
library(openxlsx)      # Excel data import/export
library(dplyr)         # Data manipulation
library(randomForest)  # Random Forest modeling
library(Metrics)       # RMSE and other metrics
library(prospectr)     # Spectral preprocessing (Savitzky-Golay)

# 3. Load dataset
data <- read.xlsx("IR Summary.xlsx", sheet = "Selected_3")

# 4. Define helper functions

# L2 normalization function to scale each spectrum
l2_normalize <- function(x) {
  norm_vec <- sqrt(rowSums(x^2))
  norm_vec[norm_vec == 0] <- 1e-8  # Avoid division by zero
  x / norm_vec
}

# Apply Savitzky-Golay 2nd derivative and L2 normalization
apply_diff2_l2 <- function(df) {
  sg_mat <- apply(df, 1, function(x) savitzkyGolay(x, m = 2, p = 3, w = 21))
  sg_mat <- t(sg_mat)
  l2_normalize(sg_mat)
}

# 5. Prepare raw and preprocessed spectral data
x_raw <- as.matrix(data[, 4:ncol(data)])
x_sg2 <- apply_diff2_l2(x_raw)
y <- data$CrI

# 6. Initialize lists to store results and models
rf_results <- list()
rf_models <- list()
mtry_types <- c("sqrt", "log2", "div3")
data_types <- c("raw", "sg2")

for (d in data_types) {
  for (m in mtry_types) {
    key <- paste(d, m, sep = "_")
    rf_results[[key]] <- list(r2_train = c(), rmse_train = c(),
                              r2_test = c(), rmse_test = c(),
                              oob_error = c())
    rf_models[[key]] <- list()
  }
}

# 7. Set random seeds for reproducibility and repeated experiments
set.seed(42)
seeds <- sample(123:1234, 50, replace = FALSE)

# 8. Loop over 50 random splits and train/test Random Forest models
for (i in 1:50) {
  set.seed(seeds[i])
  
  # Shuffle data indices and split 80% train / 20% test
  idx <- sample(nrow(data))
  split_idx <- floor(0.8 * length(y))
  train_idx <- 1:split_idx
  test_idx <- (split_idx + 1):length(y)
  
  x_raw_shuffled <- x_raw[idx, ]
  x_sg2_shuffled <- x_sg2[idx, ]
  y_shuffled <- y[idx]
  
  for (d in data_types) {
    x_input <- if (d == "raw") x_raw_shuffled else x_sg2_shuffled
    
    x_train <- x_input[train_idx, ]
    x_test <- x_input[test_idx, ]
    y_train <- y_shuffled[train_idx]
    y_test <- y_shuffled[test_idx]
    p <- ncol(x_input)
    
    for (mtry_type in mtry_types) {
      # Determine mtry parameter based on type
      mtry_val <- switch(mtry_type,
                         sqrt = floor(sqrt(p)),
                         log2 = floor(log2(p)),
                         div3 = floor(p / 3))
      
      # Train Random Forest model with 11 trees
      model <- randomForest(x = x_train, y = y_train,
                            ntree = 11, mtry = mtry_val,
                            importance = TRUE, keep.inbag = TRUE)
      
      # Generate predictions
      pred_train <- predict(model, x_train)
      pred_test <- predict(model, x_test)
      
      key <- paste(d, mtry_type, sep = "_")
      
      # Save performance metrics
      rf_results[[key]]$r2_train <- c(rf_results[[key]]$r2_train,
                                      1 - sum((y_train - pred_train)^2) / sum((y_train - mean(y_train))^2))
      rf_results[[key]]$rmse_train <- c(rf_results[[key]]$rmse_train,
                                        Metrics::rmse(y_train, pred_train))
      rf_results[[key]]$r2_test <- c(rf_results[[key]]$r2_test,
                                     1 - sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2))
      rf_results[[key]]$rmse_test <- c(rf_results[[key]]$rmse_test,
                                       Metrics::rmse(y_test, pred_test))
      rf_results[[key]]$oob_error <- c(rf_results[[key]]$oob_error,
                                       model$mse[model$ntree])
      
      rf_models[[key]][[i]] <- model
    }
  }
}

# 9. Function to summarize results
rf_summary <- function(result, label) {
  data.frame(
    Model = label,
    R2_train = mean(result$r2_train),
    R2_test = mean(result$r2_test),
    RMSE_train = mean(result$rmse_train),
    RMSE_test = mean(result$rmse_test),
    OOB_Error = mean(result$oob_error),
    SD_R2_train = sd(result$r2_train),
    SD_R2_test = sd(result$r2_test),
    SD_RMSE_train = sd(result$rmse_train),
    SD_RMSE_test = sd(result$rmse_test),
    SD_OOB = sd(result$oob_error)
  )
}

# Aggregate and print summary statistics for all configurations
summary_df <- do.call(rbind, lapply(names(rf_results), function(k) rf_summary(rf_results[[k]], k)))
print(summary_df)

# 10. Plot OOB error across number of trees for selected models
plot(seq(1, 300, by = 1), rf_models$sg2_sqrt[[50]]$mse, type = "l", lwd = 2, col = "black",
     xlim = c(10, 300), ylim = c(0, 0.02),
     xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "")
lines(seq(1, 300, by = 1), rf_models$sg2_log2[[50]]$mse, lwd = 2, col = "red")
lines(seq(1, 300, by = 1), rf_models$raw_div3[[50]]$mse, lwd = 2, col = "seagreen")
axis(1, seq(0, 300, by = 50), lwd = 2.5, cex.axis = 1.25)
axis(2, seq(0, 0.05, by = 0.005), lwd = 2.5, cex.axis = 1.25, las = 1)
mtext(expression(bold("Number of trees")), side = 1, line = 2.5, cex = 1.75)
mtext(expression(bold("OOB error")), side = 2, line = 4.5, cex = 1.75)
box(lwd = 2.5)

# 11. Visualize variable importance for a representative model
plot(seq(500, 4000, length.out = length(rf_models$sg2_div3[[50]]$importance[,2])),
     rf_models$sg2_sqrt[[50]]$importance[,2],
     type = "h", lwd = 2,
     xaxs = "i", yaxs = "i",
     xlim = c(4000, 600), ylim = c(0, 0.5),
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
axis(4, cex.axis = 1.25, col = "black", col.axis = "black", lwd = 2.5, lwd.ticks = 2.5)
mtext(expression(bold("Feature importance")), side = 4, line = 2.5,
      cex = 1.5, col = "black")
mtext(expression(bold("(Total reduction of MSE)")), side = 4, line = 4,
      cex = 1.5, col = "black")

# 12. Generate averaged predictions across models for fixed train/test split
set.seed(42)
idx <- sample(nrow(data))
split_idx <- floor(0.8 * length(idx))
train_idx <- idx[1:split_idx]
test_idx <- idx[(split_idx + 1):length(idx)]

x_train <- x_sg2[train_idx, ]
y_train <- y[train_idx]
x_test <- x_sg2[test_idx, ]
y_test <- y[test_idx]

train_preds <- sapply(rf_models$sg2_log2, function(m) predict(m, newdata = x_train))
mean_train_pred <- rowMeans(train_preds)

test_preds <- sapply(rf_models$sg2_log2, function(m) predict(m, newdata = x_test))
mean_test_pred <- rowMeans(test_preds)

# 13. Scatter plot of predicted vs measured CrI (scaled 0-100)
windows(6,6)
par(mar = c(4, 5, 2, 2), xpd = TRUE, lwd = 2.5, lwd.ticks = 2.5)

plot(y_train * 100, mean_train_pred * 100, type = "n",
     xlab = expression(bold("Measured CrI (%)")),
     ylab = expression(bold("Predicted CrI (%)")),
     xlim = c(0, 100), ylim = c(0, 100),
     xaxs = "i", yaxs = "i",
     cex.lab = 1.5, cex.axis = 1.25,
     axes = FALSE)

axis(1, lwd = 2.5, lwd.ticks = 2.5, cex.axis = 1.25)
axis(2, lwd = 2.5, lwd.ticks = 2.5, las = 1, cex.axis = 1.25)

points(y_train * 100, mean_train_pred * 100, pch = 1, col = "black", cex = 2.5)
points(y_test * 100, mean_test_pred * 100, pch = 2, col = "red", cex = 2.5)

par(xpd = FALSE)

abline(lm(mean_train_pred * 100 ~ y_train * 100), col = "black", lwd = 2.5)
abline(lm(mean_test_pred * 100 ~ y_test * 100), col = "red", lwd = 2.5)

legend("topleft",
       legend = c("Train", "Test"),
       pch = c(1, 2),
       col = c("black", "red"),
       pt.cex = 2.5,
       bty = "n",
       cex = 1.75,
       inset = 0.01)

box(lwd = 2.5)

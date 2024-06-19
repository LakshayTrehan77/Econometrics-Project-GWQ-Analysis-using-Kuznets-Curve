# Load necessary libraries
library(dplyr)
library(broom)

setwd("C:/Users/anous/OneDrive/Desktop/sixth sem/Econ/project")
merged_data <- read.csv("merged_data.csv")
merged_data <- na.omit(merged_data[, c("SDP_NORM", "ginivalue", "SDP_sq", "SDP_cube","sulfate")])
remove_outliers_zscore <- function(df, columns, threshold) {
  for (col in columns) {
    z_scores <- abs((df[[col]] - mean(df[[col]])) / sd(df[[col]]))
    df <- df[z_scores < threshold, ]
  }
  return(df)
}

columns_for_outlier_removal <- c('sulfate', 'SDP_NORM', 'ginivalue')
merged_data <- remove_outliers_zscore(merged_data, columns_for_outlier_removal, 3)
merged_data <- cbind(merged_data, 1)
names(merged_data)
X <- merged_data[, c("SDP_NORM", "ginivalue", "SDP_sq", "SDP_cube","1")]
y <- merged_data$sulfate
model <- lm(y ~ ., data = X)

residuals <- residuals(model)

mse <- mean(residuals^2)
print(paste("Mean Squared Error (MSE):", mse))


rsquared <- summary(model)$r.squared
fstatistic <- summary(model)$fstatistic[1]
print(paste("R-squared:", rsquared))
print(paste("F-statistic:", fstatistic))


summary(model)


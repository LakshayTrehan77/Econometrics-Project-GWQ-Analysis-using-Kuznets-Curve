library(tidyverse)# For modeling and summarizing
library(broom)      # For tidying model outputs

setwd("C:/Users/anous/OneDrive/Desktop/sixth sem/Econ/project")
# Read the CSV file into a data frame
merged_data <- read.csv("regionwise_data.csv")

merged_data <- na.omit(merged_data[, c("SDP_NORM", "ginivalue", "SDP_sq", "SDP_cube","sulfate")])
merged_data <- cbind(merged_data, 1)
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

z_scores <- abs((residuals - mean(residuals)) / sd(residuals))
threshold <- 3
outliers <- z_scores > threshold
num_outliers <- sum(outliers)
print(paste("Number of outliers:", sum(outliers)))


influence_list <- influence.measures(model)
influential_mat <- influence_list$infmat
cook_distance<- influential_mat[,"cook.d"]
threshold <- 4 / nrow(X)
influential_indices <- cook_distance > threshold
print(paste("Number of influential observations:", sum(influential_indices)))



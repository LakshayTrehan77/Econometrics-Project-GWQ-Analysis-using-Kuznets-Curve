library(dplyr)
library(broom)

setwd("C:/Users/anous/OneDrive/Desktop/sixth sem/Econ/project")
merged_data <- read.csv("merged_data.csv")
merged_data <- na.omit(merged_data[, c('sulfate', 'SDP_NORM', 'ginivalue')])
rem_outliers <- function(data, columns, threshold) {
  for (col in columns) {
    z_scores <- abs((data[[col]] - mean(data[[col]])) / sd(data[[col]]))
    data <- data[z_scores < threshold, ]
  }
  return(data)
}


merged_data <- rem_outliers(merged_data, c('sulfate', 'SDP_NORM', 'ginivalue'), 3)
merged_data <- cbind(merged_data, 1)
X <- merged_data[, c("SDP_NORM","1")]
y <- merged_data$sulfate

model <- lm(y ~ ., data = X)

residuals <- residuals(model)

mse <- mean(residuals^2)
print(paste("Mean Squared Error (MSE):", mse))

rsquared <- summary(model)$r.squared
print(paste("R-squared:", rsquared))

f_statistic <- summary(model)$fstatistic[1]
print(paste("F-statistic:", f_statistic))

print("\nSummary:")
print(summary(model))


plot(merged_data$SDP_NORM, y,
     xlab = "SDP",
     ylab = "Groundwater Quality Indicator",
     main = "Groundwater Quality Indicator vs SDP",
     col = "blue", 
     pch = 16)    
abline(a = coef(model)[1], b = coef(model)[2], col = "red",lwd=3)


plot(y, residuals,
     xlab = "Groundwater Quality Indicator",
     ylab = "Residuals",
     main = "Residuals vs Groundwater Quality Indicator",
     col = "red", 
     pch = 16)

plot(merged_data$SDP_NORM, residuals,
     xlab = "SDP",
     ylab = "Residuals",
     main = "Residuals vs SDP",
     col = "green",
     pch = 16)
abline(h=0, col = "red", lwd=3) 


hist(residuals, breaks = 20, 
     xlab = "Residuals", 
     ylab = "Frequency", 
     main = "Histogram of Residuals",
     col = "skyblue",  
     border = "white")


residuals_sum <- sum(residuals)
print(paste("Sum of residuals:", residuals_sum))


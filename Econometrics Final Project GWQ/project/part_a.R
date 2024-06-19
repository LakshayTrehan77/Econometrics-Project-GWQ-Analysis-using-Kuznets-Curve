library(tidyverse)# For modeling and summarizing
library(broom)      # For tidying model outputs

setwd("C:/Users/anous/OneDrive/Desktop/sixth sem/Econ/project")
merged_data <- read.csv("merged_data.csv")
merged_data <- na.omit(merged_data[c("sulfate", "SDP_NORM")])
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


# Scatter plot of Groundwater Quality Indicator vs SDP
plot(merged_data$SDP_NORM, y,
     xlab = "SDP",
     ylab = "Groundwater Quality Indicator",
     main = "Groundwater Quality Indicator vs SDP",
     col = "blue", 
     pch = 16)    



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


hist(residuals, breaks = 20, 
     xlab = "Residuals", 
     ylab = "Frequency", 
     main = "Histogram of Residuals",
     col = "skyblue",  
     border = "white")


residuals_sum <- sum(residuals)
print(paste("Sum of residuals:", residuals_sum))




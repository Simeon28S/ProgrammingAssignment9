# Programming Assignment 9
# Simeon Sipes
# 07/19/26
# Linear regression using the mtcars dataset

# Create a scatterplot of MPG based on vehicle weight.
plot(mtcars$wt, mtcars$mpg,
     main = "MPG vs Vehicle Weight",
     xlab = "Vehicle Weight (1000 lbs)",
     ylab = "Miles Per Gallon",
     pch = 19,
     col = "blue")

# Fit a linear model to predict MPG based on vehicle weight.
lm_mpg_weight <- lm(mpg ~ wt, data = mtcars)

# Add the fitted regression line to the scatterplot.
abline(lm_mpg_weight, col = "red")

# Extract the slope and hypothesis test results.
slope <- coef(lm_mpg_weight)["wt"]
summary_model <- summary(lm_mpg_weight)

p_value <- summary_model$coefficients["wt", "Pr(>|t|)"]

cat("Slope Estimate:", slope, "\n")
cat("p-value:", p_value, "\n")

# Calculate the 95% confidence interval for the slope.
conf_interval <- confint(lm_mpg_weight, "wt", level = 0.95)

cat("95% Confidence Interval for Slope:",
    conf_interval[1], conf_interval[2], "\n")

# Interpret the slope and statistical significance.
cat("The slope indicates that for each additional 1,000 pounds,\n")
cat("the expected MPG changes by", slope, "miles per gallon.\n")

alpha <- 0.05

if (p_value < alpha) {
  cat("The slope is statistically significant at the 0.05 level, indicating\n")
  cat("a significant relationship between weight and fuel efficiency.\n")
} else {
  cat("The slope is not statistically significant at the 0.05 level.\n")
}

# Predict MPG for a vehicle weighing 6,000 pounds.
new_weight <- data.frame(wt = 6)

mpg_prediction <- predict(lm_mpg_weight,
                          newdata = new_weight,
                          interval = "prediction",
                          level = 0.95)

cat("Predicted MPG for a 6,000-pound vehicle:\n")
print(mpg_prediction)

# Check whether the predicted weight is within the dataset range.
weight_range <- range(mtcars$wt)

if (6 < weight_range[1] || 6 > weight_range[2]) {
  cat("Note: The weight value is outside the range of the dataset, which\n")
  cat("limits the accuracy of the prediction. Use caution when interpreting\n")
  cat("this result.\n")
}

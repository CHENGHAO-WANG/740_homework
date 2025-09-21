############
# problem 2.3(a)
############

# Set parameters
n <- 100          # length of series
delta <- 0.1      # drift parameter
sigma_w <- 1      # standard deviation of white noise
num_series <- 6   # number of series to generate

# Set seed for reproducibility
set.seed(123)

# Initialize storage for series and results
series_list <- list()
regression_results <- list()

# Generate six random walk with drift series
for (i in 1:num_series) {
  # Generate white noise
  w_t <- rnorm(n, mean = 0, sd = sigma_w)
  
  # Initialize series
  x_t <- numeric(n)
  x_t[1] <- delta + w_t[1]  # Starting value with drift
  
  # Generate random walk with drift: x_t = delta + x_{t-1} + w_t
  for (t in 2:n) {
    x_t[t] <- delta + x_t[t-1] + w_t[t]
  }
  
  # Store the series
  series_list[[i]] <- x_t
  
  # Create time variable
  time_t <- 1:n
  
  # Fit regression: x_t = beta * t + w_t
  lm_model <- lm(x_t ~ time_t)
  regression_results[[i]] <- lm_model
}

# Load astsa package for tsplot
library(astsa)

# Save plot to plots folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
png("../plots/Problem_2.3a_random_walk_with_drift.png", width = 12, height = 8, units = "in", res = 300)

# Plot all series using tsplot
# Set layout with extra margin on right for legend
layout(matrix(1:6, 2, 3))
par(oma = c(0, 0, 0, 10))  # Add outer margin on right side
for (i in 1:num_series) {  
  # Plot using tsplot
  tsplot(series_list[[i]], main = paste("Random Walk with Drift - Series", i),
         ylab = "x_t", col = "blue")
  
  # Add fitted regression line (slope only, no intercept)
  time_t <- 1:n
  fitted_slope <- coef(regression_results[[i]])[2]
  fitted_line <- fitted_slope * time_t
  lines(time_t, fitted_line, col = "red", lwd = 2)
  
  # Add true mean line (0.1*t, no intercept)
  true_mean <- 0.1 * time_t
  lines(time_t, true_mean, col = "green", lwd = 2, lty = 2)
  
}

# Add single legend in the right margin
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 10), mar = c(0, 0, 0, 0), new = TRUE)
par(xpd = NA)
plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = 1.1, y = 0.4, legend = c("Data", "Fitted Line", "True Mean"), 
       col = c("blue", "red", "green"), lty = c(1, 1, 2), 
       horiz = FALSE, cex = 1.2, bty = "n", inset = c(0, 0))

# Close the PNG device to save the plot
dev.off()




############
# problem 2.3(b)
############

# Set parameters for linear trend plus noise
n <- 100          # length of series
beta <- 0.1       # linear trend coefficient
sigma_w <- 1      # standard deviation of white noise
num_series <- 6   # number of series to generate

# Set seed for reproducibility
set.seed(456)

# Initialize storage for series and results
series_list_b <- list()
regression_results_b <- list()

# Generate six linear trend plus noise series
for (i in 1:num_series) {
  # Generate white noise
  w_t <- rnorm(n, mean = 0, sd = sigma_w)
  
  # Create time variable
  time_t <- 1:n
  
  # Generate linear trend plus noise: x_t = beta * t + w_t
  x_t <- beta * time_t + w_t
  
  # Store the series
  series_list_b[[i]] <- x_t
  
  # Fit regression: x_t = alpha + beta * t + epsilon_t
  lm_model <- lm(x_t ~ time_t)
  regression_results_b[[i]] <- lm_model
}

# Save plot to plots folder
png("../plots/Problem_2.3b_linear_trend_plus_noise.png", width = 12, height = 8, units = "in", res = 300)

# Plot all series using tsplot
# Set layout with extra margin on right for legend
layout(matrix(1:6, 2, 3))
par(oma = c(0, 0, 0, 10))  # Add outer margin on right side
for (i in 1:num_series) {  
  # Plot using tsplot
  tsplot(series_list_b[[i]], main = paste("Linear Trend Plus Noise - Series", i),
         ylab = "x_t", col = "blue")
  
  # Add fitted regression line
  time_t <- 1:n
  fitted_values <- predict(regression_results_b[[i]])
  lines(time_t, fitted_values, col = "red", lwd = 2)
  
  # Add true mean line (beta*t = 0.1*t)
  true_mean <- beta * time_t
  lines(time_t, true_mean, col = "green", lwd = 2, lty = 2)
  
}

# Add single legend in the right margin
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 10), mar = c(0, 0, 0, 0), new = TRUE)
par(xpd = NA)
plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
legend(x = 1.1, y = 0.4, legend = c("Data", "Fitted Line", "True Mean"), 
       col = c("blue", "red", "green"), lty = c(1, 1, 2), 
       horiz = FALSE, cex = 1.2, bty = "n", inset = c(0, 0))

# Close the PNG device to save the plot
dev.off()

############
# problem 2.3(c)
############

# The trend in 2.3(b) looks more linear than the trend in 2.3(a).
# This is because that, in 2.3(a), x_t depends on x_{t-1} in addition to w_t,
# such that x_t depends on all the previous white noises,
# which leads to a more wavy trend.
# Yet in 2.3(b), y_t depends only on one w_t, the white noise at time t，
# which perfectly conforms the assumptions of linear models.
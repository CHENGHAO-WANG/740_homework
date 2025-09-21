############
# problem 1.3(a)
############

n <- 100
sigma_w <- 1
w <- rnorm(n + 50, sd = sigma_w)  # 50 extra to avoid startup problems
# AR model: x_t = -0.9*x_{t-2} + w_t
x <- filter(w, filter=c(0, -0.9), method="recursive")[-(1:50)]
# Apply moving average filter: v_t = (x_t + x_{t-1} + x_{t-2} + x_{t-3})/4
v <- filter(x, filter=rep(1/4, 4), method="convolution", sides=1)

# Save plot to plots folder
png("plots/Problem_1.3a_autoregression_with_filter.png", width=800, height=600)
tsplot(x, main="AR(2) Series with Moving Average Filter", col=4, gg=TRUE, ylab="Value")
lines(v, col=2, lty=2, lwd=2)  # Add dashed line for filtered series
legend("topright", legend=c("x_t (original)", "v_t (MA filter)"), 
       col=c(4, 2), lty=c(1, 2), lwd=c(1, 2))
dev.off()

# Comments on the results:
# x_t exhibits typical AR(2) behavior with oscillatory patterns and varying amplitudes.
# The moving average filter v_t shows smmoothing of the original series x_t,
# substantially reducing the amplitude while maintaining the overall trend.
# Yet, it exhibits phase lag. That is to say, v_t lags behind x_t, especially at some points.

############
# problem 1.3(b)
############

# Generate cosine series: x_t = cos(2*pi*t/4)
t <- 1:n
x_cos <- cos(2*pi*t/4)

# Apply moving average filter: v_t = (x_t + x_{t-1} + x_{t-2} + x_{t-3})/4
v_cos <- filter(x_cos, filter=rep(1/4, 4), method="convolution", sides=1)

# Save plot to plots folder
png("plots/Problem_1.3b_cosine_with_filter.png", width=800, height=600)
tsplot(x_cos, main="Cosine Series with Moving Average Filter", col=4, gg=TRUE, ylab="Value")
lines(v_cos, col=2, lty=2, lwd=2)  # Add dashed line for filtered series
legend("topright", legend=c("x_t = cos(2πt/4)", "v_t (MA filter)"), 
       col=c(4, 2), lty=c(1, 2), lwd=c(1, 2))
dev.off()

# Comments on the results:
# The x_t shows a perfect cosine wave with no noise.
# The moving average filter v_t shows extreme smmoothing of the original series x_t,
# giving an almost horizontal line.

############
# problem 1.3(c)
############

# Generate cosine series with noise: x_t = cos(2*pi*t/4) + w_t
w_c <- rnorm(n)  # standard normal noise
x_cosine_noise <- cos(2*pi*t/4) + w_c

# Apply moving average filter: v_t = (x_t + x_{t-1} + x_{t-2} + x_{t-3})/4
v_cosine_noise <- filter(x_cosine_noise, filter=rep(1/4, 4), method="convolution", sides=1)

# Save plot to plots folder
png("plots/Problem_1.3c_cosine_noise_with_filter.png", width=800, height=600)
tsplot(x_cosine_noise, main="Cosine + Noise Series with Moving Average Filter", col=4, gg=TRUE, ylab="Value")
lines(v_cosine_noise, col=2, lty=2, lwd=2)  # Add dashed line for filtered series
legend("topright", legend=c("x_t = cos(2πt/4) + w_t", "v_t (MA filter)"), 
       col=c(4, 2), lty=c(1, 2), lwd=c(1, 2))
dev.off()

# Comments on the results:
# The plot looks similar to the plot in 1.3(a), but with more variation,
# even after applying the moving average filter.

############
# problem 1.3(d)
############

# The moving average smooth each series,
# while preserving the trend that deviates from periodic pattern.
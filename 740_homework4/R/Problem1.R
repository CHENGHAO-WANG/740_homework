#############
# Problem 1(a)
#############

# Set seed for reproducibility
set.seed(123)

# Define matrix A
A <- matrix(c(5, 10, 10, 2), nrow = 2, byrow = TRUE)

# Number of simulations
n <- 500

# Generate independent uniform(-1, 1) random variables
s1 <- runif(n, min = -1, max = 1)
s2 <- runif(n, min = -1, max = 1)

# Create matrix S (2 x 500)
S <- rbind(s1, s2)

# Calculate X = A * S
X <- A %*% S

# Extract x1 and x2
x1 <- X[1, ]
x2 <- X[2, ]

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Open PNG device
png("../plots/Problem1a.png", width = 1200, height = 600, res = 100)

# Set up a 1x2 plotting layout
par(mfrow = c(1, 2))

# Plot s1 and s2
plot(s1, s2, main = "Source Variables (s1, s2)", 
     xlab = "s1", ylab = "s2", pch = 16, col = rgb(0, 0, 1, 0.5),
     xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
abline(h = 0, v = 0, col = "gray", lty = 2)

# Plot x1 and x2
plot(x1, x2, main = "Observed Variables (x1, x2)", 
     xlab = "x1", ylab = "x2", pch = 16, col = rgb(1, 0, 0, 0.5),
     xlim = c(-20, 20), ylim = c(-15, 15))
abline(h = 0, v = 0, col = "gray", lty = 2)

# Close the PNG device
dev.off()

# Reset plotting layout for future plots
par(mfrow = c(1, 1))

# Print confirmation message
cat("Plots saved as 'scatter_plots.png'\n")

#############
# Problem 1(b)
#############

# Apply PCA to the observed variables
X_matrix <- cbind(x1, x2)
pca_result <- prcomp(X_matrix, center = TRUE, scale. = FALSE)

# Extract principal components
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]

# Print PCA summary
print(summary(pca_result))
# Importance of components:
#                          PC1    PC2
# Standard deviation     7.9666 3.7345
# Proportion of Variance 0.8198 0.1802
# Cumulative Proportion  0.8198 1.0000

# Rotation matrix (loadings):"
print(pca_result$rotation)
#          PC1        PC2
# x1 0.7684730  0.6398822
# x2 0.6398822 -0.7684730

# Create plot with PCA results
png("../plots/Problem1b_pca_plot.png", width = 800, height = 600, res = 100)

# Calculate plot limits with margins
pc1_range <- range(pc1)
pc2_range <- range(pc2)
pc1_margin <- diff(pc1_range) * 0.1
pc2_margin <- diff(pc2_range) * 0.1

plot(pc1, pc2, main = "PCA of Observed Variables", 
     xlab = "PC1", ylab = "PC2", pch = 16, col = rgb(0, 0.5, 0, 0.5),
     xlim = c(pc1_range[1] - pc1_margin, pc1_range[2] + pc1_margin),
     ylim = c(pc2_range[1] - pc2_margin, pc2_range[2] + pc2_margin))
abline(h = 0, v = 0, col = "gray", lty = 2)

dev.off()

#############
# Problem 1(c)
#############

library(fastICA)

# Apply ICA to the observed variables
X_matrix <- cbind(x1, x2)
ica_result <- fastICA(X_matrix, n.comp = 2)

# Extract independent components
ic1 <- ica_result$S[, 1]
ic2 <- ica_result$S[, 2]

# Print ICA results
# Estimated mixing matrix A
print(ica_result$A)
# [,1]     [,2]
# [1,] -5.932150 -1.40662
# [2,]  2.813246  5.67238
# Unmixing matrix W
print(ica_result$W)
# [,1]       [,2]
# [1,] 0.6858902 -0.7277051
# [2,] 0.7277051  0.6858902

# Create comparison plot
png("../plots/Problem1c_pca_ica_comparison.png", width = 1600, height = 1600, res = 100)

par(mfrow = c(2, 2))

# Plot 1: Original sources (s1, s2)
plot(s1, s2, main = "Original Sources (s1, s2)", 
     xlab = "s1", ylab = "s2", pch = 16, col = rgb(0, 0, 1, 0.5),
     xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
abline(h = 0, v = 0, col = "gray", lty = 2)

# Plot 2: Observed mixed data (x1, x2)
x1_range <- range(x1)
x2_range <- range(x2)
x1_margin <- diff(x1_range) * 0.1
x2_margin <- diff(x2_range) * 0.1

plot(x1, x2, main = "Observed Variables (x1, x2)", 
     xlab = "x1", ylab = "x2", pch = 16, col = rgb(1, 0, 0, 0.5),
     xlim = c(x1_range[1] - x1_margin, x1_range[2] + x1_margin),
     ylim = c(x2_range[1] - x2_margin, x2_range[2] + x2_margin))
abline(h = 0, v = 0, col = "gray", lty = 2)

# Plot 3: PCA recovered components (PC1, PC2)
pc1_range <- range(pc1)
pc2_range <- range(pc2)
pc1_margin <- diff(pc1_range) * 0.1
pc2_margin <- diff(pc2_range) * 0.1

plot(pc1, pc2, main = "PCA of Observed Variables", 
     xlab = "PC1", ylab = "PC2", pch = 16, col = rgb(0, 0.5, 0, 0.5),
     xlim = c(pc1_range[1] - pc1_margin, pc1_range[2] + pc1_margin),
     ylim = c(pc2_range[1] - pc2_margin, pc2_range[2] + pc2_margin))
abline(h = 0, v = 0, col = "gray", lty = 2)

# Plot 4: ICA recovered components (IC1, IC2)
ic1_range <- range(ic1)
ic2_range <- range(ic2)
ic1_margin <- diff(ic1_range) * 0.1
ic2_margin <- diff(ic2_range) * 0.1

plot(ic1, ic2, main = "ICA Components (IC1, IC2)", 
     xlab = "IC1", ylab = "IC2", pch = 16, col = rgb(1, 0.5, 0, 0.5),
     xlim = c(ic1_range[1] - ic1_margin, ic1_range[2] + ic1_margin),
     ylim = c(ic2_range[1] - ic2_margin, ic2_range[2] + ic2_margin))
abline(h = 0, v = 0, col = "gray", lty = 2)

dev.off()

par(mfrow = c(1, 1))

# From the plots, we see that ICA is able to recover the original, independent source signals (s1, s2),
# while PCA does not effectively separate the mixed signals. Instead,
# PCA maximizes variance along orthogonal directions.

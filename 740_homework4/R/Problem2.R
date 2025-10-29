#############
# Problem 2
#############

# svd() from base R

# Singular Value Decomposition (SVD) Examples in R
# ===================================================

# EXAMPLE 1: Simple 3x2 Matrix
# ==================================================

# Create a simple matrix
A1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow=3, ncol=2)
A1
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6

# Perform SVD
svd_result1 <- svd(A1)

# U matrix (left singular vectors)
svd_result1$u
# [,1]       [,2]
# [1,] -0.4286671  0.8059639
# [2,] -0.5663069  0.1123824
# [3,] -0.7039467 -0.5811991

# Singular values (d)
svd_result1$d
# [1] 9.5080320 0.7728696

# V matrix (right singular vectors)
svd_result1$v
# [,1]       [,2]
# [1,] -0.3863177 -0.9223658
# [2,] -0.9223658  0.3863177

# Verify: A = U %*% diag(d) %*% t(V)
A1_reconstructed <- svd_result1$u %*% diag(svd_result1$d) %*% t(svd_result1$v)
# Reconstructed Matrix (should equal A)
A1_reconstructed
# [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6

# Reconstruction error (should be near zero)
max(abs(A1 - A1_reconstructed))
# [1] 9.992007e-16

# EXAMPLE 2: Low-Rank Approximation and Image Compression
# =========================================================

# Create a simple "image" matrix with patterns
image_matrix <- outer(1:20, 1:20, function(x,y) {
  sin(x/3) * cos(y/3) + 0.5*sin(x/2) * cos(y/4) + 0.3*cos(x/5) * sin(y/2)
})
# Original 'image' matrix (20x20), showing top-left corner
round(image_matrix[1:5, 1:5], 2)
# [,1] [,2] [,3] [,4] [,5]
# [1,] 0.68 0.71 0.65 0.47 0.22
# [2,] 1.12 1.09 0.92 0.62 0.24
# [3,] 1.40 1.31 1.07 0.69 0.22
# [4,] 1.46 1.34 1.07 0.66 0.18
# [5,] 1.31 1.18 0.92 0.54 0.10

svd_img <- svd(image_matrix)

# Singular values
round(svd_img$d, 2)
# [1] 12.08  3.98  1.16  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
# [17]  0.00  0.00  0.00  0.00

# Note that it's approximately rank-3 (But not exactly rank-3! It's full rank).

# Cumulative proportion of variance explained
cumvar <- cumsum(svd_img$d^2) / sum(svd_img$d^2)
for (i in c(1, 2, 3, 20)) {
  cat(sprintf("  First %2d components: %.2f%%\n", i, cumvar[i] * 100))
}
# First  1 components: 89.48%
# First  2 components: 99.17%
# First  3 components: 100.00%
# First 20 components: 100.00%

# Compress using different ranks
# Low-rank approximations
for (rank in c(1, 2)) {
  if (rank == 1) {
    # Special case for rank 1: use outer product
    compressed <- svd_img$d[1] * svd_img$u[, 1] %*% t(svd_img$v[, 1])
  } else {
    compressed <- svd_img$u[, 1:rank] %*% 
      diag(svd_img$d[1:rank]) %*% 
      t(svd_img$v[, 1:rank])
  }
  
  # Calculate errors
  # norm(A, "F") calculates the Frobenius norm, which is ||A||_F = sqrt(sum of all squared elements)
  abs_error <- norm(image_matrix - compressed, "F")
  rel_error <- abs_error / norm(image_matrix, "F")
  
  # Calculate compression ratio
  original_storage <- 20 * 20
  compressed_storage <- rank * (20 + 1 + 20)  # U columns + d values + V columns
  compression_ratio <- original_storage / compressed_storage
  
  cat(sprintf("  Rank-%2d: Relative error = %.4f, Compression ratio = %.2fx\n", 
              rank, rel_error, compression_ratio))
}

# Rank- 1: Relative error = 0.3244, Compression ratio = 9.76x
# Rank- 2: Relative error = 0.0909, Compression ratio = 4.88x

# Interpretation:
#   - Lower rank = more compression but higher error (blurrier)
#   - Higher rank = less compression but lower error (sharper)
#   - First few singular values capture most of the information!

# Visualize original and compressed images

# Prepare for plotting - create approximations at different ranks
ranks_to_plot <- c(1, 2, 3)
approx_list <- list()
approx_list[["Original"]] <- image_matrix

for (rank in ranks_to_plot) {
  if (rank == 1) {
    compressed <- svd_img$d[1] * svd_img$u[, 1] %*% t(svd_img$v[, 1])
  } else {
    compressed <- svd_img$u[, 1:rank] %*% 
      diag(svd_img$d[1:rank]) %*% 
      t(svd_img$v[, 1:rank])
  }
  approx_list[[paste0("Rank-", rank)]] <- compressed
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
png("../plots/Problem2_svd_image_compression.png", width = 800, height = 800, res = 100)
# Create a 2x2 plot
par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))

# Plot original
image(image_matrix, col = gray.colors(256), 
      main = "Original Image", axes = FALSE)
box()

# Plot approximations
for (rank in ranks_to_plot) {
  compressed <- approx_list[[paste0("Rank-", rank)]]
  
  # Calculate error metrics
  abs_error <- norm(image_matrix - compressed, "F")
  rel_error <- abs_error / norm(image_matrix, "F")
  
  # Calculate compression ratio
  original_storage <- 50 * 50
  compressed_storage <- rank * (50 + 1 + 50)
  compression_ratio <- original_storage / compressed_storage
  
  # Plot
  image(compressed, col = gray.colors(256), 
        main = sprintf("Rank-%d\nError: %.1f%%, Compression: %.2fx", 
                       rank, rel_error * 100, compression_ratio),
        axes = FALSE)
  box()
}

dev.off()

# Reset plot parameters
par(mfrow = c(1, 1))


# EXAMPLE 3: Using SVD for Moore-Penrose Pseudoinverse
# ======================================================

# Non-square matrix
A3 <- matrix(c(1, 2, 3, 4, 5, 6), nrow=2, ncol=3)
# Original Matrix A (2x3)
A3
# [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6

svd3 <- svd(A3)

# Compute Moore-Penrose pseudoinverse: V %*% diag(1/d) %*% t(U)
if (length(svd3$d) == 1) {
  A3_pinv <- (1/svd3$d[1]) * svd3$v %*% t(svd3$u)
} else {
  A3_pinv <- svd3$v %*% diag(1/svd3$d) %*% t(svd3$u)
}
# Moore-Penrose Pseudoinverse of A
A3_pinv
# [,1]       [,2]
# [1,] -1.3333333  1.0833333
# [2,] -0.3333333  0.3333333
# [3,]  0.6666667 -0.4166667

# Verify: A %*% A+ %*% A = A
verification <- A3 %*% A3_pinv %*% A3
# Verification A*A+*A (should equal A)
verification
# [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6

# Reconstruction error (should be near zero)
max(abs(A3 - verification))
# [1] 2.220446e-15
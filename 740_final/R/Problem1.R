#############
# Problem 1
#############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

test_whiteness <- function(x, save_path = NULL, series_name = NULL) {
  
  # If x has a name (e.g., from a data.frame column), use it
  if (is.null(series_name)) {
    series_name <- deparse(substitute(x))
  }
  
  # Spectrum estimation
  spec <- spec.pgram(x, spans = NULL, taper = 0, plot = FALSE)
  I <- spec$spec
  freq <- spec$freq
  
  # Confidence limits for white noise
  alpha <- 0.05
  mean_I <- mean(I)
  lower <- qchisq(alpha/2, df=2) * mean_I / 2
  upper <- qchisq(1-alpha/2, df=2) * mean_I / 2
  
  # Plotting
  plot_single <- function() {
    
    # Plot periodogram as vertical spikes
    plot(freq, I, type='h',
         main = paste("Whiteness Test:", series_name),
         xlab="Frequency", ylab="Power")
    
    # Horizontal CI bands
    segments(min(freq), lower, max(freq), lower, col="red", lty=2)
    segments(min(freq), upper, max(freq), upper, col="red", lty=2)
    
    # Mean line
    segments(min(freq), mean_I, max(freq), mean_I, col="blue", lty=1)
    
    # --- Binomial test for exceedances ---
    n <- length(freq)
    m <- sum(I > upper | I < lower)  # total exceedances
    binom_res <- binom.test(m, n, p = 0.05, alternative = "greater")
    pval <- binom_res$p.value
    
    # Add p-value text to the plot
    legend("topright", legend = paste0("Binom p-val = ", signif(pval, 3)),
           bty = "n")
  }
  
  
  
  # If save_path is provided, save the plot
  if (!is.null(save_path)) {
    filename <- file.path(save_path,
                          paste0("whiteness_", series_name, ".png"))
    
    png(filename, width=800, height=600)
    plot_single()
    dev.off()
  }
  
  # Plot to screen as usual
  if (is.null(save_path)) {
    plot_single()
  }
  
  # Return results
  list(
    series_name = series_name,
    freq = freq,
    I = I,
    mean_I = mean_I,
    ci_lower = lower,
    ci_upper = upper
  )
}

library(astsa)

X <- fmri1[, 4:9]  
save_dir <- "../plots/"      # Saving directory

results <- mapply(
  FUN = function(column, name) {
    test_whiteness(column,
                   save_path = save_dir,
                   series_name = name)
  },
  column = as.data.frame(X),
  name = colnames(X),
  SIMPLIFY = FALSE
)

# An fMRI series is considered as white noise if most of its
# periodogram ordinates fall within the confidence bands.
# We performed a one-side exact binomial test to formally
# evalute this. Since the 95% confidence bands are constructed,
# under the null hypothesis of white noise, the proportion of the
# periodogram ordinates exceeding the bands due to chance are
# expected to be less than 5%. And this is the null hypothesis
# of the binomial test. The p-values are shown in the plots.


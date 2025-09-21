############
# problem 1.1
############

library(astsa)

# Plot both series on the same graph with different colors and linetypes
png("plots/Problem_1.1_earthquake_explosion_comparison.png", width = 2000, height = 1000, res = 200)

# Plot both time series using tsplot with different colors and line types
# Set ylim to accommodate both series
tsplot(EQ5, col = "red", lty = 1, lwd = 2, 
       ylab = "Amplitude", main = "Earthquake vs Explosion Seismic Data Comparison",
       ylim = range(c(EQ5, EXP6)))

# Add explosion data to the same plot
lines(EXP6, col = "blue", lty = 2, lwd = 2)

# Add legend with transparent background and better positioning
legend("topleft", legend = c("Earthquake (EQ5)", "Explosion (EXP6)"), 
       col = c("red", "blue"), lty = c(1, 2), lwd = 1, 
       bg = "white", box.col = "gray", cex = 0.9)

# Close the PNG device
dev.off()

# Comments on the results:
# Plotting both series on the same graph allows for direct
# visual comparison of the seismic patterns between earthquake and explosion events.
#
# P phase (t = 1 - 1024): 
# Both signals show relatively low amplitude activity overall.
# The explosion exhibits a notable burst right at the beginning of the P phase, 
# then becomes less variable.
# The earthquake displays more even, consistent variation throughout the P phase
# with more consistent baseline noise levels. Also, its amplitude is higher than the explosion
# except at the beginning of the P phase.
#
# S phase (t = 1025 - 2048):
# This is where both signals reach their peak activity, 
# The explosion (EXP6, blue dashed line) shows very sharp, 
# concentrated bursts of high-amplitude activity that decay relatively quickly
#The earthquake (EQ5, red solid line) demonstrates more prolonged, 
# sustained high-amplitude oscillations that persist throughout much of the S phase.

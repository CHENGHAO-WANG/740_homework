############
# problem 4.2(a)
############
library(astsa)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

n <- 128

x1 <- 2*cos(2*pi*1:n*6/100) + 3*sin(2*pi*1:n*6/100)
x2 <- 4*cos(2*pi*1:n*10/100) + 5*sin(2*pi*1:n*10/100)
x3 <- 6*cos(2*pi*1:n*40/100) + 7*sin(2*pi*1:n*40/100)
x <- x1 + x2 + x3

png("../plots/Problem_4.2a.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2,2), cex.main=1, font.main=1)
tsplot(x1, ylim=c(-10,10), main=bquote(omega==6/100 ~~ A^2==13), col=4, gg=TRUE)
tsplot(x2, ylim=c(-10,10), main=bquote(omega==10/100 ~~ A^2==41), col=4, gg=TRUE)
tsplot(x3, ylim=c(-10,10), main=bquote(omega==40/100 ~~ A^2==85), col=4, gg=TRUE)
tsplot(x, ylim=c(-16,16), main="sum", col=4, gg=TRUE)
dev.off()

# Since the denominator in frequencies is 100,
# which matches the series length when n = 100,
# the three frequencies correspond to exactly 6, 10, and 40 complete cycles.
# When n = 128, we will have truncated cycles.


############
# problem 4.2(b)
############

per <- Mod( fft(x)/sqrt(n) )^2
P <- (4/n) * per
Fr <- 0:(n-1)/n

png("../plots/Problem_4.2b.png", width = 12, height = 8, units = "in", res = 300)
tsplot(Fr, P, type = "h", lwd = 3, xlab = "frequency", ylab = "scaled periodogram",
    col = 4, gg = TRUE)
abline(v = .5, lty = 5, col = 8)
dev.off()

# The plot is very similar to the one in Example 4.2 (where n = 100),
# as the frequencies and amplitudes don't change.
# However, we can see that when n = 100, the periodogram looks "cleaner".
# In the region where there are no peaks,
# there's little signal (sample variance) when n = 100,
# while there's energy leaking to neighboring frequencies from the peaks when n = 128.

############
# problem 4.2(c)
############

n <- 100
sigma_w <- 5

set.seed(1)
x1 <- 2*cos(2*pi*1:n*6/100) + 3*sin(2*pi*1:n*6/100)
x2 <- 4*cos(2*pi*1:n*10/100) + 5*sin(2*pi*1:n*10/100)
x3 <- 6*cos(2*pi*1:n*40/100) + 7*sin(2*pi*1:n*40/100)
w <- rnorm(n, 0, sigma_w)
x <- x1 + x2 + x3 + w

png("../plots/Problem_4.2c1.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2,2), cex.main=1, font.main=1)
tsplot(x1, ylim=c(-10,10), main=bquote(omega==6/100 ~~ A^2==13), col=4, gg=TRUE)
tsplot(x2, ylim=c(-10,10), main=bquote(omega==10/100 ~~ A^2==41), col=4, gg=TRUE)
tsplot(x3, ylim=c(-10,10), main=bquote(omega==40/100 ~~ A^2==85), col=4, gg=TRUE)
tsplot(x, ylim=c(-16,16), main="sum", col=4, gg=TRUE)
dev.off()

# The pattern isn't changed much for x,
# and the peaks are still visible.
# But the cyclicial pattern has been distorted,
# and there's more noise.

per <- Mod( fft(x)/sqrt(n) )^2
P <- (4/n) * per
Fr <- 0:(n-1)/n

png("../plots/Problem_4.2c2.png", width = 12, height = 8, units = "in", res = 300)
tsplot(Fr, P, type = "h", lwd = 3, xlab = "frequency", ylab = "scaled periodogram",
    col = 4, gg = TRUE)
abline(v = .5, lty = 5, col = 8)
dev.off()

# The periodogram shows more noise, even compared with the plot
# in 4.2(b) (when n = 128). We see many small spurious peaks,
# and they can even appear at the frequencies far from the peaks.
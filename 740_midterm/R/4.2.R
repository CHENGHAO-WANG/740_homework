############
# problem 4.2(a)
############
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

n <- 128

x1 <- 2*cos(2*pi*1:n*6/100) + 3*sin(2*pi*1:n*6/100)
x2 <- 4*cos(2*pi*1:n*10/100) + 5*sin(2*pi*1:n*10/100)
x3 <- 6*cos(2*pi*1:n*40/100) + 7*sin(2*pi*1:n*40/100)
x <- x1 + x2 + x3

par(mfrow = c(2,2), cex.main=1, font.main=1)
tsplot(x1, ylim=c(-10,10), main=bquote(omega==6/100~~Aˆ2==13), col=4, gg=TRUE)
tsplot(x2, ylim=c(-10,10), main=bquote(omega==10/100~~Aˆ2==41), col=4, gg=TRUE)
tsplot(x3, ylim=c(-10,10), main=bquote(omega==40/100~~Aˆ2==85), col=4, gg=TRUE)
tsplot(x, ylim=c(-16,16), main="sum", col=4, gg=TRUE)



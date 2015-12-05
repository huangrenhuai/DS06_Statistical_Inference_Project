nosim <- c(20,1000, 5000)
lambda <-0.2
cfunc <- function(x, n) {sqrt(n)*(mean(x)-1/lambda)*lambda}
# m1<- matrix(rexp(nosim*10,lambda), nosim)
# hist(apply(m1, 1, cfunc, 10))

set.seed(100)
dat <- data.frame(
  
  x = c(apply(matrix(rexp(nosim[1]*40,lambda), nosim[1]), 1, cfunc, 40),
        apply(matrix(rexp(nosim[2]*40,lambda), nosim[2]), 1, cfunc, 40),
        apply(matrix(rexp(nosim[3]*40,lambda), nosim[3]), 1, cfunc, 40)
        ),
  # size = factor(rep(nosim, rep(nosim, 3))))
  size = factor(unlist(sapply(nosim, function(x) {rep(x,x)}))))
  
library(ggplot2)
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2, lty=2, col = "red")
g + facet_grid(. ~ size)

####
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
  x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                     nosim), 1, cfunc, 10),
        apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                     nosim), 1, cfunc, 20),
        apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                     nosim), 1, cfunc, 30)
        ),
  size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
library(ggplot2)
g <- ggplot(dat, aes(x = x, fill = size)) 
g <- g+ geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 1, col="red")
g + facet_grid(. ~ size)
#########


########
n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage2 <- sapply(pvals, function(p) {
  rbinom(nosim, prob = p, size = n)/n
 

})
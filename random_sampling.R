# Check if the given function is a PDF or not.

#Define function
f <- function(x) x/4*exp(-(x^2)/8)
ex <- function(x) x*f(x)
ex2 <- function(x) x*g(x)

# Integrate over domain
area <- integrate(f, 0, Inf)$value
area

# Expected Value
expected_value <- integrate(ex,0,Inf)$value
expected_value

# Variance 
ex_value_2<- integrate(ex2, 0, Inf)$value

VarX <- ex_value_2 - expected_value^2
VarX

curve(dchisq(x, df=10),
      xlim = c(0,25),
      main = "PDF of Chi-Squared Distribution")

par(mfrow = c(1,2))
barplot(PS,
        ylim = c(0,0.2),
        xlab = "S",
        ylab = "Probability",
        col = "steelblue",
        space = 0,
        main = "Sum of Two Dice Rolls")
probability <- rep(1/6,6)
names(probability) <- 1:6

barplot(probability, 
        ylim = c(0, 0.2), 
        xlab = "D", 
        col = "steelblue", 
        space = 0, 
        main = "Outcome of a Single Dice Roll")

n <-10
reps <- 10000

samples <- replicate(reps, rnorm(n))
sample.avgs <- colMeans(samples)
is.vector(sample.avgs)
head(sample.avgs)

hist(sample.avgs,
     ylim = c(0,1.4),
     col = "steelblue",
     freq = F,
     breaks = 20,
     main = "Histogram")
curve(dnorm(x, sd = 1/sqrt(n)),
      col = "red",
      lwd = "2",
      add = T)
par(mfrow = c(2,2))


reps <- 10000
DF <- 3

Z <- replicate(reps, rnorm(DF))
X <- colSums(Z^2)

hist(X,
     freq = F,
     col = "steelblue",
     breaks = 40,
     ylab = "Density",
     main = "")
curve(dchisq(x, df = 3),
      col = "red",
      type = "l",
      lwd = 2,
      add = T)

set.seed(1)

N <- 30000
Y <- sample(0:1, N, replace = T)
S <- cumsum(Y)
R <- S/(1:N)
plot(R, 
     ylim = c(0.3, 0.7), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     xlab = "n", 
     ylab = "R_n",
     main = "Converging Share of Heads in Repeated Coin Tossing")
lines(c(0,N),
      c(0.5,0.5),
      col = "darkred",
      lty = 2,
      lwd = 2)


par(mfrow = c(2,2))

reps <- 10000
sample.sizes <- c(5,20,75,100)
set.seed(123)

for (n in sample.sizes) {
  
  samplemean <- rep(0, reps) #initialize the vector of sample means
  stdsamplemean <- rep(0, reps) #initialize the vector of standardized sample means
  
  for (i in 1:reps) {
    x <- rbinom(n, 1, 0.5)
    samplemean[i] <- mean(x)
    stdsamplemean[i] <- sqrt(n)*(mean(x) - 0.5)/0.5
  }
  
  hist(stdsamplemean, 
       col = "steelblue", 
       freq = FALSE, 
       breaks = 40,
       xlim = c(-3, 3), 
       ylim = c(0, 0.8), 
       xlab = paste("n =", n), 
       main = "")
  
  curve(dnorm(x), 
        lwd = 2, 
        col = "darkred", 
        add = TRUE)
}  

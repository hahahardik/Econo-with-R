# set seed for reproducibility
set.seed(1)

# set repetitions and the vector of sample sizes
reps <- 1000
n <- c(100, 250, 1000, 3000)

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# divide the plot panel in a 2-by-2 array
par(mfrow = c(2, 2))

# loop sampling and plotting

# outer loop over n
for (j in 1:length(n)) {
  
  # inner loop: sampling and estimating of the coefficients
  for (i in 1:reps){
    
    sample <- population[sample(1:N, n[j]), ]
    fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
    
  }
  
  # draw density estimates
  plot(density(fit[ ,2]), xlim=c(2.5, 4.5), 
       col = j, 
       main = paste("n=", n[j]), 
       xlab = bquote(hat(beta)[1]))
  
}

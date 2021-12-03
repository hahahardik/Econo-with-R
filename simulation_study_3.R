# load the MASS package
library(MASS)

# set seed for reproducibility
set.seed(4)

# simulate bivariate normal data
bvndata <- mvrnorm(100, 
                   mu = c(5, 5), 
                   Sigma = cbind(c(5, 4), c(4, 5))) 

# assign column names / convert to data.frame
colnames(bvndata) <- c("X", "Y")
bvndata <- as.data.frame(bvndata)

# subset the data
set1 <- subset(bvndata, abs(mean(X) - X) > 1)
set2 <- subset(bvndata, abs(mean(X) - X) <= 1)

par(mfrow = c(2, 1))

# plot both data sets
plot(set1, 
     xlab = "X", 
     ylab = "Y", 
     pch = 19)

points(set2, 
       col = "steelblue", 
       pch = 19)

# estimate both regression lines
lm.set1 <- lm(Y ~ X, data = set1)

lm.set2 <- lm(Y ~ X, data = set2)

# plot observations
plot(set1, xlab = "X", ylab = "Y", pch = 19)
points(set2, col = "steelblue", pch = 19)

# add both lines to the plot
abline(lm.set1, col = "green")
abline(lm.set2, col = "red")

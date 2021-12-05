#----------Ex. 1---------------#

# class size vector
cs <- c(23,19,30,22,23,29,35,36,33,25)

# test score
ts <- c(430,430,333,410,390,377,325,310,328,375)

# plot the graph
plot(ts,cs)

#----------Ex. 2---------------#

# mean, sample variance, sample sd
mean(ts)
var(ts)
sd(ts)

# covariance and correlation coefficient
cov(ts, cs)
cor(ts, cs)

#-----------------------Ex. 3-------------------------------#

library(AER)

# Simple regression model
mod <- lm(ts~cs)

# model summary
summary(mod)

#--------------------Ex. 4----------------------------------#

# Basic details of the model

class(mod)
is.list(mod)
names(mod)
mod$call

#--------------------Ex. 5----------------------------------#

# Plotting the regrssion line
plot(cs, ts, 
     xlab = "Class Score",
     ylab = "Test Score",
     main = "Simple Regression Model")
abline(lm(mod),col='red')

#--------------------Ex. 6----------------------------------#

# assign the model summary to the variable `s`
s <- summary(mod)

# check names of entries in `s`
names(s)

# save the R^2 of the regression to the variable `R2`
R2 <- s$r.squared

#--------------------Ex. 7----------------------------------#

# save the coefficient matrix to `coefs`
coefs <- s$coefficients


#-----------------------Ex. 8------------------------------#

# Dropping the intercept
mod_ni <- lm(ts ~ 0 + cs)
summary(mod_ni)

#-----------------------Ex. 9------------------------------#

# Accessing the coefficients
coef <- summary(mod_ni)$coefficients

#-----------------------Ex. 10------------------------------#

# print the contents of `coef` to the console
print(coef)

# compute the t-statistic manually and assign it to `t_stat`
t_stat <- coef[1,1]/coef[1,2]

#-----------------------Ex. 11------------------------------#

# plot the regression lines of both models
plot(cs, ts, ylim = c(0,500), xlim = c(0,40))

# Model with intercept
abline(mod,
       col = "red")

# Model without intercept
abline(mod_ni,
       col = "blue")

#-----------------------Ex. 12------------------------------#

# Compute SSR and TSS

ssr <- sum(mod$residuals^2)
tss <- sum((ts - mean(ts))^2)

#-----------------------Ex. 13------------------------------#

# compute R^2, round to four decimal places
R2 <- round(1 - (ssr/tss), digits = 4)

# check whether your result is correct using the "==" operator
summary(mod)$r.squared == R2

#-----------------------Ex. 14------------------------------#

# obtain the SER using `summary()` and save the value to `SER`
SER <- summary(mod)$sigma

# compute the SSR and save it to `SSR`
SSR <- SER^2*(length(ts)-2)

# do the comparison
SSR == sum(mod$residuals^2)

#-----------------------Ex. 15------------------------------#

# obtain the matrix and save it to `cov_matrix`
cov_matrix <- summary(mod)$cov.unscaled

# compute the standard errors and assign them to the vector `SEs`
SEs <- sqrt(diag(cov_matrix))

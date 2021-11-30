# Create sample data
STR <- c(15, 17, 19, 20, 22, 23.5, 25)
TestScore <- c(680, 640, 670, 660, 630, 660, 635)

# Initial scatterplot of the data
plot(STR, TestScore)

# Relationship with the plot
abline(a = 713, b = -3)

#------------------------------------------------------------#

#---------Estimating the coefficients the manual way-------------------#

# library: "AER"
# dataset: "CASchools"

class(CASchools)

# Calculating the Student-teacher Ratio
CASchools$STR <- CASchools$students / CASchools$teachers

# Compute test score
CASchools$Score <- (CASchools$read + CASchools$math)/2

# Sample average and standard deviation of STR and score
avg_STR <- mean(CASchools$STR)
avg_score <- mean(CASchools$Score)
sd_STR <- sd(CASchools$STR)
sd_score <- sd(CASchools$Score)


# Setting up a vector of percentiles and computing the quantiles
quantiles <- c(0.10, 0.25, 0.4, 0.50, 0.60, 0.75, 0.90)
quant_STR <- quantile(CASchools$STR, quantiles)
quant_score <- quantile(CASchools$Score, quantiles)

# Gathering everything in a Data Frame
DistributionSummary <- data.frame(Average = c(avg_STR, avg_score),
                                  StandardDeviation  = c(sd_STR, sd_score),
                                  quantile = rbind(quant_STR, quant_score))
# A bit modified plot
plot(Score ~ STR,
     data = CASchools,
     main = "Scatterplot of Test Scores and STR",
     xlab = "STR (X)",
     ylab = "Test Score (Y)")


# Checking for Correlation between Score and STR
cor(CASchools$STR, CASchools$Score)

# allows to use the variables contained in CASchools directly
attach(CASchools)

# compute beta_1_hat
beta_1 <- sum((STR - mean(STR)) * (Score - mean(Score))) / sum((STR - mean(STR))^2)

# compute beta_0_hat
beta_0 <- mean(Score) - beta_1 * mean(STR)

#---------------------Linear Model------------------------#
linear_model <- lm(Score ~ STR, data = CASchools)
linear_model

# A bit more modified plot
plot(Score ~ STR,
     data = CASchools,
     main = "Scatterplot of Test Scores and STR",
     xlab = "STR (X)",
     ylab = "Test Score (Y)",
     xlim = c(10,30),
     ylim = c(600, 720))

# Regression Line
abline(linear_model)

# Measurement of Fit
model_summary <- summary(linear_model)

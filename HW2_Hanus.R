# Homework 2
# Simulation of Bierens and Ginther (2001) tables 1.A and 2.A
# Nichole Hanus
# Due 3/31/15

# Set a directory for saving my code
setwd("~/19-704/Homeworks/Homework 2")

# Install required libraries and packages
install.packages("AER")
library(AER)
install.packages("quantreg")
library(quantreg)
library(MASS)
library(plyr)

# Read in data
data(CPS1988) 

# Make a copy of the data 
HW2 <- CPS1988

# Shorten some of the column names and append them to the dataframe
# education becomes "ed"
# experience becomes "exper"
HW2$ed <- HW2$education
HW2$exper <- HW2$experience

# Calculate the exper^2, exper^3, and exper^4 
exper_2 <- HW2$exper^2
exper_3 <- HW2$exper^3
exper_4 <- HW2$exper^4

# Transform the "wage" variable
log_wage <- log(HW2$wage)

# Append the new variables to the dataframe, HW
HW2 <- cbind(HW2,exper_2,exper_3,exper_4,log_wage)

# Perform the LAD for the standard Mincer type model and output the summary
LAD_Mincer <- rq(HW2$log_wage ~ HW2$ethnicity + HW2$ed + HW2$exper + HW2$exper_2)
summary(LAD_Mincer)

# Perform the LAD for the quartic model and output the summary
LAD_quartic <- rq(HW2$log_wage ~ HW2$ethnicity + HW2$ed + HW2$exper + HW2$exper_2 + HW2$exper_3 + HW2$exper_4)
summary(LAD_quartic)

# Begin the story

# Question 1: Create histograms of wages, log wages, education, and experience

# Create a .png file that will contain everything between png() and dev.off(), for the histograms
png(filename = "HW2_Q1_Histograms.png",
    width = 3000, # adjust width of .png file, in pixels
    height = 3000, # adjust height of .png file, in pixels
    res = 300) # set resolution in pixels per ince

# Parameters for the histograms
par(mfrow = c(2, 2), # mfrow sets the number of rows and columns
    cex = 1.3, # cex scales text and symbols
    mar = c(4, 3, 1, 0), # mar and oma set the inner and outer margins
    oma = c(0, 0, 0, 0))

# Plot the histograms
hist(HW2$wage, 
     xlim = c(0, 20000), # the x axis ranges from 0 to 20,000
     ylim = c(0, 2000), # the y axis ranges from 0 to 28155
     main = "Wage", # The main title is "Wages"
     breaks = "FD", # Use the "bins" vector to determine the x axis bins
     xlab = "")


hist(HW2$log_wage, xlim = c(0,20), ylim = c(0,1500), main = "Log Wage", breaks = "FD", xlab = "")


hist(HW2$ed, xlim = c(0,20), ylim = c(0, 15000), main = "Education", breaks = "FD", xlab = "")


hist(HW2$exper, xlim = c(-5,70), ylim = c(0, 1500), main = "Experience", breaks = "FD", xlab = "")


dev.off()


# Question 2: Scatter plots of wages and log wages against education and experience
# Create a .png file for the scatterplots
png(filename = "HW2_Q2_Scatterplots.png", width = 3000, height = 3000, res = 300) 

# Parameters for the scatterplots
par(mfrow = c(2,2), cex = 1.3, oma = c(0,0,0,0))

# Scatterplot of education vs wages
plot(jitter(HW2$ed),
     jitter(HW2$wage),
     main = "Wages and Education", 
     xlab = "Education", 
     ylab = "Wages",
     #xlim = c(0, 20),
     #ylim = c(0, 20000),    
     pch  = 19,
     cex  = .1,
     col  = rgb(0, 0, 0, 0.7))

# Plot fitted linear and median regression lines
Ed_vs_wage.reg1 <- lm(HW2$wage ~ HW2$ed)
abline(Ed_vs_wage.reg1, col = "red")

Ed_vs_wage.reg2 <- rq(HW2$wage ~ HW2$ed)
abline(Ed_vs_wage.reg2, col = "blue")

# Scatterplot of experience vs wages
plot(jitter(HW2$exper),
     jitter(HW2$wage),
     main = "Wages and Experience", 
     xlab = "Experience", 
     ylab = "Wages",
     #xlim = c(0, 20),
     #ylim = c(0, 20000),    
     pch  = 19,
     cex  = .1,
     col  = rgb(0, 0, 0, 0.7))

# Plot fitted linear and median regression line
Exper_vs_wage.reg1 <- lm(HW2$wage ~ HW2$exper)
abline(Exper_vs_wage.reg1, col = "red")

Exper_vs_wage.reg2 <- rq(HW2$wage ~ HW2$exper)
abline(Exper_vs_wage.reg2, col = "blue")

# Scatterplot of education vs log wages
plot(jitter(HW2$ed),
     jitter(HW2$log_wage),
     main ="Log Wages and Education", 
     xlab ="Education", 
     ylab ="Log Wages", 
     #xlim = c(0, 20),
    # ylim = c(0, 10),
     pch  = 19,
     cex  = .1,
     col  = rgb(0, 0, 0, 0.7))

# Plot fitted linear and median regression lines
Ed_vs_logwage.reg1 <- lm(HW2$log_wage ~ HW2$ed)
abline(Ed_vs_logwage.reg1, col = "red")

Ed_vs_logwage.reg2 <- rq(HW2$log_wage ~ HW2$ed)
abline(Ed_vs_logwage.reg2, col = "blue")


# Scatterplot of experience vs log wages
plot(jitter(HW2$exper),
     jitter(HW2$log_wage),
     main = "Log Wages and Experience", 
     xlab = "Experience", 
     ylab = "Log Wages",
     #xlim = c(0, 20),
     #ylim = c(0, 20000),    
     pch  = 19,
     cex  = .1,
     col  = rgb(0, 0, 0, 0.7))

# Plot fitted linear and median regression lines
Exper_vs_logwage.reg1 <- lm(HW2$log_wage ~ HW2$exper)
abline(Exper_vs_logwage.reg1, col = "red")

Exper_vs_logwage.reg2 <- rq(HW2$log_wage ~ HW2$exper)
abline(Exper_vs_logwage.reg2, col = "blue")

dev.off()

# Question 3: Compare wages and log wages to the normal dist

# Simulate a normal random variable (RV) using rnorm(x, y, z)
# The number of observations to draw is x (same for wage and log_wage)
# The mean of the normal RV is y
# The standard deviation of the normal RV is z
x          <- length(HW2$wage)
y_wage     <- mean(HW2$wage)
y_log_wage <- mean(HW2$log_wage)
z_wage     <- sd(HW2$wage)
z_log_wage <- sd(HW2$log_wage)

set.seed(1) # Set seed for the RV number generator, to reproduce results
norm_wage     <- rnorm(x, y_wage, z_wage) # Generate normal RV to compare against wage
norm_log_wage <- rnorm(x, y_log_wage, z_log_wage) # Generate normal RV to compare against log_wage

# Create a .png file for the normal plots
png(filename = "HW2_Q3_Normalplots.png", width = 3000, height = 3000, res = 300) 
# Parameters for the normal plots
par(mfrow = c(2,1), cex = 1.3, oma = c(0,0,0,0))

# Sort the wage and log wages
# The floor and ceiling function find the integer that is not greater
# (or less than) any element of a vector
min.d <- floor(min(HW2$wage, norm_wage)) # Find the smallest value
max.d <- ceiling(max(HW2$wage, norm_wage)) # Find the largst value

min.d_log <- floor(min(HW2$log_wage, norm_log_wage)) 
max.d_log <- ceiling(max(HW2$log_wage, norm_log_wage)) 

# Plot the sorted wage values against nomral draws
plot(sort(jitter(norm_wage)),
     sort(jitter(HW2$wage)),
     xlim = c(min.d, max.d),
     ylim = c(min.d, max.d),
     main = "Wage Values Compared to Normal Dist",
     ylab = "Sorted Wage Values",
     xlab = "Sorted Draws from a Normal Distribution",
     pch  = 19,
     col  = rgb(0, 0, 0, .7))
abline(0, 1) # Draw a line on the plot intercept 0 and slope 1

# Plot the sorted log wage values against normal draws
plot(sort(jitter(norm_log_wage)),
     sort(jitter(HW2$log_wage)),
     xlim = c(min.d_log, max.d_log),
     ylim = c(min.d_log, max.d_log),
     main = "Log Wage Values Compared to Normal Dist",
     ylab = "Sorted Log Wage Values",
     xlab = "Sorted Draws from a Normal Distribution",
     pch  = 19,
     col  = rgb(0, 0, 0, .7))
abline(0, 1) # Draw a line on the plot intercept 0 and slope 1

dev.off()

# Question 4: Plot the regression residuals for the regressions in Table 1.A
# and 2.A, using ordinary linear regression rather than median regression.

# First need to construct linear regressions presented in Table 1.A and 2.A
Table1A.reg <- lm(HW2$log_wage ~ HW2$ethnicity + HW2$ed + HW2$exper + HW2$exper_2)
Table2A.reg <- lm(HW2$log_wage ~ HW2$ethnicity + HW2$ed + HW2$exper + HW2$exper_2 + HW2$exper_3 + HW2$exper_4)

# Make predictions for all the regressors from the linear regression equations 
Table1A.predictors  <- data.frame(Ethnicity    = HW2$ethnicity,
                                  Education    = HW2$ed,
                                  Experience   = HW2$exper,
                                  Experience_2 = HW2$exper_2)
Table2A.predictors  <- data.frame(Ethnicity    = HW2$ethnicity,
                                  Education    = HW2$ed,
                                  Experience   = HW2$exper,
                                  Experience_2 = HW2$exper_2,
                                  Experience_3 = HW2$exper_3,
                                  Experience_4 = HW2$exper_4)

Table1A.predictions <- predict(Table1A.reg, Table1A.predictors)
Table2A.predictions <- predict(Table2A.reg, Table2A.predictors)

# Calculate residuals
Table1A.residuals = HW2$log_wage - Table1A.predictions
Table2A.residuals = HW2$log_wage - Table2A.predictions

# Plot the residuals on a scatter plot
# Create a .png file for the residual plots
png(filename = "HW2_Q4_Residuals_Scatter.png", width = 3000, height = 3000, res = 300) 
# Parameters for the residual plots
par(mfrow = c(1,2), cex = 1.3, oma = c(0,0,0,0))

plot(Table1A.residuals,
     pch = 19,
     col = rgb(0, 0, 0, .7))

plot(Table2A.residuals,
     pch = 19,
     col = rgb(0, 0, 0, .7))

dev.off()

# qqplot of the regression residuals from the real regression
install.packages("car", repos = "http://lib.stat.cmu.edu/R/CRAN/")
library(car)

# Create a .png file for the residual plots
png(filename = "HW2_Q4_Residuals_qqPlot.png", width = 3000, height = 3000, res = 300) 
# Parameters for the residual QQ plots
par(mfrow = c(2,1), cex = 1.3, oma = c(0,0,0,0))

# Plot for Table1A
# id.n = 3 identifies the three largest residuals
qqPlot(Table1A.reg,
       main = "Observed Data - Table1A - Linear Regression",
       id.n = 3, 
       pch  = 19,
       col  = rgb(0, 0, 0, .5))
abline(0, 1)

# Plot for Table2A
# id.n = 3 identifies the three largest residuals
qqPlot(Table2A.reg,
       main = "Observed Data - Table2A - Linear Regression",
       id.n = 3,
       pch  = 19,
       col  = rgb(0, 0, 0, .5))
abline(0, 1)

dev.off()

# Review the diagnostic plots of the residuals
# Use the influence plot for Table1A and Table2A
# Create png for Table1A
png(filename = "HW2_Q4_Residual_Table1A_Diagnostic.png", width = 3000, height = 3000, res = 300) 
# Parameters 
par(cex = 1.3, oma = c(0,0,0,0))
# Influence index plot and identify the largest three outliers
influenceIndexPlot(Table1A.reg, id.n = 3, main = "Table 1A Residual Diagnostics")
dev.off()

# Create png for Table2A
png(filename = "HW2_Q4_Residual_Table2A_Diagnostic.png", width = 3000, height = 3000, res = 300) 
# Parameters 
par(cex = 1.3, oma = c(0,0,0,0))
# Influence index plot and identify the largest three outliers
influenceIndexPlot(Table2A.reg, id.n = 3, main = "Table 2A Residual Diagnostics")
dev.off()

# Question 5: Evaluate the backcasting quality of the Mincer type model
# Use cross-validation to test whether this model is too complex

# Create a .png file for the backcasting plots
png(filename = "HW2_Q5_Backcasting.png", width = 3000, height = 3000, res = 300) 

# Parameters for the residual plots
par(cex = 1.3)

# Plot the predicted log_wage scores versus the observed log_wage scores
# Looking at the linear model
plot(Table1A.predictions, # Plot linear predictions
     HW2$log_wage, # Plot actual log_wage values
     ylab = "Actual Log_Wage",
     xlab = "Predicted Mincer Log_Wage",
     pch = 19,
     col = rgb(0, 0, 0, .3))
abline(0, 1)
dev.off()

# Perform cross-validation
# Sample 2/3 of the cell ids from the log_wage data w/out replacement
# This is our training data

# First, assign cell ids to HW2 dataframe
cell_id <- c(1:length(HW2$education))

# Append cell ids to HW2
HW2 <- cbind(HW2, cell_id)

# Sample from the HW2 dataframe
set.seed(10)
train <- sample(HW2$cell_id, # Sample the TableA1 predictor values
                2*length(HW2$cell_id)/3, # Sample 2/3 of these values
                replace = FALSE) # Sample w/out replacement
# Create "test" data that are the TableA1 predictor values not selected
test <- HW2$cell_id[ - train]

# Run linear regression
# Subset the data to only select rows where the log_wage
# are "in" (%in%) the training data
train1 <- lm(HW2$log_wage ~ HW2$ethnicity + HW2$ed + HW2$exper + HW2$exper_2, 
             data = HW2[HW2$cell_id %in% train, ])
train2 <- lm(HW2$log_wage ~ 1,
             data = HW2[HW2$cell_id %in% train, ])  

# The actual outcome is on the first line
# The regression predictions are on the second line
# Calculate the squared difference for all predictions on the third line
test1 <- (HW2$log_wage[HW2$cell_id %in% test] - 
            predict(train1, HW2[HW2$cell_id %in% test, ]))^2
test2 <- (HW2$log_wage[HW2$cell_id %in% test] -
            predict(train2, HW2[HW2$cell_id %in% test, ]))^2

# Calculate the rMSE for the two regressions
rMSEtest1 <- sqrt(sum(test1) / length(test1))
rMSEtest2 <- sqrt(sum(test2) / length(test2))

# Review the summary statistics
summary(train1)$sigma # Get rMSe from the backcast
summary(train2)$sigma

# Question 8: Bonus
# I decided to look at the CDFs of the median regressions
# calculated in Part 1 against normal distributions

# develop a png file
png(filename = "HW2_Bonus_cdfs.png", width = 3000, height = 3000, res = 300) 

# Parameters for the residual plots
par(mfrow = c(1,2), cex = 1.3)

# Wages cdf
plot(ecdf(HW2$wage),
     xlab = "Wages",
     ylab = "Cumulative Probability",
     col  = "red",
     pch  = 19,
     main = "Empirical (Red) vs. Normal (Blue) CDFs")

plot(ecdf(norm_wage),
     xlab = "Wages",
     ylab = "Cumulative Probability",
     col  = "blue",
     pch  = 19,
     add  = TRUE)

plot(ecdf(HW2$log_wage),
     xlab = "Log Wages",
     ylab = "Cumulative Probability",
     col = "red",
     pch = 19,
     main = "Empirical (Red) vs. Normal (Blue) CDFs")

plot(ecdf(norm_log_wage),
     xlab = "Log Wages",
     ylab = "Cumulative Probability",
     col  = "blue",
     pch  = 19,
     add  = TRUE)

dev.off()

# Compare empirical cdfs against the normal distribution with the same mean and sd
ks.test_wage <- ks.test(HW2$wage, norm_wage)
summary(ks.test_wage)

ks.test_log_wage <- ks.test(HW2$log_wage, norm_log_wage)
summary(ks.test_log_wage)

#-----------------------------------------------------------------------------
# X and Y is the variable, and x_i is the number in the vectors.
X <- c(1, 2, 3, 4, 5, 6, 7, 8)
Y <- c(2, 4, 6, 8, 10, 12, 14, 16)

#-----------------------------------------------------------------------------
# Variance Function
s2_function <- function(x){
  n <- length(x)
  s2_var <- sum((x - mean(x))^2) / (n-1)
  return(s2_var)
}
# Covariance Function
cov_custom <- function(X, Y){
  n <- length(X)
  COV_XY <- sum((X - mean(X)) * (Y - mean(Y))) / (n-1)
  return(COV_XY)
}

# Correlation Function
corr_custom <- function(X, Y){
  t_frac <- sum((X - mean(X)) * (Y - mean(Y)))
  b_frac <- sqrt(sum((X - mean(X))^2) * sum((Y - mean(Y))^2))
  corre <- t_frac / b_frac
  return(corre)
}

#-----------------------------------------------------------------------------
#Variance Output
s2_function(X)
var(X)
var(Y)

# Covariance Ouput
cov(X, Y)
cov_custom(X, Y)

# Correlation function
corr_custom(X, Y)
cor(X, Y)

#-----------------------------------------------------------------------------

# Simulated Data Set From ISLR 2
# Create data (this is just dummy data; replace with your real data)
X <- seq(0, 100, length.out=100)
Y <- 3 + 0.05 * X - 0.0005 * X^2 + rnorm(100, 0, 0.5) 

# Plot data
plot(X, Y, ylim=c(2,12), xlim=c(0,100), pch=16, cex=0.6, xlab="X", ylab="Y")

# Plot lines (again, dummy functions, replace with your real functions)
lines(X, 3 + 0.05 * X, col="orange", lwd=2)
lines(X, 3 + 0.05 * X - 0.0005 * X^2, col="blue", lwd=2)
lines(X, 2.8 + 0.058 * X - 0.00052 * X^2 + 0.000002 * X^3, col="green", lwd=2)
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# Create data (this is just dummy data; replace with your real data)
Flexibility <- c(2,5,10,20)
MSE <- c(2, 1.2, 0.8, 1.8)

# Plot
plot(Flexibility, MSE, ylim=c(0,2.5), xlim=c(2,20), pch=15, cex=1.5, xlab="Flexibility", ylab="Mean Squared Error", type="n")

# Add Lines
curve(2 - 0.2*x + 0.015*x^2, from=2, to=20, col="red", add=TRUE, lwd=2)
curve(2 - 0.3*x, from=2, to=20, col="blue", add=TRUE, lwd=2)

# Add points
points(Flexibility, MSE, pch=15, col="green", cex=1.5)

# Add dashed line
abline(h=1, lty=2)
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# Simulating data to mimic the structure (you'd use actual data here)
X <- seq(0, 100, length.out=100)
Y <- 2 + 0.1 * X - 0.001 * X^2 + rnorm(100, 0, 1)

# Plot data
plot(X, Y, ylim=c(2,12), xlim=c(0,100), pch=1, xlab="X", ylab="Y")

# Plot lines 
lines(X, 2 + 0.1 * X, col="orange", lwd=2)
lines(X, 2 + 0.1 * X - 0.001 * X^2, col="blue", lwd=2)
lines(X, 3 + 0.08 * X - 0.0012 * X^2, col="green", lwd=2)
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

# Simulating data to mimic the structure (again, use your actual data)
Flexibility <- c(2,5,10,20)
MSE <- c(2, 1.2, 0.8, 1.8)

# Plot 
plot(Flexibility, MSE, ylim=c(0,2.5), xlim=c(2,20), pch=15, cex=1.5, xlab="Flexibility", ylab="Mean Squared Error", type="n")

# Lines
curve(2 - 0.2*x + 0.015*x^2, from=2, to=20, col="red", add=TRUE, lwd=2)
curve(1.5 - 0.3*x + 0.01*x^2, from=2, to=20, col="blue", add=TRUE, lwd=2)

# Rectangles (dummy positions; adjust as necessary)
rect(4.5, 1.1, 5.5, 1.3, border="orange", col="orange")
rect(9.5, 0.7, 10.5, 0.9, border="orange", col="orange")

# Dashed line
abline(h=1, lty=2)

#-----------------------------------------------------------------------------
### BIAS-VARIANCE DECOMPOSITION
# Simulating data
set.seed(123)  # For reproducibility
n <- 100
x <- seq(0, 1, length.out=n)
true_f <- function(x) { 2*x^2 }
y_true <- true_f(x)
y_observed <- y_true + rnorm(n, 0, 0.5)

# Fit a simple linear model
model <- lm(y_observed ~ x)

# Predictions from the model
y_hat <- predict(model, newdata = data.frame(x=x))

# Calculate components
error <- y_observed - y_hat
squared_bias <- (y_true - y_hat)^2
variance <- var(y_hat)

# Bias-Variance Decomposition
expected_squared_error <- mean((y_observed - y_hat)^2)
calculated_variance <- variance
calculated_bias_squared <- mean(squared_bias)
error_variance <- var(error)

# Print results
cat("Expected Squared Error: ", expected_squared_error, "\n")
cat("Variance: ", calculated_variance, "\n")
cat("Bias^2: ", calculated_bias_squared, "\n")
cat("Error Variance: ", error_variance, "\n")

#-----------------------------------------------------------------------------
### BIAS-VARIANCE DECOMPOSITION PLOTTING TEST
# Simulating data
set.seed(123)  # For reproducibility
n <- 100
x <- seq(0, 1, length.out=n)
true_f <- function(x) { 2*x^2 }
y_true <- true_f(x)
y_observed <- y_true + rnorm(n, 0, 0.5)

# Fit a simple linear model
model <- lm(y_observed ~ x)

# Predictions from the model
y_hat <- predict(model, newdata = data.frame(x=x))

# Calculate components
bias <- y_true - y_hat
squared_bias <- bias^2

# Plotting
plot(x, y_observed, pch=16, col="blue", ylim=c(min(y_observed), max(y_true)), main="Bias-Variance Decomposition", xlab="X", ylab="Y", frame.plot=FALSE)
lines(x, y_true, col="darkgreen", lwd=2)
lines(x, y_hat, col="red", lwd=2)

# Adding legend
legend("topright", legend=c("Observed Data", "True Function", "Model Predictions"), 
       col=c("blue", "darkgreen", "red"), lty=c(0,1,1), lwd=c(2,2,2), pch=c(16, NA, NA))

# Highlighting bias at a few points for clarity
sample_indices <- sample(1:n, 10)  # randomly select 10 points
for (i in sample_indices) {
  lines(c(x[i], x[i]), c(y_hat[i], y_true[i]), col="purple", lty=2)
}
points(x[sample_indices], y_true[sample_indices], pch=16, col="darkgreen")
points(x[sample_indices], y_hat[sample_indices], pch=16, col="red")

# Adding additional legend for bias
legend("top", legend="Bias", col="purple", lty=2, lwd=2, cex=0.8)

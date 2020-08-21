# Create 3 random Normals with mean 20 and st dev 5
v1 <- rnorm(95412, mean = 20, sd = 5)
v2 <- rnorm(95412, mean = 20, sd = 5)
v3 <- rnorm(95412, mean = 20, sd = 5)

# Compute the target as a weighted sum as shown
y <- 0.5 * v1 + 0.2 * v2 + 0.3 * v3

# Combine into a data frame
my.df <- data.frame(y, v1, v2, v3)

# Let's look at the scatter plot matrix
pairs(my.df) # NOTE: takes awhile

# Now let's build a linear regression model and look at it
myReg <- lm(y ~ v1 + v2 + v3)
summary(myReg)

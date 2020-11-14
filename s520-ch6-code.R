# Median of the standard normal
qnorm(0.5)
# Find the IQR
qnorm(0.75) - qnorm(0.25)
# Let X be Normal(0, 10^2)
# Find the IQR
qnorm(0.75, mean=0, sd=10) - qnorm(0.25, mean=0, sd=10)

# Let Z be std normal
# Let Y = Z^2
# What does the pdf of Y look like?
z = rnorm(100000)
y = z^2
hist(y, prob=TRUE)
mean(y)

# What's the IQR of Y?
# Find quantiles:
# P(Y <= q) = a
# P(Z^2 <= q) = a
# P(-sqrt(q) < Z < sqrt(q)) = a
# P(-sqrt(q) < Z < 0) + P(0 < Z < sqrt(q)) = a
# P(0 < Z < sqrt(q)) = a/2
# P(Z < sqrt(q)) = 0.5 + a/2
# sqrt(q) = qnorm(0.5 + a/2)
# q = qnorm(0.5 + a/2)^2
qnorm(0.5 + 0.25/2)^2
qnorm(0.5 + 0.75/2)^2
qnorm(0.5 + 0.75/2)^2 - qnorm(0.5 + 0.25/2)^2
# Check:
summary(y)
# What does this summary mean? Ch. 7


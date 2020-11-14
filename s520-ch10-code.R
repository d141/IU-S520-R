### Why do we need t?

# Write a function to simulate t

t.statistic = function(n, mu = 0, sigma = 1) {
    my.sample = rnorm(n, mu, sigma)
    x.bar = mean(my.sample)
    s = sd(my.sample)
    t = (mean(my.sample) - mu)/(s/sqrt(n))
    return(t)
}

# Simulate 10,000 t-statistics

t.list = replicate(10000, t.statistic(n = 6))

# Does the t-statistic follow the normal

qqnorm(t.list)
# not quite

### Compare normal and t graphically

curve(dnorm, from=-4, to=4)
abline(h = 0)
curve(dt(x, df=5), col="red",
      add=TRUE)
curve(dt(x, df=10), col="blue",
      add=TRUE)
curve(dt(x, df=50), col="green", add=TRUE)
# Gets closer to normal as df increases

# Compare normal and t numerically

pnorm(-1.75)
pt(-1.75, df=5)
pt(-1.75, df=10)
pt(-1.75, df=50)

### Example: Sample of 2009 country club fees
# 2008 average fee was $31912
# Test to see if fees have changed
fees = c(29121, 31472, 28054, 31005, 36295, 32771,
         26205, 33299, 25602, 33726, 39731, 27816)
# Are they normal?
hist(fees)
qqnorm(fees)
# Leap of faith: Assume normal population
mean(fees)
sd(fees)
t = (mean(fees) - 31912) /
  (sd(fees) / sqrt(12))
# Two-tailed P-value
2 * pt(t, df=11)
# or
2 * (1 - pt(abs(t), df=11))
# Find 95% CI
mean(fees) - qt(.975, df=11) * sd(fees)/sqrt(12)
mean(fees) + qt(.975, df=11) * sd(fees)/sqrt(12)
# If you don't like the leap of faith, get more data

### Example: 1920 city sizes

# Get data
library(boot)
pop1920 = bigcity$u
# Draw graphs
plot(density(pop1920))
qqnorm(pop1920)
# Try a log transformation
log1920 = log(pop1920)
plot(density(log1920))
qqnorm(log1920)
# Better but still a little skewed

# Option 1: Proceed with t-interval on the logged data
n = length(log1920)
x.bar = mean(log1920)
s = sd(log1920)
# Lower bound
lower = x.bar - qt(0.975, df = n - 1) * s/sqrt(n)
# Upper bound
upper = x.bar + qt(0.975, df = n - 1) * s/sqrt(n)
c(lower, upper)
# Back-transform
exp(c(lower, upper))

# Option 2: Use the sign test
# If you've never done this:
install.packages("BSDA")
# Load BSDA package
library(BSDA)
SIGN.test(pop1920)
# I prefer Option 2 but it requires learning more stats

### Paired data

# Do ergonomic keyboards increase typing
# speed over standard ones?
ergonomic = c(69, 80, 60, 71, 73, 64, 63, 70, 63, 74)
standard = c(70, 68, 54, 56, 58, 64, 62, 51, 64, 53)
data.frame(ergonomic, standard)

# Calculate the differences
differences = ergonomic - standard
differences
summary(differences)
boxplot(differences, ylab = "Ergonomic speed - standard speed (wpm)",
  main = "Typing speeds on two keyboards")
qqnorm(differences, main = "Normal QQ plot of typing speed differences")

# Option 1: t-test
n = length(differences)
x.bar = mean(differences)
s = sd(differences)
t.statistic = (x.bar - 0)/(s/sqrt(n))
# P-value
2 * (1 - pt(abs(t.statistic), df = n - 1))

# Option 2: Wilcox signed rank test
# If you've never installed "coin":
install.packages("coin")
# Load coin package
library(coin)
# Do test
wilcoxsign_test(ergonomic ~ standard, distribution = "exact")

# Option 3: 
library(BSDA)
SIGN.test(differences)

# Option 1 and 2 are justifiable
# Option 3 has its assumptions met, but lacks power


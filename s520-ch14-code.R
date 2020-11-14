### Bivariate normal data

# Trosset's bivariate normal functions
source("binorm.R")
# or just copy-paste if you don't know the path

# No association:
x = rnorm(1000)
y = rnorm(1000)
plot(x, y)
binorm.scatter(cbind(x, y))
cor(x, y)

# Strong positive
# linear association
x = rnorm(1000)
y = x + rnorm(1000)
binorm.scatter(cbind(x, y))
cor(x, y)

# Strong negative linear association
x = rnorm(1000)
y = - x + rnorm(1000)
binorm.scatter(cbind(x, y))
cor(x, y)

# Weak positive linear association
x = rnorm(1000)
y = x/4 + rnorm(1000)
binorm.scatter(cbind(x, y))
cor(x, y)

# Weak negative linear association
x = rnorm(1000)
y = - x/4 + rnorm(1000)
binorm.scatter(cbind(x, y))
cor(x, y)

# Nonlinear association: NOT bivariate normal
x = rnorm(1000)
y = x^2 + rnorm(1000)
binorm.scatter(cbind(x, y))
# Ellipse does a bad job

# Real data: Checking bivariate normality
anorexia = read.table("anorexia.txt",
                      header=TRUE)
# Pick out the data for the standard treatment
Standard = anorexia[anorexia$Treatment=="Standard",]
Before = Standard$Before
After = Standard$After
Diffs = After - Before
# Consider Before, Diffs
# Is Before approx. normal?
qqnorm(Before)
# Is Diffs approx. normal?
qqnorm(Diffs)
# Is the data bivariate normal?
plot(Before, Diffs)
binorm.scatter(cbind(Before, Diffs))
cor(Before, Diffs)

# What is correlation?
# Change x to standard units:
mean.before = mean(Before)
sd.before = sd(Before)
z.before = (Before - mean.before) / sd.before
# Change y to standard units:
mean.diff = mean(Diffs)
sd.diff = sd(Diffs)
z.diff = (Diffs - mean.diff) / sd.diff
# Find the average of the products
n = length(Before)
sum(z.before * z.diff) / (n - 1)
# Same as correlation

# Correlation is a measure of 
# linear association

x = rnorm(1000)
y = -0.5 * x + rnorm(1000)
plot(x, y)
cor(x, y)
# Correlation does a good job
# of measuring the relationship

# Lognormal: The logs of the data come from a normal
x = rlnorm(100)
y = rlnorm(100)
plot(x, y)
cor(x, y)
# Correlation does an okay job
# of measuring the relationship

# Nonlinear data
x = rnorm(1000)
y = x^3
plot(x, y)
cor(x, y)
cor(x, y, method="kendall")
cor(x, y, method="spearman")


x = rnorm(1000)
y = x^2
plot(x, y)
cor(x, y)
# Correlation does a bad job
# of measuring the relationship
# Note: There are other kinds of correlation
# but they fail as well
cor(x, y, method="kendall")
cor(x, y, method="spearman")

# Back to anorexia data:
plot(Before, Diffs)
# Lighter girls gain weight
# Heavier girls lose weight
# What's the explanation?
summary(Before)
summary(After)
plot(Before, After)
cor(Before, After)
# Test null hypothesis of zero correlation
cor.test(Before, After)
# The relationship between weight before
# and weight after is weak at best
# It doesn't matter much whether you were
# light or heavy before
plot(density(After))
# You'll mostly likely end up around
# the mean after
# This is a special case of
# "REGRESSION TO THE MEAN"
# or "THE REGRESSION EFFECT"
# or just "REGRESSION"




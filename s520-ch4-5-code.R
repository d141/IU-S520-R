###Ch. 4: Using the binomial

# dbinom and pbinom example
# Flight with 100 seats accepts 105 reservations
# On average, 90% show up
# What's the prob. exactly 100 people show up?
# Assuming independence:
dbinom(100, 105, 0.9)
# What's the prob. at least 1 person 
# doesn't get a seat?
# P(Y > 100) = 1 - P(Y <= 100)
1 - pbinom(100, 105, 0.9)
# Is independence a good assumption?
# If not, get better data


# Binomial combo
# 100 people each toss 10 fair coins
# What's the prob. the majority
# get more heads than tails?
p = 1 - pbinom(5, 10, 0.5)
1 - pbinom(50, 100, p)




### Ch. 5: Continuous random variables

# Simulating a uniform random variable

x = runif(10000)
hist(x)
x = runif(1000000)
hist(x)



# Simulating the sum of 1000 uniforms
y = replicate(10000, sum(runif(1000)))
hist(y)


### The normal distribution

# Standard normal curve

curve(dnorm, -4, 4)
abline(h = 0)
# or specify the x-values
x = seq(-4, 4, 0.01)
f = dnorm(x)
plot(x, f, type="l")
abline(h=0)

# Normal with mean 50 and SD 5

x = seq(30, 70, 0.01)
f = dnorm(x, mean=50, sd=5)
plot(x, f, type="l")
abline(h = 0)

# Compare to simulations of 100 fair coin flips

data = rbinom(100000, 100, 0.5)
hist(data)
hist(data, prob=TRUE)
lines(x, f)



# Using the normal
# Let Z have a std normal distribution
# P(Z <= 1) = P(Z < 1)
pnorm(1)
# P(Z > 1) = P(Z >= 1)
1 - pnorm(1)
# P(-1 < Z < 1) = P(|Z| < 1)
pnorm(1) - pnorm(-1)
# P(-2 < Z < 2)
pnorm(2) - pnorm(-2)
# P(-3 < Z < 3)
pnorm(3) - pnorm(-3)
# P(|Z| > 1) = P(Z < -1) + P(Z > 1)
pnorm(-1) + (1 - pnorm(1))
2 * pnorm(-1)
2 * (1 - pnorm(1))
# P(|Z| > 6)
2 * (1 - pnorm(6))

# Let X be normal with mean 100,
# variance 10^2
# P(90 < X < 110)
pnorm(110, mean=100, sd=10) -
  pnorm(90, mean=100, sd=10)
pnorm(110, 100, 10) - pnorm(90, 100, 10)
# P(80 < X < 120)
pnorm(120, 100, 10) - pnorm(80, 100, 10)
# P(70 < X < 130)
pnorm(130, 100, 10) - pnorm(70, 100, 10)



# Adding/subtracting independent normals
# gives you a new normal RV
x1 = rnorm(10000, mean=10, sd=5)
x2 = rnorm(10000, mean=-20, sd=10)
y = x1 + x2
hist(y)



# The same is NOT true for multiplying,
# squaring, dividing etc.
# e.g
z = rnorm(10000)
chisq1 = z^2
hist(chisq1)
# This one's called a
# Chi-Squared(1) random variable
# Compare
hist(rchisq(10000,1))

z1 = rnorm(10000)
z2 = rnorm(10000)
t1 = z1 / z2
hist(t1)
# That one's called a t(1)

F = t1^2
hist(F)
# That one's is called F(1,1)

# None of this is important right now
# but we'll see all these distributions
# again later




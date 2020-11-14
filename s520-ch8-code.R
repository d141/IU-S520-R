### Simulation example: Dice

# Recall:
# Expected value of one die roll = 3.5
# Variance = 35/12 = 2.917

die.rolls = function(n){
	x = sample(c(1,2,3,4,5,6), size=n, replace=TRUE)
	return(x)
}
x = die.rolls(20) # Roll a die 20 times
x
sum(x)/20 # "Plug-in" estimate of mean
sum((x-mean(x))^2/20) # Plug-in variance

# We already have a function to find the plug-in mean
mean(x)
# Write a function to find plug-in variance
var.plugin = function(x){
	n = length(x)
	v = sum((x-mean(x))^2/n)
	return(v)
}
var.plugin(x)

mean(die.rolls(20))
sum((x-mean(x))^2/20)
# Replicate: Repeat the 20 die rolls 50,000 times
mean.replicated = replicate(50000, mean(die.rolls(20)))
hist(mean.replicated)
summary(mean.replicated)
# On average, we get the right answer
var.plugin.replicated = replicate(50000, var.plugin(die.rolls(20)))
hist(var.plugin.replicated)
summary(var.plugin.replicated)
# On average, we DON’T get the right answer
# How much are we wrong by?
mean(var.plugin.replicated)/(35/12)
# Almost exactly 5%
# Hmmm
# What if we divide by 19 instead of 20?
var.magic = function(x){
	n = length(x)
	v = sum((x-mean(x))^2/(n-1))
	return(v)
}
var.magic.replicated = replicate(50000, var.magic(die.rolls(20)))
summary(var.magic.replicated)
# R uses this "unbiased" version of sample variance
var.magic(x)
var(x)





### Simulation example: Coin tossing

# Write a function
tosser = function(n, p){
  outcomes = c("Heads", "Tails")
  return(sample(outcomes, size=n, replace=TRUE, prob=c(p, 1-p)))
}
# Try out your function
tosser(n=5, p=0.5)
tosser(n=10, p=0.1)
tosser(n=10, p=0.9)

# Now write a function to count heads

headcounter = function(n, p){
  x = tosser(n, p)
  return(sum(x == "Heads"))
}
headcounter(n=100, p=0.5)
headcounter(n=1000, p=0.5)
headcounter(n=10000, p=0.5)
replicate(5, headcounter(n=1000, p=0.5))

# Or just use the built-in function rbinom():

rbinom(5, 1000, 0.5)

# Look at lots of replications

setsof1000tosses = rbinom(90210, 1000, 0.5)
hist(setsof1000tosses, breaks=seq(400, 600, 5))

# Instead of counts, look at proportions

prop.heads = setsof1000tosses / 1000
hist(prop.heads, breaks=seq(0.4, 0.6, 0.005))





### Simulation example: Roulette

# Single-number bet:
# +35 for win (1 chance)
# -1 for loss (37 chances)
# Expected value of one bet: -0.0526
# We write a function to simulate 365 bets on "00"

roulette = function(n){
	prizes = c(35, rep(-1,37))
	x = sample(prizes, size=n, replace=TRUE)
	return(sum(x))
}
roulette(365) # Total winnings
replicate(10, roulette(365))
y = replicate(10000, roulette(365))
head(y)
summary(y)
y.avg = y/365
summary(y.avg)

# Now look at sets of 36,500 bets on red
z = replicate(10000, roulette(36500))
summary(z)
z.avg = z/36500
summary(z.avg)
boxplot(y.avg, z.avg,
  names=c("Sets of 365 spins","Sets of 36,500 spins"),
  ylab="Average winnings per game")
abline(h=-2/38, col="red")
# Rough statement of the Law of Large Numbers:
# The average of any sufficiently large iid sample
# is almost certain to be close to the expected value

hist(y.avg)
plot(density(y.avg))
qqnorm(y.avg)
hist(z.avg)
plot(density(z.avg))
qqnorm(z.avg)
# Rough statement of the Central Limit Theorem:
# The average of any sufficiently large iid sample
# has an approximate normal distribution

### Confidence intervals: Choral singers

singer = read.table("singer.txt", header=TRUE)

# 90% CI for the mean
xbar = mean(singer$height)
q = qnorm(.95)
s = sd(singer$height)
n = length(singer$height)
# Lower bound
xbar - q * s/sqrt(n)
# Upper bound
xbar + q * s/sqrt(n)

# 80% CI for the proportion 6 feet or taller
p.hat = sum(singer$height >= 72) / n
q = qnorm(.90)
# Lower bound
p.hat - q * sqrt(p.hat*(1 - p.hat)/n)
# Upper bound
p.hat + q * sqrt(p.hat*(1 - p.hat)/n)

# Do confidence intervals work? A simulation

p = 0.2
p.hats = rbinom(10000,235,p)/235
lower.bounds = p.hats - q * sqrt(p.hats*(1 - p.hats)/n)
upper.bounds = p.hats + q * sqrt(phats*(1 - p.hats)/n)
summary(lower.bounds)
summary(upper.bounds)
sum(p < lower.bounds) # should be 10% of the time
sum(p > upper.bounds) # should be 10% of the time
# A bit off




## ===========================================================================
## The following codes accompany the lecture notes.



### Sample
salaries = scan("faculty100.txt")
mean(salaries)
sum(salaries >= 100000)

### Population
salaries.all = scan("faculty.txt")
mean(salaries.all)

### Does the Law of Large Numbers hold?
# Take samples of size 1 million
x = sample(salaries.all, size=1e6, replace=TRUE)
mean(x)
mean(sample(salaries.all, size=1e6, replace=TRUE)) # Repeat a few times
# Take samples of size 1 billion
mean(sample(salaries.all, size=1e9, replace=TRUE))
replicate(10, mean(sample(salaries.all, size=1e9, replace=TRUE)))

### Does the Central Limit Theorem hold?
# Take samples of size 1
sample.means.1 = replicate(1000,
  mean(sample(salaries.all, size=1)))
qqnorm(sample.means.1)
# Take samples of size 10
sample.means.10 = replicate(1000,
  mean(sample(salaries.all, size=10)))
qqnorm(sample.means.10)
# Take samples of size 100
sample.means.100 = replicate(1000,
  mean(sample(salaries.all, size=100)))
qqnorm(sample.means.100)

### Confidence intervals
# Number of standard errors for 95% confidence
qnorm(0.975)
# Confidence interval for population mean of faculty salaries
mean(salaries) -
  qnorm(0.975) * sd(salaries) / sqrt(length(salaries))
mean(salaries) +
  qnorm(0.975) * sd(salaries) / sqrt(length(salaries))

### Do confidence intervals work?

# Write an R function to take a random sample and calculate a CI
faculty.interval = function(n){
  salaries.sample = sample(salaries.all, size=n)
  x.bar = mean(salaries.sample)
  s = sd(salaries.sample)
  lower = x.bar - qnorm(0.975) * s / sqrt(n)
  upper = x.bar + qnorm(0.975) * s / sqrt(n)
  return(c(lower, upper))
}

# Do 1000 simulations
simulations = replicate(1000, faculty.interval(100))
lower.list = simulations[1,]
upper.list = simulations[2,]
too.low = sum(upper.list < mean(salaries.all))
too.high = sum(lower.list > mean(salaries.all))
just.right = 1000 - too.low - too.high
print(c(too.low, too.high, just.right))

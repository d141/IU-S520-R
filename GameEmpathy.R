GameEmpathy = read.table("GameEmpathy.txt", header=TRUE)

# Male empathy vs. female empathy
MaleEmpathy = GameEmpathy$empathy[GameEmpathy$sex == "male"]
FemaleEmpathy = GameEmpathy$empathy[GameEmpathy$sex == "female"]
par(mfrow=c(1,1))
plot(density(MaleEmpathy), ylim=c(0,0.5),
  main="Empathy (black = men, red = women)")
lines(density(FemaleEmpathy), col="red")
par(mfrow=c(1,2))
qqnorm(MaleEmpathy, main="Male empathy")
qqnorm(FemaleEmpathy, main="Female empathy")
t.test(FemaleEmpathy, MaleEmpathy)

# Type of game
par(mfrow=c(1,1))
boxplot(empathy ~ game.type, data=GameEmpathy,
  ylab="Empathy (1-7 scale)", main="Empathy by game")
GTA.empathy = GameEmpathy$empathy[GameEmpathy$game.type == "GTA"]
HalfLife.empathy = GameEmpathy$empathy[GameEmpathy$game.type == "HalfLife"]
neutral.empathy = GameEmpathy$empathy[GameEmpathy$game.type == "neutral"]
summary(GTA.empathy)
summary(HalfLife.empathy)
summary(neutral.empathy)
sd(GTA.empathy)
sd(HalfLife.empathy)
sd(neutral.empathy)
par(mfrow=c(1,3))
qqnorm(GTA.empathy, main="GTA", ylab="Empathy")
qqnorm(HalfLife.empathy, main="Half Life", ylab="Empathy")
qqnorm(neutral.empathy, main="Neutral", ylab="Empathy")
anova(lm(empathy ~ game.type, data=GameEmpathy))

# Identification and empathy
GTA.players = GameEmpathy[GameEmpathy$game.type == "GTA",]
HalfLife.players = GameEmpathy[GameEmpathy$game.type == "HalfLife",]
neutral.players = GameEmpathy[GameEmpathy$game.type == "neutral",]
par(mfrow=c(1,3))
plot(GTA.players$identify, GTA.players$empathy,
  main="GTA players", xlab="Idenitification", ylab="Empathy")
plot(HalfLife.players$identify, HalfLife.players$empathy,
  main="Half-Life players", xlab="Idenitification", ylab="Empathy")
plot(neutral.players$identify, neutral.players$empathy,
  main="Neutral game players", xlab="Idenitification", ylab="Empathy")
cor(GTA.players$identify, GTA.players$empathy)
cor(HalfLife.players$identify, HalfLife.players$empathy)
cor(neutral.players$identify, neutral.players$empathy)



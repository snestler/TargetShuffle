# Read in real estate data for regression
myData <- read.csv("2019_MLB_data.csv",header=T,sep=",",quote="\"",dec=".",fill=T)

# Example of a good model (OBP)

myReg <- lm(R ~ OBP, data = myData)
myR2 <- summary(myReg)$r.squared

n <- 1000
CoefDet <- dim(n)
for(i in 1:n){
  tempRuns <- sample(myData$R)
  tempReg <- lm(tempRuns ~ OBP, data = myData)
  CoefDet[i] <- summary(tempReg)$r.squared
}
max(CoefDet)
myR2
length(CoefDet[CoefDet>myR2])
.Last.value / n

# Let's graph a histogram of the R^2 values and see where ours falls.
hist(CoefDet, xlim=c(0,1))
abline(v=myR2, col="red", lty=2)

# Example of a poor model (triples)

myReg <- lm(R ~ X3B, data = myData)
myR2 <- summary(myReg)$r.squared

n <- 1000
CoefDet <- dim(n)
for(i in 1:n){
  tempRuns <- sample(myData$R)
  tempReg <- lm(tempRuns ~ X3B, data = myData)
  CoefDet[i] <- summary(tempReg)$r.squared
}
max(CoefDet)
myR2
length(CoefDet[CoefDet>myR2])

# Let's graph a histogram of the R^2 values and see where ours falls.
hist(CoefDet, xlim=c(0,1))
abline(v=myR2, col="red", lty=2)
.Last.value / n

# Example of a so-so model (doubles)

myReg <- lm(R ~ X2B, data = myData)
myR2 <- summary(myReg)$r.squared

n <- 1000
CoefDet <- dim(n)
for(i in 1:n){
  tempRuns <- sample(myData$R)
  tempReg <- lm(tempRuns ~ X2B, data = myData)
  CoefDet[i] <- summary(tempReg)$r.squared
}
max(CoefDet)
myR2
length(CoefDet[CoefDet>myR2])
.Last.value / n

# Let's graph a histogram of the R^2 values and see where ours falls.
hist(CoefDet, xlim=c(0,1))
abline(v=myR2, col="red", lty=2)

# Example of a so-so model (LOB)

myReg <- lm(R ~ LOB, data = myData)
myR2 <- summary(myReg)$r.squared

n <- 1000
CoefDet <- dim(n)
for(i in 1:n){
  tempRuns <- sample(myData$R)
  tempReg <- lm(tempRuns ~ LOB, data = myData)
  CoefDet[i] <- summary(tempReg)$r.squared
}
max(CoefDet)
myR2
length(CoefDet[CoefDet>myR2])
.Last.value / n

# Let's graph a histogram of the R^2 values and see where ours falls.
hist(CoefDet, xlim=c(0,1))
abline(v=myR2, col="red", lty=2)
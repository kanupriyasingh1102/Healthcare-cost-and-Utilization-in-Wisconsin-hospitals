#Project on Healthcare
#importing file
library(readxl)
hc <- read.csv(file.choose()) 
View(hc)
summary(hc)

#part1


hist(hc$AGE, main = "Histogram for age frequency",
     xlab = "Age Group",
     ylab = "Frequency of Patients",
     prob= TRUE,
     col = "blue")
lines(density(hc$AGE))
summary(as.factor(hc$AGE))
x <- aggregate(TOTCHG~AGE,FUN = sum,data = hc)
x
max(x)


#part2

which.max(summary(as.factor(hc$APRDRG)))

dc <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = hc)
dc
dc[which.max(dc$TOTCHG),]


#part3

str(hc)
summary(as.factor(hc$RACE))
head(hc)
hc<-na.omit(hc)
hc$RACE<-as.factor(hc$RACE)
mod<- aov(TOTCHG ~ RACE, data = hc)
mod
summary(mod)
summary(hc$RACE)

#part 4

mod1 <- lm(TOTCHG ~ AGE + FEMALE, data = hc)
hc$FEMALE<-as.factor(hc$FEMALE)
mod1 <- lm(TOTCHG ~ AGE + FEMALE, data = hc)
summary(mod1)
summary(hc$FEMALE)
head(hc)


#part 5

hc$RACE<-as.factor(hc$RACE)
mod2 <- lm(TOTCHG ~ AGE + FEMALE + RACE, data = hc)
summary(mod2)

#PART6
mod3 <- lm(TOTCHG ~ ., data = hc)
summary(mod3)


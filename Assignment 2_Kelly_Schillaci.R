library(readr)
storyteller <- read_csv("IST 707/data-storyteller (1).csv")
str(storyteller)

(sum(is.na(storyteller)))

colnames(storyteller)

storyteller$School <- factor(storyteller$School)
SectionsBySchool <- aggregate(storyteller$Section, by=list(NewCat=storyteller$School),FUN=length)


storyteller$TotalEnrolled <- storyteller$`Very Ahead +5`+storyteller$`Middling +0`+storyteller$`Behind -1-5`+storyteller$`More Behind -6-10`+storyteller$`Very Behind -11`+storyteller$Completed

storyteller$percentVeryAhead <- storyteller$`Very Ahead +5`/storyteller$TotalEnrolled
storyteller$percentMiddling <- storyteller$`Middling +0`/storyteller$TotalEnrolled
storyteller$percentBehind <- storyteller$`Behind -1-5`/storyteller$TotalEnrolled
storyteller$percentMoreBehind <- storyteller$`More Behind -6-10`/storyteller$TotalEnrolled
storyteller$percentVeryBehind <- storyteller$`Very Behind -11`/storyteller$TotalEnrolled
storyteller$percentCompleted <- storyteller$Completed/storyteller$TotalEnrolled

hist(storyteller$percentBehind)
hist(storyteller$percentCompleted)
hist(storyteller$percentVeryAhead)
hist(storyteller$percentMiddling)
hist(storyteller$percentMoreBehind)
hist(storyteller$percentVeryBehind)

mean(storyteller$percentBehind)
mean(storyteller$percentCompleted)
mean(storyteller$percentVeryAhead)
mean(storyteller$percentMiddling)
mean(storyteller$percentMoreBehind)
mean(storyteller$percentVeryBehind)

Behindbyschool <- tapply(storyteller$percentBehind, list(storyteller$School),mean)
completebyschool<- tapply(storyteller$percentCompleted, list(storyteller$School),mean)
VeryAheadbyschool<- tapply(storyteller$percentVeryAhead, list(storyteller$School),mean)
middlingbyschool<- tapply(storyteller$percentMiddling, list(storyteller$School),mean)
morebehindbyschool<- tapply(storyteller$percentMoreBehind, list(storyteller$School),mean)
verybehindbyschool<-tapply(storyteller$percentVeryBehind, list(storyteller$School),mean)

comparison <- data.frame(Behindbyschool,completebyschool, VeryAheadbyschool, middlingbyschool, morebehindbyschool, verybehindbyschool)
d <- comparison
Schools <- rownames(d)
rownames(d) <- NULL
data <- cbind(Schools,d)



colnames(comparison)
str(comparison)
summary(comparison)
rownames(comparison)
ncol(comparison)



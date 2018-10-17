#messing with political data
#I was treasurer for Ryan Fenwick's 2018 Mayoral campaign in Louisville and had the opportunity to do some descriptive data analytics for the team
#this data combines Democrat voter information with results of our first few months of canvassing
#the goal: determine how to prioritize canvassing efforts and focus our spending to maximize impact 

library(readxl)
library(zipcode)
library(ggplot2)
library(dplyr)
data(zipcode)

nrow(zipcode)
head(zipcode)
#this is my working directory.  change it to your preferred filepath
setwd("C:/Users/danie/Documents/civic engagement/Ryan Campaign/")
#the data came from VoteBuilder, combining ryan's previously recorded canvassing data with the existing data in the votebuilder/DNC database
power <- read_excel("C:/Users/danie/Documents/civic engagement/Ryan Campaign/Fenwick Master data 3-21-2018.xlsx", sheet="DanielsFormatAllthegoodstuff201")
# !diagnostics off
#make sure all variable names start with letters and not numbers.  In excel, I ran a quick 'find and replace' for "2016:"
ryandatazip <- merge(power, zipcode, by.x="Zip5", by.y = "zip")
 
louisville <- c(lon= -85.7, lat=38.21)
louisville_map <- get_map(louisville, zoom = 11, scale = 1)
ggmap(louisville_map) +
  geom_point(aes(longitude, latitude), data=ryandatazip)

ryandatazip[ryandatazip=='NA'] <- NA
answers <- ryandatazip
answers$race <- substr(answers$CodedRaceName, 1, 3) 
answers$white <- ifelse(answers$race == "Cau", 1, 0)
answers$black <- ifelse(answers$race == "Afr", 1, 0)
answers$wb <- answers$white+answers$black
answers$und <- ifelse(answers$supportnumeric == 3, 1, 0)
answers$ryan <- ifelse(answers$supportnumeric == 1, 1, ifelse(answers$supportnumeric == 2, 1,0))
answers$fischer <- ifelse(answers$supportnumeric == 4, 1, ifelse(answers$supportnumeric == 5, 1,0))
answers$support <- ifelse(answers$supportnumeric == 3, "Undecided", ifelse(answers$supportnumeric == 1, "Ryan", ifelse(answers$supportnumeric == 2, 'Ryan', ifelse(answers$supportnumeric == 4, "Fischer", ifelse(answers$supportnumeric == 5, "Fischer",0)))))
answers$raceRyan <- paste(answers$race,answers$support, sep = " ")
answersfull <- answers
answers <- answers[!is.na(answers$Fenwick_Support),]

write.table(answersfull, file="Fulldata.csv", row.names=FALSE)

#####
#Important Stuff
#####
#ideology
ggplot(b, aes(DNCIdeology, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIdeology, fill = race)) + geom_density(alpha = 0.2)

#AGE
ggplot(answers, aes(Age, fill = support)) + geom_density(alpha = 0.2)

#Race and support
counts <- table(b$raceRyan)
barplot(counts)

#race and turnout
ggplot(d, aes(DNCGenTurnoutV2, fill = race)) + geom_density(alpha = 0.2)


#College Educated
bb<- b %>% group_by(support) %>% summarise(grad=mean(DNCCollegeGrad))
ggplot(bb, aes(y=grad, x=support)) + geom_bar(stat="identity")

#KIDS
bb<- b %>% group_by(support) %>% summarise(kids=mean(DNCKidsPresent))
ggplot(bb, aes(y=kids, x=support)) + geom_bar(stat="identity")
mean(d$DNCKidsPresent, na.rm=TRUE)
ggplot(d, aes(DNCKidsPresent, fill = race)) + geom_density(alpha = 0.2)

#Min Wage
ggplot(b, aes(DNCIssueMinWage, fill = race)) + geom_density(alpha = 0.2)

#ideology
ggplot(b, aes(DNCIdeology, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIdeology, fill = race)) + geom_density(alpha = 0.2)


#Race voter data
sum(d$white, na.rm=TRUE) #87,000
sum(d$black, na.rm = TRUE) #27,000

#age voter turnout
dd<- d %>% group_by(Age) %>% summarise(turnout=mean(DNCGenTurnoutV2))
ggplot(dd, aes(x=Age, y=turnout)) + geom_bar(stat="identity")

ggplot(dd, aes(x=Age, y=turnout)) + geom_smooth()

#####
#CHARTS
ggplot(answers, aes(Age, fill = support)) + geom_histogram(alpha =.5, aes(y = ..density..), position = 'identity')
#this is a helpful chart for comparing support across ages
ggplot(answers, aes(Age, fill = support)) + geom_density(alpha = 0.2)

b<- subset(answers, wb==1)
c<- subset(b, ryan==1)


#this looks at the racial breakdown of fenwick support
counts <- table(b$raceRyan)
barplot(counts)

ggplot(c, aes(Age, fill = raceRyan)) + geom_histogram(alpha =.5, aes(y = ..density..), position = 'identity')
#this chart looks at the age breakdown of Ryan supporters, broken down by race 
ggplot(c, aes(Age, fill = raceRyan)) + geom_density(alpha = 0.2)


#support demographics from white voters
cau <- subset(b, white==1)
ggplot(cau, aes(Age, fill = support)) + geom_density(alpha = 0.2)

#support demographics from black voters
afr<- subset(b, black==1)
ggplot(afr, aes(Age, fill = support)) + geom_density(alpha = 0.2)

#distribution of turnout for races
d <- subset(answersfull, wb ==1)


#this data is  the full dataset
ggplot(d, aes(DNCGenTurnoutV2, fill = race)) + geom_density(alpha = 0.2)
#this is the response subset
ggplot(b, aes(DNCGenTurnoutV2, fill = race)) + geom_density(alpha = 0.2)

#turnout vs age vs race
ggplot(data=d, aes(y=DNCGenTurnoutV2, x=Age)) + geom_point()

#turnout vs support level
bb<- b %>% group_by(support) %>% summarise(voting=mean(DNCGenTurnoutV2))
ggplot(bb, aes(y=voting, x=support)) + geom_bar(stat="identity")
bb$voting[1]/bb$voting[2]-1

#looking at perceived issues

#College Educated
bb<- b %>% group_by(support) %>% summarise(grad=mean(DNCCollegeGrad))
ggplot(bb, aes(y=grad, x=support)) + geom_bar(stat="identity")

#KIDS
bb<- b %>% group_by(support) %>% summarise(kids=mean(DNCKidsPresent))
bb
ggplot(bb, aes(y=kids, x=support)) + geom_bar(stat="identity")
ggplot(d, aes(DNCKidsPresent, fill = support)) + geom_density(alpha = 0.2)
ggplot(d, aes(DNCKidsPresent, fill = race)) + geom_density(alpha = 0.2)
mean(d$DNCKidsPresent, na.rm=TRUE)
ggplot(b, aes(y=DNCGenTurnoutV2, x=DNCKidsPresent)) + geom_point()


#Choice
ggplot(b, aes(DNCIssueChoice, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIssueChoice, fill = race)) + geom_density(alpha = 0.2)


#Minimum Wage
ggplot(b, aes(DNCIssueMinWage, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIssueMinWage, fill = race)) + geom_density(alpha = 0.2)

e<-b
bc<- e %>% group_by(support) %>% summarise(MinWage=mean(DNCIssueMinWage, na.rm=TRUE))
ggplot(bc, aes(y=MinWage, x=support)) + geom_bar(stat="identity")

#gun control
ggplot(b, aes(DNCIssueGunCtrl, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIssueGunCtrl, fill = race)) + geom_density(alpha = 0.2)

#gun control
ggplot(b, aes(DNCIssueClimate, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIssueClimate, fill = race)) + geom_density(alpha = 0.2)

#ideology
ggplot(b, aes(DNCIdeology, fill = support)) + geom_density(alpha = 0.2)
ggplot(b, aes(DNCIdeology, fill = race)) + geom_density(alpha = 0.2)


#general age statistics - not all that helpful...
#conclusion - Ryan is more popular among young white people and older Black people
ggplot(b, aes(Age, fill = support)) + geom_density(alpha = 0.2)

mean(subset(answers, ryan>0), answers$Age*answers$ryan)
a<-subset(answers, ryan>0)
mean(a$Age*a$ryan)
median(a$Age*a$ryan)

a<-subset(answers, fischer>0)
mean(a$Age*a$fischer)
median(a$Age*a$fischer)

a<-subset(answers, und>0)
mean(a$Age*a$und)
median(a$Age*a$und)

sum(d$white, na.rm=TRUE) #87,000
sum(d$black, na.rm = TRUE) #27,000
sum(d$white*d$DNCGenTurnoutV2/100, na.rm = TRUE)
sum(d$black*d$DNCGenTurnoutV2/100, na.rm = TRUE)


sum(ifelse(d$Primary16=="Y",1,0), na.rm = TRUE)/sum(ifelse(d$General16=="Y",1,0), na.rm = TRUE)

#proportion of primary voters to general voters in 2014
sum(ifelse(d$Primary14=="Y",1,0), na.rm = TRUE)/sum(ifelse(d$General14=="Y",1,0), na.rm = TRUE)

sum(ifelse(d$General14=="Y",1,0), na.rm = TRUE)/sum(ifelse(d$General16=="Y",1,0), na.rm = TRUE)

sum(ifelse(d$Primary14=="Y",1,0), na.rm = TRUE)/sum(ifelse(d$'Voter File VANID'!="NA",1,0), na.rm = TRUE)

sum(ifelse(d$Primary14=="Y",1,0), na.rm = TRUE)

sum(ifelse(d$Primary16=="Y",1,0), na.rm = TRUE)

sum(d$black*ifelse(d$Primary14=="Y",1,0), na.rm = TRUE)

sum(d$white*ifelse(d$Primary14=="Y",1,0), na.rm = TRUE)

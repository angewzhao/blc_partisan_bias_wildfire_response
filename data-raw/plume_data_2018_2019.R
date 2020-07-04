library(Hmisc)

pu<-readRDS("C:/Users/me/Documents/Wildfire&Plume/mzip_month_smoke.RDS")
pu<-readRDS("C:/Users/iriss/Dropbox/Papers/Wildfire/mzip_month_smoke.RDS")

#+++++++++++++++++++++++++++++++++
#extract number of smoke days
#+++++++++++++++++++++++++++++++++

plume<-pu[pu$density==0,]
#aggregate 2018 and 2019 together
ag<-aggregate(plume$days, by=list(plume$month, plume$mzip), sum)
#aggregate 2018 and 2019, all months together
ag<-aggregate(plume$days, by=list(plume$mzip), sum)
colnames(ag)<-c("zipcode", "daycnt")

#plume density
ag<-aggregate(pu$days, by=list(pu$mzip, pu$density), sum)

days.all<-ag[ag$Group.2==0,]
days.high<-ag[ag$Group.2==27,]
days.med<-ag[ag$Group.2==16,]
days.low<-ag[ag$Group.2==5,]

#+++++++++++++++++++++++++++++++++
#extract number of smoke days
#+++++++++++++++++++++++++++++++++
#after 2018 August

pu$yrmth<-paste(pu$year, pu$month, sep="")

plume<-pu[pu$yrmth>201807,]

#plume density
ag<-aggregate(plume$days, by=list(plume$mzip, plume$density), sum)

days.all<-ag[ag$Group.2==0,]
days.high<-ag[ag$Group.2==27,]
days.med<-ag[ag$Group.2==16,]
days.low<-ag[ag$Group.2==5,]


day<-cbind(days.all, days.high, days.med, days.low)
colnames(day)<-c("zipcode", "density", "days.all",
            "zipcode2", "density2", "days.high",
            "zipcode3", "density3", "days.med",
            "zipcode4", "density4", "days.low")

#read in wildfire poll
library(car)
library(foreign)
#survey weight
library(questionr)

west<-read.spss("C:/Users/me/Dropbox/Papers/WesternRegionalPoll/STAN0128_main_OUTPUT.sav", use.value.labels =FALSE,
                to.data.frame=TRUE)
west<-read.spss("C:/Users/iriss/Dropbox/Papers/WesternRegionalPoll/STAN0128_main_OUTPUT.sav", use.value.labels =FALSE,
                to.data.frame=TRUE)
#extract CA sample

ca<-west[west$state_lookup==6,]

#merge zipcode file
ca2<-merge(ca, day, by.x="inputzip", by.y="zipcode", all.x=TRUE, all.y=FALSE)

ca2$smoke<-recode(ca2$q1_mod8, "1=1; 2=0; 3=NA")

t.test(ca2$days.all~ca2$smoke)
tapply(ca2$days.all, ca2$smoke, mean, na.rm=T)
summary(lm(days.all~smoke, data=ca2, weight=ca2$weight))

#by partyid
ca2$pid3b<-recode(ca2$pid3, "1=1; 2=2; else=3")


tapply(ca2$days.all, list(ca2$pid3b, ca2$smoke), mean, na.rm=T)
tapply(ca2$days.low, list(ca2$pid3b, ca2$smoke), mean, na.rm=T)
tapply(ca2$days.med, list(ca2$pid3b, ca2$smoke), mean, na.rm=T)
tapply(ca2$days.high, list(ca2$pid3b, ca2$smoke), mean, na.rm=T)



a<-glm(smoke~days.all*factor(pid3b), data=ca2, weight=ca2$weight, family='binomial')

newda<-data.frame(expand.grid(days.all=0:50, pid3b=1:3))


a<-glm(smoke~days.high*factor(pid3b), data=ca2, weight=ca2$weight, family='binomial')

newda<-data.frame(expand.grid(days.high=0:50, pid3b=1:3))

p.out<-data.frame(cbind(newda, predict(a, newda, type="link", se=TRUE)))
p.out2<- within(p.out, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})


plot(p.out2$PredictedProb[1:51], col='blue', ylim=c(0,1))
points(p.out2$PredictedProb[52:101], col='red')
points(p.out2$PredictedProb[103:153], col='green')
abline(h=0.5)

library(ggplot2)

ggplot(p.out2, aes(x = days.high, y = PredictedProb)) + 
#ggplot(p.out2, aes(x = days.all, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(pid3b)), alpha = 0.2) +
  geom_line(aes(colour = factor(pid3b)),size = 1)+
  geom_hline(yintercept=0.5, linetype="dashed")


tapply(ca2$smoke, ca2$pid3b, mean, na.rm=T)
summary(glm(smoke~factor(pid3b), data=ca2, weight=ca2$weight, family='binomial'))

summary(glm(smoke~daycnt, data=ca2, weight=ca2$weight, family='binomial'))

tapply(ca2$daycnt, list(ca2$smoke, ca2$pid3b), mean, na.rm=T)

tapply(ca2$daycnt, ca2$pid3b, mean, na.rm=T)

tapply(ca2$smoke, ca2$pid3b, mean, na.rm=T)

#among dem
dem<-ca2[ca2$pid3b==1,]

tapply(dem$daycnt, dem$smoke, mean, na.rm=T)



par(mar=c(5,15,2,2), mfrow=c(3,2))


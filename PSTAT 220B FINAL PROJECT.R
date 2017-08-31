library("ggplot2")
library("grid")
library("gridExtra")
library("car")
library("boot")

#create data frame
seatbelt <- data.frame(read.table("seatbelt.dat.txt",header=TRUE))

attach(seatbelt)

#sbprobs is the proportion of time the employee was observed
#wearing a seatbelt in a given period
sbprobs <- SeatBelt/Observed

#combine original data and sbprobs column
seatbeltprobs <- cbind(seatbelt,sbprobs)

#split seatbeltprobs into a list of multiple datasets divided by employee
splitsbprobs <- split(seatbeltprobs,seatbeltprobs$ID)

detach(seatbelt)

#create plots of proportion of time for each period that
#each employee was observed wearing a seatbelt 
ggplot(data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
       aes(x=Period,y=sbprobs,colour=factor(ID)))+
  geom_point(size=3)+
  geom_line()+
  facet_wrap(~ID,ncol=4)+
  labs(title="Proportion of Observations in each Period that each Employee Wore a Seat Belt")+
  labs(y="Proportion of Observations Wearing Seat Belt")+
  labs(colour="ID")

#create one plot for all employees showing proportion of time 
#for each period each employee was observed wearing a seatbelt
ggplot(data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
       aes(x=Period,y=sbprobs))+
  geom_point(size=3,aes(group=ID),colour="azure4")+
  geom_line(aes(group=ID),colour="azure4")+
  geom_smooth(size=3,colour="cornflowerblue",method="loess",
              data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
              se=FALSE)+
  labs(title="Proportion of Observations in each Period that each Employee Wore a Seat Belt")+
  labs(y="Proportion of Observations Wearing Seat Belt")+
  labs(colour="ID")

#Empirical desnities of proportion of seatbelt use for each period
ggplot(data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
       aes(sbprobs,fill=as.factor(Period)))+
  #geom_histogram(binwidth=0.15)+
  geom_density(alpha=0.2)+
  facet_wrap(~Period,ncol=2)+
  labs(title="Emperical Distributions of Proportions of Time Employees
       Wore Seatbelts in each Observation Period")+
  labs(y="Density")+
  labs(x="Proportion of Time Observed Wearing Seatbelt")+
  scale_fill_discrete("Period")

#Emperical distribution of driving Experience
ggplot(data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
       aes(Experience))+
  geom_histogram(binwidth=3,colour="orange",fill="black")+
  labs(title="Histogram of Employee Driving Experience")+
  labs(y="Count")
  
#create one plot for all employees shoing proportion of time
#for each epriod each employee was observed wearing a seatbelt
#by driving experience
ggplot(data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
       aes(x=Period,y=sbprobs))+
  geom_point(size=3,aes(group=ID,colour=Experience))+
  geom_line(aes(group=ID,colour=Experience))+
  facet_wrap(~ID,ncol=4)+
  labs(title="Proportion of Observations in each Period that each Employee Wore a Seatbelt")+
  labs(y="Proportion of Observations Wearing Seat Belt")+
  labs(colour="Experience")
#Might be able to see that those with more driving experience
#were less likely to continue wearing a seatbelt

#Scatterplot of Experience vs. seatbelt usage proportion by period
ggplot(data=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),],
       aes(x=Experience, y=sbprobs))+
  geom_point(size=3)+
  facet_wrap(~Period,ncol=2)+
  geom_smooth(se=F,method="lm")+
  labs(y="Proportion of Observations Wearing a Seatbelt")+
  labs(title="Proportion of Observations Employees wore a 
       Seat Belt in Each Time Period")
#sloping line in period 1 tells us that more experienced drivers may
#have been less likely than newer drivers to were a seatbelt before 
#intervention
#flat lines in periods 2 and 3 tell us that all drivers, no matter 
#the amount of experience, were fairly equally likely to wear and
#continue wearing a seatbelt
#sloping line in period 4 tells us that more experienced drivers may
#have been less likely than newer drivers to continue wearing their
#seatbelts
#May suggest that more experienced drivers may need more frequent 
#interventions

#attach data
attach(seatbelt)

#create Binomial GLM with only Period as explanatory variable
fit1 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period),
            family=binomial)
summary(fit1)

# fit1.1 <- glm(cbind(SeatBelt,Observed-SeatBelt)~Experience,
#             family=binomial)
# summary(fit1.1)

# anova(fit1,fit1.1)

#create Binomial GLM with Period and Experience main effects
fit2 <-glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience,
           family=binomial)
summary(fit2)

# anova(fit1.1,fit2,test="Chisq")

#check residual vs. covariate plots to check choice of link and/or
#missing covariates
#finding pearson residuals for fit2
pearesids2 <- residuals(fit2,type="pearson")

#find working residuals for fit2
worresid2 <- residuals(fit2,type="working")

#find deviance residuals for fit2
devresid2 <- residuals(fit2,type="deviance")

#find partial residuals fot fit2
parresids2 <- residuals(fit2,type="partial")

#working residuals vs covariates
wrcovplots2<-lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                             y=worresid2,
                                             xlab=names(seatbeltprobs)[i],
                                             ylab="Working Residuals")+
                      geom_point(colour="palevioletred",size=2)+
                      geom_smooth(method="loess",se=TRUE, 
                                  colour="palevioletred4",size=1)+
                      labs(title="Working Residual Plot for fit1"))
  
do.call(grid.arrange,c(wrcovplots2,ncol=2))

#deviance residuals vs covariates
drcovplot2 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                              y=devresid2,
                                              xlab=names(seatbeltprobs)[i],
                                              ylab="Deviance Residuals")+
                       geom_point(colour="orchid",size=2)+
                       geom_smooth(method="loess",se=TRUE, 
                                   colour="orchid4",size=1)+
                       labs(title="Deviance Residual Plot for fit1")) 
do.call(grid.arrange,c(drcovplot2,ncol=2))

#pearson residuals vs covariates
percovplot2 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids2,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1)+
                        labs(title="Pearson Residual Plot for fit1"))
do.call(grid.arrange,c(percovplot2,ncol=2))

#partial residuals vs covariates to check type of dependence that the
#dependent variable has on the IVs
parplots2.dat <- data.frame(cbind(seatbeltprobs[!is.na(seatbeltprobs$sbprobs),2],
                                  seatbeltprobs[!is.na(seatbeltprobs$sbprobs),5]))
names <- c("Period","Experience")
parplots2 <- lapply(1:2,function(i) qplot(x=parplots2.dat[,i],y=parresids2[,i],xlab=names[i],ylab="Partial Residuals")+geom_point(colour="palevioletred",size=2)+geom_smooth(method="loess",se=TRUE, colour="palevioletred4",size=1))
do.call(grid.arrange,c(parplots2,ncol=2))

#working residuals vs observations for fit2
wrplot2 <- qplot(y=worresid2,xlab="Observation",ylab="Working Residuals",main="Working Residual Plot for fit2")+geom_point(colour="aquamarine3",size=2)+geom_smooth(method="loess",se=FALSE,size=1)

#deviance residuals vs covariates for fit2
drplot2 <- qplot(y=devresid2,xlab="Observation",ylab="Deviance Residuals",main="Deviance Residual Plot for fit2")+geom_point(colour="orchid",size=2)+geom_smooth(method="loess",se=FALSE,color="orchid4",size=1)

#pearson residuals vs covariates for fit2
perplot2 <- qplot(y=pearesids2,xlab="Observation",ylab="Pearson Residuals",main="Deviance Residual Plot for fit2")+geom_point(colour="green",size=2)+geom_smooth(method="loess",se=FALSE,color="green4",size=1)

grid.arrange(perplot2,drplot2,wrplot2,ncol=2)










#try probit different link function
fit2.1 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience,
              family=binomial(link=probit))
summary(fit2.1)

#finding pearson residuals for fit2
pearesids2.1 <- residuals(fit2.1,type="pearson")

#find working residuals for fit2
worresid2.1 <- residuals(fit2.1,type="working")

#find deviance residuals for fit2
devresid2.1 <- residuals(fit2.1,type="deviance")

#find partial residuals fot fit2
parresids2.1 <- residuals(fit2.1,type="partial")

#working residuals vs covariates
wrcovplots2.1<-lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                             y=worresid2.1,
                                             xlab=names(seatbeltprobs)[i],
                                             ylab="Working Residuals")+
                      geom_point(colour="palevioletred",size=2)+
                      geom_smooth(method="loess",se=TRUE, 
                                  colour="palevioletred4",size=1)) 
do.call(grid.arrange,c(wrcovplots2.1,ncol=2))

#deviance residuals vs covariates
drcovplot2.1 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                              y=devresid2.1,
                                              xlab=names(seatbeltprobs)[i],
                                              ylab="Deviance Residuals")+
                       geom_point(colour="orchid",size=2)+
                       geom_smooth(method="loess",se=TRUE, 
                                   colour="orchid4",size=1)) 
do.call(grid.arrange,c(drcovplot2.1,ncol=2))

#pearson residuals vs covariates
percovplot2.1 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids2.1,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1)) 
do.call(grid.arrange,c(percovplot2.1,ncol=2))

#working residuals vs observations for fit2.1
wrplot2.1 <- qplot(y=worresid2.1,xlab="Observation",ylab="Working Residuals",main="Working Residual Plot for fit2.1")+geom_point(colour="aquamarine3",size=2)+geom_smooth(method="loess",se=FALSE,size=1)

#deviance residuals vs covariates for fit2.1
drplot2.1 <- qplot(y=devresid2.1,xlab="Observation",ylab="Deviance Residuals",main="Deviance Residual Plot for fit2.1")+geom_point(colour="orchid",size=2)+geom_smooth(method="loess",se=FALSE,color="orchid4",size=1)

#pearson residuals vs covariates for fit2.1
perplot2.1 <- qplot(y=pearesids2.1,xlab="Observation",ylab="Pearson Residuals",main="Deviance Residual Plot for fit2.1")+geom_point(colour="green",size=2)+geom_smooth(method="loess",se=FALSE,color="green4",size=1)

grid.arrange(perplot2.1,drplot2.1,wrplot2.1,ncol=2)


#try cloglog link function
fit2.2 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience,
              family=binomial(link=cloglog))
summary(fit2.2)

#finding pearson residuals for fit2
pearesids2.2 <- residuals(fit2.2,type="pearson")

#find working residuals for fit2
worresid2.2 <- residuals(fit2.2,type="working")

#find deviance residuals for fit2
devresid2.2 <- residuals(fit2.2,type="deviance")

#find partial residuals fot fit2
parresids2.2 <- residuals(fit2.2,type="partial")

#working residuals vs covariates
wrcovplots2.2<-lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=worresid2.2,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Working Residuals")+
                        geom_point(colour="palevioletred",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="palevioletred4",size=1)) 
do.call(grid.arrange,c(wrcovplots2.2,ncol=2))

#deviance residuals vs covariates
drcovplot2.2 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                y=devresid2.2,
                                                xlab=names(seatbeltprobs)[i],
                                                ylab="Deviance Residuals")+
                         geom_point(colour="orchid",size=2)+
                         geom_smooth(method="loess",se=TRUE, 
                                     colour="orchid4",size=1)) 
do.call(grid.arrange,c(drcovplot2.2,ncol=2))

#pearson residuals vs covariates
percovplot2.2 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                 y=pearesids2.2,
                                                 xlab=names(seatbeltprobs)[i],
                                                 ylab="Pearson Residuals")+
                          geom_point(colour="green",size=2)+
                          geom_smooth(method="loess",se=TRUE, 
                                      colour="green4",size=1)) 
do.call(grid.arrange,c(percovplot2.2,ncol=2))

#working residuals vs observations for fit2.2
wrplot2.2 <- qplot(y=worresid2.2,xlab="Observation",ylab="Working Residuals",main="Working Residual Plot for fit2.2")+geom_point(colour="aquamarine3",size=2)+geom_smooth(method="loess",se=FALSE,size=1)

#deviance residuals vs covariates for fit2.2
drplot2.2 <- qplot(y=devresid2.2,xlab="Observation",ylab="Deviance Residuals",main="Deviance Residual Plot for fit2.2")+geom_point(colour="orchid",size=2)+geom_smooth(method="loess",se=FALSE,color="orchid4",size=1)

#pearson residuals vs covariates for fit2.2
perplot2.2 <- qplot(y=pearesids2.2,xlab="Observation",ylab="Pearson Residuals",main="Deviance Residual Plot for fit2.2")+geom_point(colour="green",size=2)+geom_smooth(method="loess",se=FALSE,color="green4",size=1)

grid.arrange(perplot2.2,drplot2.2,wrplot2.2,ncol=2)

#cloglog makes no difference

#try adding cubic experience term
fit2.3 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience+I(Experience^3),
              family=binomial)
summary(fit2.3)

#finding pearson residuals for fit2
pearesids2.3 <- residuals(fit2.3,type="pearson")

#find working residuals for fit2
worresid2.3 <- residuals(fit2.3,type="working")

#find deviance residuals for fit2
devresid2.3 <- residuals(fit2.3,type="deviance")

#find partial residuals fot fit2
parresids2.3 <- residuals(fit2.3,type="partial")

#working residuals vs covariates
wrcovplots2.3<-lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=worresid2.3,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Working Residuals")+
                        geom_point(colour="palevioletred",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="palevioletred4",size=1)) 
do.call(grid.arrange,c(wrcovplots2.3,ncol=2))

#deviance residuals vs covariates
drcovplot2.3 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                y=devresid2.3,
                                                xlab=names(seatbeltprobs)[i],
                                                ylab="Deviance Residuals")+
                         geom_point(colour="orchid",size=2)+
                         geom_smooth(method="loess",se=TRUE, 
                                     colour="orchid4",size=1)) 
do.call(grid.arrange,c(drcovplot2.3,ncol=2))

#pearson residuals vs covariates
percovplot2.3 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                 y=pearesids2.3,
                                                 xlab=names(seatbeltprobs)[i],
                                                 ylab="Pearson Residuals")+
                          geom_point(colour="green",size=2)+
                          geom_smooth(method="loess",se=TRUE, 
                                      colour="green4",size=1)) 
do.call(grid.arrange,c(percovplot2.3,ncol=2))










#create Binomial GLM with Period and Experience main effects and interactions
fit3 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)*Experience,
            family=binomial)
summary(fit3)
#not an extremely large amount of evidence against the interactions

#test to see if adding Experience and then interactions are significant
anova(fit1,fit2,fit3,test="Chisq")

 #because adding interaction terms is significant, but interaction coefficients
#are not significant (these are approximate tests) we check that multiple 
#covariates are not providing the same information
vif(fit3)

#find peason residuals for fit3
pearesids3 <- residuals(fit3,type="pearson")

#find working residuals for fit3
worresid3 <- residuals(fit3,type="working")

#find deviance residuals for fit3
devresid3 <- residuals(fit3,type="deviance")

#find partial residuals fot fit1
parresids3 <- residuals(fit3,type="partial")

#working residuals vs observations
wrplot3 <- qplot(y=worresid3,xlab="Observation",ylab="Working Residuals",main="Working Residual Plot for fit2")+geom_point(colour="aquamarine3",size=2)+geom_smooth(method="loess",se=FALSE,size=1)

#deviance residuals vs observations
drplot3 <- qplot(y=devresid3,xlab="Observation",ylab="Deviance Residuals",main="Deviance Residual Plot for fit2")+geom_point(colour="orchid",size=2)+geom_smooth(method="loess",se=FALSE,color="orchid4",size=1)

#pearson residuals vs observations
perplot3 <- qplot(y=pearesids3,xlab="Observation",ylab="Pearson Residuals",main="Deviance Residual Plot for fit2")+geom_point(colour="green",size=2)+geom_smooth(method="loess",se=FALSE,color="green4",size=1)

grid.arrange(perplot3,drplot3,wrplot3,ncol=2)

#add square term because residual plots have a slight U pattern
fit4 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)*Experience+I(Experience^3),
            family=binomial)
summary(fit4)
#square term not significant
#AIC increases->bad
anova(fit3,fit4,test="Chisq")
#overall test not significant
#do not include square term

#look at plots to check for departures from the model
glm.diag.plots(fit3)
#these plots are slightly concerning
#try transformation of Experience

#transform Experience variable to fix issues with residual plots
fit5 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)*Experience-1,family=binomial)
summary(fit5)
glm.diag.plots(fit5)

fit5.1 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)*Experience+I(Experience^2)-1,family=binomial)
summary(fit5.1)
glm.diag.plots(fit5.1)

fit5.2 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)*Experience+I(Experience^2)+I(Experience^3)-1,family=binomial)
summary(fit5.2)
glm.diag.plots(fit5.2)

fit5.3 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+sqrt(Experience),family=binomial)
summary(fit5.3)
glm.diag.plots(fit5.3)

fit5.4 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+log(Experience),family=binomial)
summary(fit5.4)
glm.diag.plots(fit5.4)

fit5.5 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience,family=binomial)
summary(fit5.5)
glm.diag.plots(fit5.5)

fit5.6 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience+I(Experience^2),family=binomial)
summary(fit5.6)
glm.diag.plots(fit5)

fit5.7 <- glm(cbind(SeatBelt,Observed-SeatBelt)~as.factor(Period)+Experience+I(Experience^2)+I(Experience^3),family=binomial)
summary(fit5.7)
glm.diag.plots(fit5.7)


#find pearson residuals for fit5
pearesids5 <- residuals(fit5,type="pearson")
pearesids5.1 <- residuals(fit5.1,type="pearson")
pearesids5.2 <- residuals(fit5.2,type="pearson")
pearesids5.3 <- residuals(fit5.3,type="pearson")
pearesids5.4 <- residuals(fit5.4,type="pearson")
pearesids5.5 <- residuals(fit5.4,type="pearson")
pearesids5.6 <- residuals(fit5.4,type="pearson")
pearesids5.7 <- residuals(fit5.4,type="pearson")


#find working residuals for fit5
worresid5 <- residuals(fit5,type="working")

#find deviance residuals for fit5
devresid5 <- residuals(fit5,type="deviance")


#working residuals vs observatins for fit5
wrplot5 <- qplot(y=worresid5,xlab="Observation",ylab="Working Residuals",main="Working Residual Plot for fit5")+geom_point(colour="aquamarine3",size=2)+geom_smooth(method="loess",se=FALSE,size=1)

#deviance residuals vs observations for fit5
drplot5 <- qplot(y=devresid5,xlab="Observation",ylab="Deviance Residuals",main="Deviance Residual Plot for fit5")+geom_point(colour="orchid",size=2)+geom_smooth(method="loess",se=FALSE,color="orchid4",size=1)

#pearson residuals vs observations for fit5
perplot5 <- qplot(y=pearesids5,xlab="Observation",ylab="Pearson Residuals",main="Deviance Residual Plot for fit5")+geom_point(colour="green",size=2)+geom_smooth(method="loess",se=FALSE,color="green4",size=1)

grid.arrange(perplot5,drplot5,wrplot5,ncol=2)
#tranformation makes essentially no difference in these plots 
#for the sake of interpretability, do not use log transformation


#Pearson residuals vs. linear predictors for fit5
percovplot5 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids5,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1))
do.call(grid.arrange,c(percovplot5,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5"))

#Pearson residuals vs. linear predictors for fit5.1
percovplot5.1 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids5.1,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.1,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.1"))

#Pearson residuals vs. linear predictors for fit5.2
percovplot5.2 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids5.2,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.2,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.2"))

#Pearson residuals vs. linear predictors for fit5.3
percovplot5.3 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids5.3,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.3,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.3"))

#Pearson residuals vs. linear predictors for fit5.4
percovplot5.4 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids5.4,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.4,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.4"))

#Pearson residuals vs. linear predictors for fit5.5
percovplot5.5 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                 y=pearesids5.5,
                                                 xlab=names(seatbeltprobs)[i],
                                                 ylab="Pearson Residuals")+
                          geom_point(colour="green",size=2)+
                          geom_smooth(method="loess",se=TRUE, 
                                      colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.5,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.5"))

#Pearson residuals vs. linear predictors for fit5.6
percovplot5.6 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                 y=pearesids5.6,
                                                 xlab=names(seatbeltprobs)[i],
                                                 ylab="Pearson Residuals")+
                          geom_point(colour="green",size=2)+
                          geom_smooth(method="loess",se=TRUE, 
                                      colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.6,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.6"))

#Pearson residuals vs. linear predictors for fit5.7
percovplot5.7 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                                 y=pearesids5.7,
                                                 xlab=names(seatbeltprobs)[i],
                                                 ylab="Pearson Residuals")+
                          geom_point(colour="green",size=2)+
                          geom_smooth(method="loess",se=TRUE, 
                                      colour="green4",size=1))
do.call(grid.arrange,c(percovplot5.7,ncol=2,main="Pearson Residuals vs. Linear Predictors fot fit5.7"))










#working residuals vs covariates for fit3
wrcovplots3<-lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                             y=worresid3,
                                             xlab=names(seatbeltprobs)[i],
                                             ylab="Working Residuals")+
                      geom_point(colour="palevioletred",size=2)+
                      geom_smooth(method="loess",se=TRUE, 
                                  colour="palevioletred4",size=1)) 
do.call(grid.arrange,c(wrcovplots3,ncol=2))

#deviance residuals vs covariates for fit3
drcovplot3 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                              y=devresid3,
                                              xlab=names(seatbeltprobs)[i],
                                              ylab="Deviance Residuals")+
                       geom_point(colour="orchid",size=2)+
                       geom_smooth(method="loess",se=TRUE, 
                                   colour="orchid4",size=1)) 
do.call(grid.arrange,c(drcovplot3,ncol=2))

#pearson residuals vs covariates for fit3
percovplot3 <- lapply(c(2,5),function(i) qplot(x=seatbeltprobs[!is.na(seatbeltprobs$sbprobs),i],
                                               y=pearesids3,
                                               xlab=names(seatbeltprobs)[i],
                                               ylab="Pearson Residuals")+
                        geom_point(colour="green",size=2)+
                        geom_smooth(method="loess",se=TRUE, 
                                    colour="green4",size=1)) 
do.call(grid.arrange,c(percovplot3,ncol=2))






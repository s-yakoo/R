## Final Project
#Data Analytic Task

#1. read in .csv file
survey <- read.csv("survey1.csv")
summary(survey)


survey$DRINK <- as.factor(survey$DRINK)
drink.label <- c('YES','NO')
survey$Drink <- as.factor(drink.label[survey$DRINK])

survey$GENDER <- as.factor(survey$GENDER)
gender.label <- c('Male','Female')
survey$Gender <- as.factor(gender.label[survey$GENDER])

survey$SMOKE <- as.factor(survey$SMOKE)
smoke.label <- c('YES','NO')
survey$Smoke <- as.factor(smoke.label[survey$SMOKE])

survey$ETHNICITY <- as.factor(survey$ETHNICITY)
ethnicity.label <- c('Other','Other','Other','Other','Latino','White')
survey$Ethnicity <- as.factor(ethnicity.label[survey$ETHNICITY])

#BMI <- ((survey$WT)/(survey$HT^2)) * 703
survey$bmi <- as.numeric((survey$WT)/(survey$HT^2)) * 703

attach(survey)

#2. Think about the research question and provide tables describing your data and associations.

# unadjusted associations of predictors with HD
u.model1<-glm(relevel(as.factor(HD),"1")~bmi,family="binomial"(link="logit"),data=survey)
u.model2<-glm(relevel(as.factor(HD),"1")~Drink,family="binomial"(link="logit"),data=survey)
u.model3<-glm(relevel(as.factor(HD),"1")~PA,family="binomial"(link="logit"),data=survey)
u.model4<-glm(relevel(as.factor(HD),"1")~HOURS,family="binomial"(link="logit"),data=survey)
u.model5<-glm(relevel(as.factor(HD),"1")~Smoke,family="binomial"(link="logit"),data=survey)
u.model6<-glm(relevel(as.factor(HD),"1")~AGE,family="binomial"(link="logit"),data=survey)
u.model7<-glm(relevel(as.factor(HD),"1")~Ethnicity,family="binomial"(link="logit"),data=survey)
u.model8<-glm(relevel(as.factor(HD),"1")~Gender,family="binomial"(link="logit"),data=survey)
u.model9 <-glm(relevel(as.factor(HD),"1")~HBP,family="binomial"(link="logit"),data=survey)

summary(u.model1)
summary(u.model2)
summary(u.model3)
summary(u.model4)
summary(u.model5)
summary(u.model6)
summary(u.model7)
summary(u.model8)
summary(u.model9)


#percentages for all levels of a factor
descript(bmi)
freq(Drink)
descript(PA)
descript(HOURS)
freq(Smoke)
descript(AGE)
freq(Ethnicity)
freq(Gender)
freq(HBP)

# Performs a Likelihood Ratio Test
anova(u.model1,test="LRT")
anova(u.model2,test="LRT")
anova(u.model3,test="LRT")
anova(u.model4,test="LRT")
anova(u.model5,test="LRT")
anova(u.model6,test="LRT")
anova(u.model7,test="LRT")
anova(u.model8,test="LRT")
anova(u.model9,test="LRT")


# Computes the ORs and 95% CIs
exp(cbind(OR = coef(u.model1), confint(u.model1)))
exp(cbind(OR = coef(u.model2), confint(u.model2)))
exp(cbind(OR = coef(u.model3), confint(u.model3)))
exp(cbind(OR = coef(u.model4), confint(u.model4)))
exp(cbind(OR = coef(u.model5), confint(u.model5)))
exp(cbind(OR = coef(u.model6), confint(u.model6)))
exp(cbind(OR = coef(u.model7), confint(u.model7)))
exp(cbind(OR = coef(u.model8), confint(u.model8)))
exp(cbind(OR = coef(u.model9), confint(u.model9)))


#3. 

a.model1<-glm(relevel(as.factor(HD),"1")~bmi,family="binomial"(link="logit"),data=survey)
summary(a.model1)
a.model2<-glm(relevel(as.factor(HD),"1")~bmi+HBP,family="binomial"(link="logit"),data=survey)
summary(a.model2)
a.model3<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE,family="binomial"(link="logit"),data=survey)
summary(a.model3)
a.model4<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS,family="binomial"(link="logit"),data=survey)
summary(a.model4)
a.model5<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS+Drink,family="binomial"(link="logit"),data=survey)
summary(a.model5)
a.model6<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS+Drink+PA,family="binomial"(link="logit"),data=survey)
summary(a.model6)
a.model7<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS+Drink+PA+Smoke,family="binomial"(link="logit"),data=survey)
summary(a.model7)
a.model8<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS+Drink+PA+Smoke+Ethnicity,family="binomial"(link="logit"),data=survey)
summary(a.model8)
a.model9<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS+Drink+PA+Smoke+Ethnicity+Gender,family="binomial"(link="logit"),data=survey)
summary(a.model9)

#final model 
a.model4<-glm(relevel(as.factor(HD),"1")~bmi+HBP+AGE+HOURS,family="binomial"(link="logit"),data=survey)

exp(cbind(OR = coef(a.model4), confint(a.model4)))


#4. Calculate and interpret the odds ratio for each of the following, provide a 95% confidence interval for each...

# need to make continuous bmi into categorical

survey$BMI[survey$bmi >= 30] <-'obese'
survey$BMI[survey$bmi >= 25 & survey$bmi <= 29.9] <-'over'
survey$BMI[survey$bmi >= 18.5 & survey$bmi <= 24.9] <-'normal'
survey$BMI[survey$bmi < 18.5] <-'under'

install.packages("multcomp")
library("multcomp")


#a. Are obese hypertensive subjects more likely to have heart disease than normal weight
#hypertensive subjects, controlling for all other variables in the model?

a.model4<-glm(relevel(as.factor(HD),"1")~BMI+HBP+AGE+HOURS,family="binomial"(link="logit"),data=survey)
summary(a.model4)

delta.a<-matrix(c(0,1,0,-1,0,0,0),1)
test1<-glht(a.model4, linfct=delta.a)
summary(test1)
exp(confint(test1)$confint)[1,]

delta.b <-matrix(c(0,1,-1,0,0,0,0),1)
test2<-glht(a.model4, linfct=delta.b)
summary(test2)
exp(confint(test2)$confint)[1,]

delta.c <-matrix(c(0,1,-1,0,1,0,0),1)
test3<-glht(a.model4, linfct=delta.c)
summary(test3)
exp(confint(test3)$confint)[1,]

delta.d <-matrix(c(0,0,-1,1,1,0,0),1)
test4<-glht(a.model4, linfct=delta.d)
summary(test4)
exp(confint(test4)$confint)[1,]

detach(survey)

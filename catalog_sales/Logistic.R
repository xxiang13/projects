########################
#Stat Project Logistic #
########################

load("/Users/apple/Documents/MSiA/MSiA 401 stat/Project/clean_data_2.Rdata")
attach(project4)
#slip data in to training & test two sets
train <- subset(project4, train == 1) #return 50418 obs.
test<- subset(project4, train == 0) #return 51114 obs.
train<-train[-c(41618,12517,11788,78696,37929,39868,2867,37468,30114,80954,38551,48690,42104,41618),]
detach(project4)
#in trainning data
#create a response variable, name "response"
train$response<- ifelse(train$targdol==0, 0, 1)
test$response<- ifelse(test$targdol==0, 0, 1)
attach(train)

#check each preditive variable if needs to be tranformed
hist(recencydays) #find out recencydays right skewed
hist(log(recencydays+1)) #after log transformed, more nornal distributed
hist(log(slshist+1), breaks=50) #after log transformed, more nornal distributed



fit<-glm(response~ recencymon+falord+sprord+datead6+ordtyr:ordlyr:ord3ago, data = train)
summary(fit)
outlierTest(fit)
library(car)
vif(fit)
dwt(fit)
logLik(fit) #log lik -13344/15 (df=14)
deviance(fit) 
fit$null.deviance - deviance(fit)
fit0<-glm(response~1, binomial, train)
anova(fit0,fit, test="Chisq") #shows fit model significant 
fit$fitted.values[1:5]

tab = table(train$response, fit$fitted.values >.15)
tab
prop.table(tab,1)
sum(diag(tab))/sum(tab) #classifiation rate
tab[2,2]/sum(tab[2,]) #classification rate for response =1
tab[1,1]/sum(tab[1,]) #classification rate for reponse =0

library(pROC)
plot.roc(train$response,fit$fitted.values )

phat = predict(fit, test, type ="resp")
tab2 = table(test$response, phat>.15)
tab2
sum(diag(tab2))/sum(tab2) # test set classification rate
tab2[2,2]/sum(tab2[2,]) #classification rate for response =1
tab2[1,1]/sum(tab2[1,]) #classification rate for reponse =0

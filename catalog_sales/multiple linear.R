################################
#Stat Project multiple linear  #
#Shawn Li                      #
################################

load("/Users/apple/Documents/MSiA/MSiA 401 stat/Project/clean_data_2.Rdata")
attach(project4_new)

targdol_nonzero<-subset(project4, targdol > 0)
#slip data in to training & test two sets
train1 <- subset(targdol_nonzero, train == 1) #return 50418 obs.
test1<- subset(targdol_nonzero, train == 0)

detach(project4)


attach(targdol_nonzero)
#check linearity
plot(recencymon,targdol)
plot(falord,targdol)
plot(sprord,targdol)
plot(slshist,targdol)
plot(slstyr,targdol)
plot(slslyr,targdol)
plot(sls2ago,targdol)
plot(sls3ago,targdol)
plot(ordtyr,targdol)

fit1<-lm(log(targdol+1)~ recencymon + slshist+ slstyr*slslyr+slstyr*sls2ago+slstyr*sls3ago+slslyr*sls3ago+slslyr*sls2ago + sls2ago*sls3ago
         +ordtyr*ordlyr+ordtyr*ord2ago+ordtyr*ord3ago+ordlyr*ord2ago+ordlyr*ord3ago + ord2ago*ord3ago+falord + sprord,data = train1)
summary(fit1)
fit2<-step(fit1)
summary(fit2)

fit.multi = lm(formula = log(targdol + 1) ~ datead6 + sqrt(slstyr) + 
                 sqrt(slslyr) + sqrt(sls2ago) + sqrt(sls3ago) + sqrt(slshist) + sqrt(ordtyr) + sqrt(ordlyr) + 
                 sqrt(ord2ago) + sqrt(ord3ago) + 
                 ordtyr:ordlyr + sqrt(sls2ago):sqrt(sls3ago) , data = train1)
summary(fit.multi)


library(car)
vif(fit3)
dwt(fit3)

outlierTest(fit3) # Bonferonni p-value for most extreme obs
qqPlot(fit2, main="QQ Plot") #qq plot for studentized resid 
influencePlot(fit3)
leveragePlots(fit3) # leverage plots


train1<-train1[-c(41618,12517,11788,78696,37929,39868,2867,37468,30114,80954,38551,48690,42104,41618),]

fit3<-lm(log(targdol+1)~slstyr+slslyr+sls2ago+sls3ago+ordtyr+ordlyr
         +ord2ago+ord3ago+slstyr:sls2ago+slslyr:sls3ago+slslyr:sls2ago+ordtyr:ordlyr+slstyr:slslyr+ordlyr:ord2ago,data = train1)
summary(fit3)
vif(fit3)

str(train1)
attach(train1)
data<-data.frame(targdol,slstyr,slslyr,sls2ago,sls3ago,slshist,ordtyr,ordlyr,ord2ago,ord3ago,falord,sprord,recencymon,recencydays)
cor(data)




#outlinears
library(car)
outlierTest(fit3) # Bonferonni p-value for most extreme obs
qqPlot(fit3, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit3) # leverage plots

# leverage plot
index.multi = seq(1,dim(vars)[1]);
leverage.multi = hat(model.matrix(fit.multi));
plot(index.multi,leverage.multi);

#influential_points (cook's distance)
# influence plot
library(car);   # needed for "influencePlot" function below
influencePlot(fit.multi);

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff = 4/(dim(vars)[1] - length(fit.multi$coefficients)-2); 
plot(fit.multi, which=4, cook.levels=cutoff, main = "Cook's D Plot");
influencePlot(fit.multi,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# tailored setting to define influential observations
fit.multi.temp = cbind(cooks.distance(fit.multi), hat(model.matrix(fit.multi)));   # tempoaray matrix
#vars[which(fit.multi.temp[,1]>cutoff & fit.multi.temp[,2] > 2*cutoff),];


# fitting without the influential observations
vars = vars[-which(fit.multi.temp[,1]>cutoff & fit.multi.temp[,2] > 2*cutoff),];
fit.multi = lm(formula = log(targdol + 1) ~ datead6 + slstyr + 
                 slslyr+sls2ago + slshist+ ordtyr + ordlyr + ord2ago + 
                 ordhist + falord + sprord  + recencymon+ordtyr:ordlyr:ord2ago+slstyr:slslyr:sls2ago, data = multiple_project)



#Normality
# Histogram
fit.multi.stdres = rstandard(fit.multi);
hist(fit.multi.stdres, freq=FALSE); # with freq=T, histogram with counts
curve(dnorm,add=TRUE) # dnorm = normal density

# Normal plot(QQ plot)
qqnorm(fit.multi.stdres);
qqline(fit.multi.stdres, col="red");

qqPlot(fit.multi, main="QQ Plot") #qq plot for studentized resid 

# Residual vs fitted
plot(fit.multi$fitted,fit.multi$resid); 

# standardized
index.multi = seq(1,dim(vars)[1]);
plot(index.multi,rstandard(fit.multi));



#Linearity 
# Evaluate Nonlinearity
fit.multi2 = lm(formula = log(targdol + 1) ~ datead6 + slstyr + 
                  slslyr+sls2ago + slshist+ ordtyr + ordlyr + ord2ago + 
                  ordhist + falord + sprord  + recencymon, data = multiple_project)
# component + residual plot 
crPlots(fit.multi2)
# Ceres plots 
ceresPlots(fit.multi2)


#homoscedasticity
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit.multi)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit.multi)

#Multicolinarlity (VIF and correlation)
library(car)
vif(fit.multi)
reduced_vars; 
cor(reduced_vars);
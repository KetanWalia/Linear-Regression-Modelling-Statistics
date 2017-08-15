
rm(list=ls())

setwd('C:/Users/ketan walia/Downloads')


library(MASS)
library(readxl)
library(moments)
library(dplyr)

##Problem 1: Simple Linear Models
hw2pb1=read.table("HW2Pb1Data.txt",header=T)

#head(hw2pb1)

attach(hw2pb1) 
model <-lm(Result_with_noise ~ Discipline*Method)

summary(lm(Result_with_noise ~ Discipline*Method)) 

a<-predict(lm(Result_with_noise ~ Discipline*Method),interval="confidence") 

final1<- cbind(Result,Result_with_noise,a)
final1<-data.frame(final1)
#View(final1)
#head(final1)
#class(final1)

s<-ifelse(final1$lwr<final1$Result,1,ifelse(final1$Result>final1$upr,1,0))

comp <- cbind(final1,s)


print(sum(s))

write.csv(final1,"prob1.csv")

detach(hw2pb1)

# Problem 2 :SIMPLE STATISTICS
library(MASS)
library(readxl)
library(moments)
library(dplyr)

data_2 <- read.table("HW2Pb2Data.txt",header=TRUE)
#head(data_2)
attach(data_2)
hist(data_2$X)
#normal_test_x <- shapiro.test(X)
#normal_test_y <- shapiro.test(Y)

#### Question 2.1

par(mfrow=c(1,2))
qqnorm(X, main = "Normal Q-Q Plot X Variable",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(X)

qqnorm(Y, main = "Normal Q-Q Plot Y Variable",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(Y)

#### Question 2.2
par(mfrow=c(1,2))

boxplot(X,main = "BoxPlot X Variable")
boxplot(Y, main = "BoxPlot Y Variable")

#### Question 2.3
summ_X <- data.frame(mu=mean(X), Std=sd(X), Skew=skewness(X), kurt=kurtosis(X))

summ_Y <- data.frame(mu=mean(Y), Std=sd(Y), Skew=skewness(Y), kurt=kurtosis(Y))

v<-rbind(summ_X,summ_Y)

row.names(v)<-c("X","Y")

#### Question 2.4
set.seed(100)
m<-1000; med<-numeric(m); mn<-numeric(m);
for (i in 1:m){
  b=sample(X,replace=T);
  med[i]=median(b);
  mn[i]=mean(b);
} 

med_x<-var(med)
mean_x<-var(mn)

set.seed(102)
n<- 1000
med_y<-numeric(n)
avg_y<-numeric(n)
for (i in 1:n){
  c=sample(Y,replace=T);
  med_y[i]=median(c);
  avg_y[i]=mean(c);
}
#length(med_y)
med_y<-var(med_y)

mean_y<-var(avg_y)

j<- data.frame(med_x,mean_x,med_y,mean_y)
View(j)

#### Question 2.5

not_paired = t.test(X,Y,paired=F)
paired = t.test(X,Y,paired=T)

willcox_paired = wilcox.test(X,Y,paired=T)
willcox_notpaired = wilcox.test(X,Y,paired=F)

detach (data_2) 

###PROBLEM 3: More Modelling

rm(list=ls())
library(MASS)
library(readxl)
library(moments)
library(dplyr)

data_3 <- read.table("C:\\Users\\ketan walia\\Downloads\\HW2Pb3Data.txt",header=TRUE)
attach(data_3)

#### Question 3.1
# fit the linear regression model
model = lm(y~x1+x2)
summary(model)
#summary(aov(y~x1+x2))
#res = residuals(model)


#### Question 3.2
res = residuals(model)
pairs(~ model$residuals+x1+x2)

#### Question 3.3
model2 = lm(y~x1+x2+I(x1^2)+I(x2^2)+I(x1*x2))
summary(model2)

#### Question 3.4
library(MASS)
model3 <-stepAIC(model2,direction="backward")

# build the model with selected attributes
model3 <- lm(y~x1+x2+I(x1^2))
summary(model3)

#### Question 3.5

plot(x1,residuals(model3))

#### Question 3.6
par(mfrow=c(1,2))
qqnorm(residuals(model),xlab="model1")
qqline(residuals(model))

qqnorm(residuals(model3),xlab="model3")
qqline(residuals(model3))
hist(residuals(model3))
#shapiro.test(residuals(model3))
detach(data_3)

#### Question 3.7
dt <- read.table("C:\\Users\\ketan walia\\Downloads\\HW2Pb3Data.txt",header=TRUE)
attach(dt)


#Create a cross validation function
library(moments)
library(MASS)
partition.cv <- function (dat, ratio){
  n=dim(dat)[1]
  trn.ind=which(runif(n)<ratio)
  trn=dat[trn.ind,]
  val=dat[-trn.ind,]
  cv=list(train=trn,validation=val)
  return(cv)
}

#Create a MSE error calculation function
mse <- function(model, dat){
  mean((dat$y-predict(model,dat))^2)
}


num=100
ratio=0.8
trn.err=matrix(0,nrow=3,ncol=num)
tst.err=matrix(0,nrow=3,ncol=num)

for (it in 1:num){
  cv=partition.cv(dt,ratio)
  train=cv$train
  validation=cv$validation
  cvmodel1 <- lm(y ~ x1+x2, data=train)
  cvmodel2 <- lm (y ~ x1+x2+I(x1^2)+I(x2^2)+I(x1*x2),
                  data=train)
  cvmodel3 = lm (y ~ x1+x2+I(x1^2), data=train)
  
  trn.err[1,it]=mse(cvmodel1,train)
  tst.err[1,it]=mse(cvmodel1,validation)
  trn.err[2,it]=mse(cvmodel2,train)
  tst.err[2,it]=mse(cvmodel2,validation)
  trn.err[3,it]=mse(cvmodel3,train)
  tst.err[3,it]=mse(cvmodel3,validation)
}
View(trn.err)
trn.mn=rep(0,3)
trn.sd=rep(0,3)
tst.mn=rep(0,3)
tst.sd=rep(0,3)

for (j in 1:3){
  trn.mn[j] = mean(trn.err[j,])
  trn.sd[j] = sd(trn.err[j,])
  tst.mn[j] = mean(tst.err[j,])
  tst.sd[j] = sd (tst.err[j,])
  print(paste("Model ", j, " Training Error: ",trn.mn[j],
              "+/-", trn.sd[j]))
  print(paste(" Test Error: ", tst.mn[j], "+/-",
              tst.sd[j]))
}

### Problem 4: Training and Testing Models

rm(list=ls())
library(MASS)
library(readxl)
library(moments)
library(dplyr)
#### Question 4.1
data_train <- read.table("C:\\Users\\ketan walia\\Downloads\\HW2PB4Data.train.txt",header=TRUE)
data_test <- read.table("C:\\Users\\ketan walia\\Downloads\\HW2PB4Data.test.txt",header=TRUE)
## Combining the dataset
data_set = rbind(data_train,data_test)
#class(data_set)
head(data_set)

attach(data_set)

d <- levels(town)
##Creating dummy variables for town A
c <-1
for (i in data_set$town){
if(i=="A"){
  data_set$townA[c]<- 1
  c<-c+1
}else{
  
  data_set$townA[c]<- 0
  c<-c+1
}
}
##Creating dummy variables for town B

c <-1
for (i in data_set$town){
  #c<-1
  if(i=="B"){
    data_set$townB[c]<- 1
    c<-c+1
  }else{
    
    data_set$townB[c]<- 0
    c<-c+1
  }
}
##Creating dummy variables for town C

c <-1
for (i in data_set$town){
  #c<-1
  if(i=="C"){
    data_set$townC[c]<- 1
    c<-c+1
  }else{
    
    data_set$townC[c]<- 0
    c<-c+1
  }
}
##Creating dummy variables for town D
c <-1
for (i in data_set$town){
  #c<-1
  if(i=="D"){
    data_set$townD[c]<- 1
    c<-c+1
  }else{
    
    data_set$townD[c]<- 0
    c<-c+1
  }
}
#head(data_set)
data_set$town<-NULL
head(data_set)

## More dummy variables

d1 = ifelse(data_set$district == "d1",1,0)
d2 = ifelse(data_set$district == "d2",1,0)
data_set$district<-NULL

fam1 = ifelse(family == "f1",1,0)
fam2 = ifelse(family == "f2",1,0)
data_set$family<-NULL

GenM = ifelse(gender == "male",1,0)
data_set$gender<-NULL

st1 = ifelse(street == "s1",1,0)
st2 = ifelse(street == "s2",1,0)
st3 = ifelse(street == "s3",1,0)
data_set$street<-NULL
##Combining the dataset
data_set = cbind.data.frame(data_set,d1,d2,fam1,fam2,
                           st1,st2,st3,GenM,replicate)

data_set[[2]]<-NULL

View(data_set)

##Splitting into training and test data again
train_data = data_set[1:347,]
test_data = data_set[348:720,]

#### Question 4.2
model1 = lm(subject~.,train_data)
summary(model1)

#### Question 4.3
# create a model with interaction of variables

model2<-lm(subject~replicate+townA+townB+townC+townD+d1+d2+st1+st2+st3+fam1
               +fam2+GenM+replicate*townA+replicate*townB+replicate*townC+replicate*townD+replicate*d1+replicate*d2+replicate*st1+replicate*st2+replicate*st3+replicate*fam1+ replicate*fam2+replicate*GenM
               +townA*townB+townA*townC+townA*townD*townA*d1+townA*d2+townA*st1
               +townA*st2+townA*st3+townA*fam1+townA*fam2+townA*GenM
               +townB*townC+townB*townD*townB*d1+townB*d2+townB*st1+townB*st2+townB*st3+townB*fam1+townB*fam2+townB*GenM
               +townC*townD*townC*d1+townC*d2+townC*st1+townC*st2+townC*st3+townC*fam1
               +townC*fam2+townC*GenM
               +townD*d1+townD*d2+townD*st1+townD*st2+townD*st3+townD*fam1+townD*fam2+townD*GenM
               +d1*d2+d1*st1+d1*st2+d1*st3+d1*fam1+d1*fam2+d1*GenM
               +d2*st1+d2*st2+d2*st3+d2*fam1+d2*fam2+d2*GenM
               +st1*st2+st1*st3+st1*fam1+st1*fam2+st1*GenM
               +st2*st3+st2*fam1+st2*fam2+st2*GenM
               +st3*fam1+st3*fam2+st3*GenM
               +fam1*fam2+fam1*GenM
               +fam2*GenM,data=train_data)


#summary(model2)
summary(aov(model2))

#### Question 4.4
model3 = lm(subject~GenM+st1+d1+townC+townA+townD*fam2+ d2*st3+townD*d2+townC*GenM+townC*st2+townC*d2+townB*fam1+townB*st1+townA*st1+townA*d2+replicate*townA,train_data)
summary(model3)

library(MASS)
stepAIC(model3,direction="both")


model4 = lm(subject ~ GenM + st1 + d1 + townC + townA + townD + 
                  fam2 + d2 + st3 + st2 + townB + fam1  + townD*fam2 + 
                  d2*st3 + townD*d2 + GenM*townC + townC*st2 + townB*fam1 + 
                  st1*townB + st1*townA + townA*d2 + townA*replicate+ replicate , data = train_data)

summary(model4)

#### Question 4.5
n<-dim(train_data)[1]
p<-23
#dof= degree of freedom
#mse=sse/dof
dof<-n-p-1
#dof<-323
sse_model4 = sum((predict(model4,train_data)-train_data$subject)^2)

mse_model4<-(sum(sse_model4))/dof

#### Question 4.6
test_y = predict(model4,test_data)

# Calculate the test MSE
sset_model4 = sum((predict(model4,test_data)-test_data$subject)^2)
N<-dim(test_data)[1]
P<-23
Doft<-N-P-1
mset_model4<-(sum(sset_model4))/Doft



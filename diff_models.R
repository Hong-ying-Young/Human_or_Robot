rm()
update.packages() 
install.packages("longitudinal")
install.packages("e1071")
install.packages("plyr")
install.packages(c("tidyr", "devtools"))
install.packages("arm")
install.packages("ISLR")
install.packages("boot")
install.packages("mnlogit")
install.packages("mlogit")
install.packages("nnet")
install.packages("glmulti")
install.packages("biglm")
install.packages("kernlab")
install.packages("randomForest")
install.packages("party")
install.packages("adabag")
install.packages("ada")
install.packages("cvAUC")

#merge bids and train files by bidder_id
library(MASS)
library(lasso2)
library(glmnet)
library("longitudinal")
library(e1071)
library("plyr")
library("arm")
library(ISLR)
library(glmnet)
library(boot)
library("mnlogit")
library("mlogit")
library(nnet)
library(glmulti)
library(biglm)
library(kernlab)
library(randomForest)
library(party)
require(graphics)
library("adabag")
library(ada)
library(cvAUC)

setwd("~/Documents/kaggle/facebook human and robot")
load("~/Documents/kaggle/facebook human and robot/data.RData")
bids <- data.frame(read.csv(file="bids.csv", head=TRUE, sep=","))
train <- data.frame(read.csv(file="train.csv",head=TRUE, sep=","))
test <- data.frame(read.csv(file="test.csv",head=TRUE, sep=","))
submit = data.frame(read.csv(file="Submission.csv",head=TRUE,sep=","))
test = test[,1:4]
data.train<- merge(train, bids,  by.x="bidder_id",by.y="bidder_id")
data.test <- merge(test, bids, by.x="bidder_id",by.y="bidder_id")
test.ly = data.frame(read.csv(file="new_test_05_27.csv",head=TRUE,sep=","))
train.ly = data.frame(read.csv(file="new_train_05_27.csv",head=TRUE,sep=","))


#features not scaled data from ly
features.ly = data.frame(read.csv(file="features_0605.csv"))


bid_num = features.ly$bid_num
auc_num = features.ly$auc_num
uniq_device = features.ly$uniq_device
uniq_country = features.ly$uniq_country
uniq_ip = features.ly$uniq_ip
uniq_url = features.ly$uniq_url
mean_auc_bid = as.numeric(as.character(features.ly$mean_auc_bid))
std_auc_bid = as.numeric(as.character(features.ly$std_auc_bid))
mean_speed = as.numeric(as.character(features.ly$mean_speed))
std_speed = as.numeric(as.character(features.ly$std_speed))

mean_bid_time_percent = as.numeric(as.character(features.ly$mean_bid_time_percent))
std_bid_time_percent = as.numeric(as.character(features.ly$std_bid_time_percent))
winning_rate = as.numeric(as.character(features.ly$winning_rate))
parallel_bid = as.numeric(as.character(features.ly$parallel_bid))
consecutive_bid = as.numeric(as.character(features.ly$consecutive_bid))

bids_over_auc = as.numeric(as.character (features.ly$bids_over_auc))
bids_over_device = as.numeric(as.character (features.ly$bids_over_device))
bids_over_country = as.numeric(as.character (features.ly$bids_over_country))
bids_over_ip = as.numeric(as.character (features.ly$bids_over_ip))
bids_over_url = as.numeric(as.character (features.ly$bids_over_url))

auc_over_device = as.numeric(as.character(features.ly$auc_over_device))
auc_over_country = as.numeric(as.character(features.ly$auc_over_country))
auc_over_ip = as.numeric(as.character(features.ly$auc_over_ip))
auc_over_url = as.numeric(as.character(features.ly$auc_over_url))
device_over_ip = as.numeric(as.character (features.ly$device_over_ip))
country_over_ip = as.numeric(as.character (features.ly$country_over_ip))

best = data.frame(read.csv(file="best_score_ly.csv",head=TRUE,sep=","))
best = merge(best,submit,by.x = "bidder_id",by.y = "bidder_id", all.y=T)
best = best[order(best$id),]

features.n = data.frame(bid_num,auc_num,uniq_device,uniq_country,uniq_ip,uniq_url,mean_auc_bid,std_auc_bid,mean_speed,std_speed,
mean_bid_time_percent,std_bid_time_percent,winning_rate,parallel_bid,consecutive_bid,
bids_over_auc,bids_over_device,bids_over_country,bids_over_ip,bids_over_url,
auc_over_device,auc_over_country,auc_over_ip,auc_over_url,device_over_ip,country_over_ip)
feature.n = log(features.n+0.5)
features.n = scale(features.n,center=TRUE,scale=TRUE)

bidder_id = features.ly$bidder_id
features.n = data.frame(features.n,bidder_id)

test.n = merge(features.n,test,by.x="bidder_id",by.y="bidder_id")
train.n = merge(features.n, train, by.x="bidder_id",by.y="bidder_id")

outcome =as.factor(train.n$outcome)
train.n=data.frame(train.n[,2:27],outcome)
#test.n=data.frame(test.n[,2:14])

#logistic regression

fm = formula(outcome ~ bid_num | auc_num| uniq_device| uniq_country | uniq_ip | uniq_url | mean_auc_bid|bids_over_device|bids_over_country| bids_over_ip| bids_over_url) 
train.n <- mlogit.data(train.n, choice = "outcome", varying=c(1:11), shape="wide")
 
#fit1 = mnlogit(fm, train.n,ncores = 2)
fit1 = glm(outcome ~.+bid_num:auc_num  ,family = binomial(logit),data=train.n)

#fit1 = glm(as.factor(outcome) ~ auc_num+bid_num:auc_num +std_auc_bid + auc_over_device+auc_over_url+ country_over_ip,family = #binomial(logit),data=train.n)
logistic.reg <- step(fit1, outcome ~. , data=train.n, family="binomial")
summary(logistic.reg)

pred1 = predict(logistic.reg,test.n,type="response")
pred1 = data.frame(test.n$bidder_id,pred1)
out1 = merge(pred1,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out1 = out1[order(out1$id),]
out1$pred1[is.na(out1$pred1)] <- 0
#out1$pred1 = as.numeric(as.character(out1$pred1))
    
write.table(out1,"~/Documents/kaggle/facebook human and robot/out1.txt", sep="\t")


#newdata = data.frame(bid_num,auc_num,uniq_device,uniq_country,uniq_ip,uniq_url,mean_auc_bid,std_auc_bid,
#mean_speed,std_speed,bids_over_device,bids_over_country,bids_over_ip,bids_over_url,device_over_ip,country_over_ip)
#fit1$coef[12]*newdata[,11] is deleted (bids_over_auc)
#newdata = as.matrix(newdata)

#exp1 = exp(fit1$coef[1]+fit1$coef[2]*newdata[,1]+fit1$coef[3]*newdata[,2]+fit1$coef[4]*newdata[,3]+fit1$coef[5]*newdata[,4]
#+fit1$coef[6]*newdata[,5]+fit1$coef[7]*newdata[,6]+fit1$coef[8]*newdata[,7]+fit1$coef[9]*newdata[,8]+fit1$coef[10]*newdata[,9]
#+fit1$coef[11]*newdata[,10]+fit1$coef[12]*newdata[,12]+fit1$coef[13]*newdata[,13]+fit1$coef[14]*newdata[,14]+fit1$coef[15]*newdata[,15]+fit1$coef[16]*newdata[,16]+fit1$coef[17]*newdata[,17])

#out1 = exp1/(1+exp1)

#out = data.frame(out2,test.ly)

#neutral network
TrainData = sample(1:1984,1000)
ValData = setdiff(1:1984,TrainData)
train.n.val = train.n[TrainData,]
val.n = train.n[ValData,]

ideal <- class.ind(train.n$outcome)

#cross validation

fit2 = nnet(train.n[TrainData,-22], ideal[TrainData,], size=8, softmax=TRUE,maxit=1000)
pred2 = predict(fit2, train.n[ValData,-22], type="class")
#or we can use type="raw" or type="class"
#calculate sucess rate
true = train.n[ValData,22]
count = rep(0,length(true))
for (i in 1:length(true)){
	if (pred2[i]==true[i]){
		count[i]=1
	} 
	else count[i]=0
}

sucess.rate = sum(count)/length(true)

#test and prediction
fit2 = nnet(train.n[,-22],ideal, size=8, softmax=TRUE, maxit = 1000)
#pred2 =  predict(fit2, train.n[ValData,-14], type="class")

pred2 =  predict(fit2, test.n[,2:22], type="raw")

pred2 = data.frame(test.n$bidder_id,pred2[,2])

out2 = merge(pred2,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out2 = out2[order(out2$id),]
out2[,2][is.na(out2[,2])] <- 0
#out2$pred2 = as.numeric(as.character(out2$pred2))
    
write.table(out2,"~/Documents/kaggle/facebook human and robot/out2.txt", sep="\t")

 #support vector machine (SVM)
   rbf <- rbfdot(sigma=0.1)
   SVM <- ksvm(outcome~.,data=train.n[TrainData,],type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)          
   pred3 = predict(SVM, train.n[ValData,-22], type="probabilities") 
   pred3 = predict(SVM, train.n[ValData,-22], type="response")          
true = train.n[ValData,22]
count = rep(0,length(true))
for (i in 1:length(true)){
	if (pred3[i]==true[i]){
		count[i]=1
	} 
	else count[i]=0
}

sucess.rate = sum(count)/length(true)

 SVM <- ksvm(outcome~.,data=train.n,type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)          
 pred3 = predict(SVM, test.n[,2:22], type="probabilities")
 pred3 = data.frame(test.n$bidder_id,pred3[,2])   
 out3 = merge(pred3,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out3 = out3[order(out3$id),]

out3$pred3[is.na(out3$pred3)] = 0
#out3$pred3 = as.numeric(as.character(out3$pred3))
write.table(out3,"~/Documents/kaggle/facebook human and robot/out3.txt", sep="\t")

#randome forest
#five fold cross validation


randome.forest = randomForest(as.factor(outcome[TrainData]) ~ ., data=train.n[TrainData,-22], ntree=20, nodesize=5, mtry=9)
pred4 = predict(randome.forest, test.n[ValData,2:22],type="response")
#or we can use type="raw" or type="class"
#calculate sucess rate
true = train.n[ValData,22]
count = rep(0,length(true))
for (i in 1:length(true)){
	if (pred4[i]==true[i]){
		count[i]=1
	} 
	else count[i]=0
}

sucess.rate = sum(count)/length(true)




#calculate success rate by 5 fold cross validation
TrainData = sample(1:1984,1000)
ValData = setdiff(1:1984,TrainData)
train.n.val = train.n[TrainData,]
val.n = train.n[ValData,]

ideal <- class.ind(train.n$outcome)


n = length(outcome)
ind <- sample(10, n, replace=TRUE)
sr = rep(0, 10) 
SR = rep(0,10)

for (i in 1:10){ 
    randome.forest = randomForest(train.n$outcome[ind=!i]~.,data=train.n[ind!=i,-27],ntree=1000,nodsize=5,importance=TRUE)
    pred = predict(randome.forest, train.n[ind==i,1:26],type="response") 
    for (j in 1:length(outcome[ind==i])){
    	if (pred[j]==true[j]){
    		count[j]=1
    	}
    	else count[j]=0
    	    }
    sr[i] = sum(count)/length(true)

  } 
 SR = mean(sr)
 
 #ROC cross validation
 for (i in 1:10){ 
    randome.forest = randomForest(train.n$outcome[ind!=i]~.,data=train.n[ind!=i,-27],ntree=1000,nodsize=5,importance=TRUE)
    pred = predict(randome.forest, train.n[ind==i,1:26],type="prob")
    pred = pred[,2]    
    out = cvAUC(as.vector(pred),train.n$outcome[ind==i])
    sr[i] = out$cvAUC
  } 


#randome.forest = randomForest(as.factor(outcome) ~ ., data=train.n[,-22], ntree=5000, nodesize=5, mtry=9,importance=TRUE)

randome.forest = randomForest(as.factor(outcome) ~ ., data=train.n[,-27], ntree=2000, nodesize=5,importance=TRUE)
pred4 = predict(randome.forest, test.n[,2:27],type="prob")         
pred4 = data.frame(test.n$bidder_id,pred4[,2])

out4 = merge(pred4,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out4 = out4[order(out4$id),]
out4[,2][is.na(out4[,2])] <- 0
#out4$pred4 = as.numeric(as.character(out4$pred4))
write.table(out4,"~/Documents/kaggle/facebook human and robot/out4.txt", sep="\t")


 #conditional tree
conditional.tree= ctree(as.factor(outcome) ~ ., data=train.n[,-22]) 
pred5 = treeresponse(conditional.tree, test.n[,2:22])

pred5 = data.frame(test.n$bidder_id,pred5)

out5 = merge(pred5,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out5 = out5[order(out5$id),]
out5[,2][is.na(out5[,2])] <- 0
table(pred5)
#out4$pred4 = as.numeric(as.character(out4$pred4))


#PCA in R
pc = prcomp(train.n[,-27],center=TRUE,scale=TRUE)
names(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))

#trunc.train
pc.use = 24
trunc.train = pc$x[,1:pc.use]%*%t(pc$rotation[,1:pc.use])
trunc.train <- scale(trunc.train, center = TRUE , scale=1/pc$scale)
randome.forest = randomForest(as.factor(outcome) ~., data=trunc.train, ntree=1000, nodesize=5,importance=TRUE)

#tunc.test
pc = prcomp(test.n[,2:27],center=TRUE,scale=TRUE)
names(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))
pc.use = 24
trunc.test = pc$x[,1:pc.use]%*%t(pc$rotation[,1:pc.use])
trunc.test <- scale(trunc.test, center = TRUE , scale=1/pc$scale)

pred4 = predict(randome.forest, trunc.test,type="prob") 
pred4 = data.frame(test.n$bidder_id,pred4[,2])

out4 = merge(pred4,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out4 = out4[order(out4$id),]
out4[,2][is.na(out4[,2])] <- 0
#out4$pred4 = as.numeric(as.character(out4$pred4))
write.table(out4,"~/Documents/kaggle/facebook human and robot/out4.txt", sep="\t")


#cross validation for randome forest
for (i in 1:5){ 
    randome.forest = randomForest(outcome[ind!=i]~.,data=trunc.train[ind!=i,],ntree=2000,nodsize=5,importance=TRUE)
    pred = predict(randome.forest, trunc.train[ind==i,],type="prob")
    pred = pred[,2]    
    out = cvAUC(as.vector(pred),train.n$outcome[ind==i])
    sr[i] = out$cvAUC
  } 
randome.forest = randomForest(outcome~.,data=trunc.train,ntree=2000,nodsize=5,importance=TRUE)
pred = predict(randome.forest, trunc.train,type="prob")
pred = pred[,2]


#adaboosting
#adaboost = boosting(train.n$outcome~.,data = as.matrix(train.n[,-27]),mfinal=25,control=rpart.control(maxdepth=3))
#5 fold cross validation of adaboost
n = length(train.n$outcome)
ind <- sample(5, n, replace=TRUE)
sr = rep(0, 5) 
SR = rep(0,5)

for (i in 1:5){ 
    adaboost = ada(train.n$outcome[ind!=i]~.,data=train.n[ind!=i,-27],iter=500,nu=0.05,type="discrete")
    pred = predict(adaboost, train.n[ind==i,1:26],type="prob")
    pred = pred[,2]    
    out = cvAUC(as.vector(pred),train.n$outcome[ind==i])
    sr[i] = out$cvAUC
  } 
  

adaboost = ada(train.n$outcome~.,data=train.n[,-27],iter=500,nu=0.05,type="discrete")
pred6 = predict(adaboost,test.n[,2:27],type="prob")

pred6 = pred6[,2]
#pred6 = predict(adaboost,test.n[,2:27],type="vector")
pred6 = data.frame(test.n$bidder_id,pred6)
out6 = merge(pred6,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out6 = out6[order(out6$id),]
out6[,2][is.na(out6[,2])] <- 0
write.table(out6,"~/Documents/kaggle/facebook human and robot/out6.txt", sep="\t")

#pca for adaboosting
adaboost = ada(train.n$outcome~.,data=data.frame(trunc.train),iter=500,nu=0.1,type="discrete")
pred6 = predict(adaboost,data.frame(trunc.test),type="prob")

#cross validation for randome forest
for (i in 1:5){ 
    adaboost = ada(outcome[ind!=i]~.,data=data.frame(trunc.train[ind!=i,]),iter=500,nu=0.05,type="discrete")
    pred = predict(adaboost, data.frame(trunc.train[ind==i,]),type="prob")
    pred = pred[,2]    
    out = cvAUC(as.vector(pred),train.n$outcome[ind==i])
    sr[i] = out$cvAUC
  } 


  
 
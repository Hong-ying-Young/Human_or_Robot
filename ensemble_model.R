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
install.packages("Metrics")

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
library(Metrics)
setwd("~/Documents/kaggle/facebook human and robot")
#load("~/Documents/kaggle/facebook human and robot/data.RData")
#bids <- data.frame(read.csv(file="bids.csv", head=TRUE, sep=","))
train <- data.frame(read.csv(file="train.csv",head=TRUE, sep=","))
test <- data.frame(read.csv(file="test.csv",head=TRUE, sep=","))
submit = data.frame(read.csv(file="Submission.csv",head=TRUE,sep=","))
test = test[,1:4]
data.train<- merge(train, bids,  by.x="bidder_id",by.y="bidder_id")
data.test <- merge(test, bids, by.x="bidder_id",by.y="bidder_id")
test.ly = data.frame(read.csv(file="new_test_05_27.csv",head=TRUE,sep=","))
train.ly = data.frame(read.csv(file="new_train_05_27.csv",head=TRUE,sep=","))


#features not scaled data from ly
features.ly = data.frame(read.csv(file="features_0608.csv"))


bid_num = features.ly$bid_num
auc_num = features.ly$auc_num
uniq_device = features.ly$uniq_device
uniq_country = features.ly$uniq_country
uniq_ip = features.ly$uniq_ip
uniq_url = features.ly$uniq_url

mean_auc_bid = as.numeric(as.character(features.ly$mean_auc_bid))
std_auc_bid = as.numeric(as.character(features.ly$std_auc_bid))
mean_auc_country =as.numeric(as.character (features.ly$mean_auc_country))
std_auc_country = as.numeric(as.character (features.ly$std_auc_country))
mean_auc_device = as.numeric(as.character (features.ly$mean_auc_device))
std_auc_device = as.numeric(as.character (features.ly$std_auc_device))
mean_auc_ip = as.numeric(as.character (features.ly$mean_auc_ip))
std_auc_ip = as.numeric(as.character (features.ly$std_auc_ip))
mean_auc_url = as.numeric(as.character (features.ly$mean_auc_url))
std_auc_url = as.numeric(as.character (features.ly$std_auc_url))

mean_speed = as.numeric(as.character(features.ly$mean_speed))
std_speed = as.numeric(as.character(features.ly$std_speed))

mean_bid_time_percent = as.numeric(as.character(features.ly$mean_bid_time_percent))
std_bid_time_percent = as.numeric(as.character(features.ly$std_bid_time_percent))
winning_rate = as.numeric(as.character(features.ly$winning_rate))
parallel_bid = as.numeric(as.character(features.ly$parallel_bid))
consecutive_bid = as.numeric(as.character(features.ly$consecutive_bid))

mean_enter_time_percent = as.numeric(as.character (features.ly$mean_enter_time_percent))
std_enter_time_percent = as.numeric(as.character (features.ly$std_enter_time_percent))
mean_exit_time_percent = as.numeric(as.character (features.ly$mean_exit_time_percent))
std_exit_time_percent = as.numeric(as.character (features.ly$mean_exit_time_percent))
median_auc_bid = as.numeric(as.character (features.ly$median_auc_bid))
median_auc_country = as.numeric(as.character (features.ly$median_auc_country))
median_auc_device = as.numeric(as.character (features.ly$median_auc_device))
median_auc_ip = as.numeric(as.character (features.ly$median_auc_ip))
median_auc_url = as.numeric(as.character (features.ly$median_auc_url))

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
auc_over_device,auc_over_country,auc_over_ip,auc_over_url,device_over_ip,country_over_ip,mean_auc_country,std_auc_country,mean_auc_device,
std_auc_device,mean_auc_ip,std_auc_ip,mean_auc_url,std_auc_url,mean_enter_time_percent,std_enter_time_percent,mean_exit_time_percent, 
std_exit_time_percent,median_auc_bid,median_auc_country,median_auc_device,median_auc_ip,median_auc_url)
feature.n = log(features.n+0.5)
features.n = scale(features.n,center=TRUE,scale=TRUE)

bidder_id = features.ly$bidder_id
features.n = data.frame(features.n,bidder_id)

test.n = merge(features.n,test,by.x="bidder_id",by.y="bidder_id")
train.n = merge(features.n, train, by.x="bidder_id",by.y="bidder_id")

outcome =as.factor(train.n$outcome)
train.n=data.frame(train.n[,2:44],outcome)

#PCA in R
pc = prcomp(train.n[,-27],center=TRUE,scale=TRUE)
names(pc)

#trunc.train
pc.use = 24
trunc.train = pc$x[,1:pc.use]%*%t(pc$rotation[,1:pc.use])
#trunc.train <- scale(trunc.train, center = TRUE , scale=1/pc$scale)
#trunc.test
pc = prcomp(test.n[,2:27],center=TRUE,scale=TRUE)
names(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))
pc.use = 24
trunc.test = pc$x[,1:pc.use]%*%t(pc$rotation[,1:pc.use])
#trunc.test <- scale(trunc.test, center = TRUE , scale=1/pc$scale)
out = array(dim=c(4629,5))

for (i in 1:5){
	randome.forest = randomForest(as.factor(outcome) ~., data=trunc.train, ntree=2000, nodesize=5,importance=TRUE)
	pred = predict(randome.forest, trunc.test,type="prob") 
	out[,i]=pred[,2]
}

pred = apply(out,c(1),mean)
which(pred>=0.5)
pred = data.frame(test.n$bidder_id,pred)

out1 = merge(pred,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out1 = out1[order(out1$id),]
out1[,2][is.na(out1[,2])] <- 0
#out4$pred4 = as.numeric(as.character(out4$pred4))
write.table(out1,"~/Documents/kaggle/facebook human and robot/ensemble_rf_pca.txt", sep="\t")






#ensemble for adaboost
for (i in 1:5){
	adaboost = ada(train.n$outcome~.,data=train.n[,-44],iter=500,nu=0.05,type="discrete")
    pred = predict(adaboost,test.n[,2:44],type="prob")
    out[,i]=pred[,2]
}

pred2 = apply(out,c(1),mean)
which(pred2>=0.5)
pred2 = data.frame(test.n$bidder_id,pred2)
out2 = merge(pred2,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
out2 = out2[order(out2$id),]
out2[,2][is.na(out2[,2])] <- 0
#out4$pred4 = as.numeric(as.character(out4$pred4))
write.table(out2,"~/Documents/kaggle/facebook human and robot/multi_ada.txt", sep="\t")



#ensemble for multi-model
    
    randome.forest = randomForest(as.factor(outcome) ~., data=train.n[,-27], ntree=2000, nodesize=5,importance=TRUE)
	pred2 = predict(randome.forest, test.n[,2:27],type="prob")
	pred2 = pred2[,2]
	pred2 = pred2*1.05
	ideal <- class.ind(train.n$outcome)
	
    nnet = nnet(train.n[,-27],ideal, size=5, softmax=TRUE, maxit = 2000)
    pred3 =  predict(nnet, test.n[,2:27], type="raw")
    pred3 = pred3[,2]

pred = data.frame(pred1,pred2)
multi.ensemble = apply(pred,c(1),mean)
which(pred2>=0.5)
multi.ensemble = data.frame(test.n$bidder_id,multi.ensemble)
multi.ensemble = merge(multi.ensemble,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
multi.ensemble = multi.ensemble[order(multi.ensemble$id),]
multi.ensemble[,2][is.na(multi.ensemble[,2])] <- 0
#out4$pred4 = as.numeric(as.character(out4$pred4))
write.table(multi.ensemble,"~/Documents/kaggle/facebook human and robot/multi.ensemble.txt", sep="\t")

#ensemble of five RF
out = array(dim=c(4629,5))

for (i in 1:5){
	randome.forest = randomForest(as.factor(outcome) ~., data=train.n[,-44], ntree=3500, nodesize=5,importance=TRUE)
	pred = predict(randome.forest, test.n[,2:44],type="prob") 
	out[,i]=pred[,2]
}

pred = apply(out,c(1),mean)
which(pred>=0.5)
pred = data.frame(test.n$bidder_id,pred)

emsemble.rf = merge(pred,submit,by.x = "test.n.bidder_id",by.y = "bidder_id", all.y=T)
emsemble.rf = emsemble.rf[order(emsemble.rf$id),]
emsemble.rf[,2][is.na(emsemble.rf[,2])] <- 0
#out4$pred4 = as.numeric(as.character(out4$pred4))
write.table(emsemble.rf,"~/Documents/kaggle/facebook human and robot/emsemble.rf.txt", sep="\t")

install.packages("foreach")
library(foreach)

#imporve prediction by bagging
length_divisor<-4
iterations<-1000
predictions = foreach(m=1:iterations,.combine=cbind) %do% {
training_positions <- sample(nrow(train.n), size=floor((nrow(train.n)/length_divisor)))
train_pos<-1:nrow(train.n) %in% training_positions
randomeForest = randomForest(as.factor(outcome[train_pos]) ~., data=train.n[train_pos,-44], ntree=3500, nodesize=5,importance=TRUE)
pred = predict(randome.forest, test.n[,2:44],type="prob") 
}

predictions<-rowMeans(predictions)


#5 fold corss validation for randome forest
ideal <- class.ind(train.n$outcome)
n = length(outcome)
ind <- sample(5, n, replace=TRUE)
sr = rep(0, 5) 
#logloss= rep(0,5)
SR = rep(0,5)
#LogLoss = rep(0,5)

 for (i in 1:5){ 
    randome.forest = randomForest(train.n$outcome[ind!=i]~.,data=train.n[ind!=i,-44],ntree=3500,nodsize=5,importance=TRUE)
    pred = predict(randome.forest, train.n[ind==i,1:43],type="prob")
    pred = pred[,2]    
    out1 = cvAUC(as.vector(pred),train.n$outcome[ind==i])
    #out2 = logLoss(train.n$outcome[ind==i],as.vector(pred))
    sr[i] = out1$cvAUC
    #logloss[i] = out2
  } 
 
 SR = mean(sr)

#5 fold cross validation for adaboosting    
 for (i in 1:5){ 
 	adaboost = ada(train.n$outcome[ind!=i]~.,data=train.n[ind!=i,-44],iter=500,nu=0.05,type="discrete")
    pred = predict(adaboost, train.n[ind==i,1:43],type="prob")
    pred = pred[,2]    
    out1 = cvAUC(as.vector(pred),train.n$outcome[ind==i])
    #out2 = logLoss(train.n$outcome[ind==i],as.vector(pred))
    sr[i] = out1$cvAUC
    #logloss[i] = out2
  } 
 
 SR = mean(sr)
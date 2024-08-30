
##########################Bagging ##############################
################################################################
install.packages("adabag")
#install.packages("rpart")
#install.packages("ipred")
library(ipred)
library(rpart)
library(adabag)
#bagging函数“内嵌”模型是分类树，其参数control应为rpart函数的参数

set.seed(123)

# read data
TelephoneData<-read.csv("/Users/zhao/Desktop/大三课作业/数据挖掘实验/Telephone.csv",head = TRUE,fileEncoding = 'GBK')
head(TelephoneData)
typeof(TelephoneData$流失)

ctrl<-rpart.control(maxdepth=30)

# testing and training
N <- length(TelephoneData[,1])#看第一列长度
N
sub <- sample(1:N,2*N/3)
train=TelephoneData[sub,]
TelephoneData$流失<-as.factor(TelephoneData$流失)

# implement bagging by the package of ipred
set.seed(12)
# nbagg:an integer giving the number of bootstrap
#replications. control 同rpart 设置
(bagModel_ipred<-bagging(流失~.,data=TelephoneData,nbagg=25,coob=TRUE,control=ctrl)) #以流失为样本集

bagModel_ipred$importance

(sort(bagModel_ipred$importance))

bagModel_ipred_pre<-predict.bagging(bagModel_ipred,newdata=TelephoneData,type="class")
bagModel_ipred_pre

bagModel_ipred_pre$confusion

bagModel_ipred_pre$error

(Conf_ipred<-table(TelephoneData$流失,bagModel_ipred_pre$class))

(error_ipred<-(sum(Conf_ipred)-sum(diag(Conf_ipred)))/sum(Conf_ipred))

# implement bagging by the package of adabag
set.seed(1234)

#Bagging with different numbers of classifiers

errorBagging <- as.numeric()

for(i in 1:25){
  
  bagModel<- bagging(流失~., data=TelephoneData, mfinal = i)
  
  bagModel.pred <- predict.bagging(bagModel,newdata = TelephoneData)
  
  errorBagging[i] <- bagModel.pred$error
}
errorBagging
baggingBest<-which.min(errorBagging)
baggingBest

#install.packages("ggplot2")
library(ggplot2) 
errorBagging=as.data.frame(errorBagging)

p <- ggplot(errorBagging,aes(x=1:25,y=errorBagging))+
  
  geom_line(colour="red", linetype="dashed", #size 
            linewidth= 1)+
  
  geom_point(size=3, shape=18)+
  
  ylim(0.13,0.45) +
  
  xlab("the number of basic classifiers")+
  
  theme_bw()+
  
  theme(panel.grid = element_blank())+
  
  theme(axis.title = element_text(face = "bold"))
p

#bagModel <- bagging(流失~.,data=TelephoneData[sub,],mfinal=9,control=ctrl)
#bagModel <- bagging.cv(流失~.,data=TelephoneData,v=10,mfinal=25,control=ctrl)
#print(bagModel)

bagModel.pred <- predict.bagging(bagModel,newdata=TelephoneData,newmfinal = baggingBest)

bagModel.pred

bagModel.pred$prob

(bagConfusion<-bagModel.pred$confusion)
(bagerror<-bagModel.pred$error)

sensitivity=bagConfusion[1,1]/(bagConfusion[1,1]+bagConfusion[1,2])
sensitivity
#Sensitivity=Recall=TP/(TP+FN)

##########################RandomForest ##############################
################################################################
#install.packages("randomForest")
library(randomForest)
set.seed(12345)
#randomforesModel<-randomForest(流失~., data=TelephoneData,important=TRUE,proximity=TRUE,mtry=k(k is m),ntree=400(size of forest))
randomforesModel<-randomForest(流失~., data=TelephoneData,important=TRUE,proximity=TRUE)
#mtry: 各节点的输入变量个数，默认根号p
#ntree: 所包含的决策树个数，默认500
print(randomforesModel)
#(classification only) the confusion matrix of the
#prediction (based on OOB data).
randomforesModel$confusion
#随机森林各个决策树基于OOB的整体预测错误率以及
#对各个类别的预测错误率
randomforesModel$err.rate

head(randomforesModel$oob.times)#各观测作为oob的次数

#各个样本作为OOB的次数
head(randomforesModel$votes)

par(mfrow=c(1,1),mar=c(5,5,3,1))#mar调整绘图区域距离外围框线的距离
plot(randomforesModel,main="随机森林OOB错判率和决策树棵数")
#plot( )的绘图数据是randomForest()返回值中的err.rate.黑色线是整体判错率，红色线是对NO类预测的错判率，
#绿色线是对YES类预测的判错率。模型对No类的预测效果好于Yes类和整体。
plot(margin(randomforesModel),type="h",main="边界点",xlab="观测序列",ylab="比率差")
#margin（）考察处于边界附近的点和判错情况
#附近的点的定义依据：投票给正确类别（该观测说属的实际类别）的树的比率-
#                    投票给众数类（除正确类别以外的其他众数类别）的树的比率
#之差为正表示预测正确，为负表示预测错误，差的绝对值越小，越接近于零，表明该观测处在分类边界上


#training &  testing
ind <- sample(2, nrow(TelephoneData), replace = TRUE, prob=c(0.8, 0.2))

test.pred <- predict(randomforesModel, TelephoneData[ind == 2,])
(confusion.random<-table(observed = TelephoneData[ind==2, "流失"], predicted = test.pred))

train.rf <- randomForest(流失 ~ ., data=TelephoneData[ind == 1,])
test.pred <- predict(train.rf, TelephoneData[ind == 2,])
(confusion.random<-table(observed = TelephoneData[ind==2, "流失"], predicted = test.pred))
(error.random<-(sum(confusion.random)-sum(diag(confusion.random)))/sum(confusion.random))

importance(randomforesModel)# type可以是1，也可以是2，
#用于判别计算变量重要性的方法，1表示使用精度平均较少值作为度量标准；
#2表示采用节点不纯度的平均减少值最为度量标准。值越大说明变量的重要性越强；
par(mfrow=c(1,1))
barplot(randomforesModel$importance[,1],main="输入变量重要性测度(预测精度变化)指标柱形图")
box() 
varImpPlot(randomforesModel,main="输入变量重要性测度散点图",cex=0.7)  #cex字体大小

randomforesModel$importance
is.matrix(randomforesModel$importance)

#find the best value of mtry.
#默认情况下数据集变量个数的二次方根（分类模型）或三分之一（预测模型）。
#决策树各节点的输入变量个数
n=length(names(TelephoneData))
n
set.seed(100)
for(i in 1:(n-1)){mtryFit<-randomForest(流失~.,data=TelephoneData,mtry=i)
err=mean(mtryFit$err.rate)
print(err)}

##########################Adaboosting##############################
################################################################

ctrl<-rpart.control(maxcompete=4, maxdepth=5)
set.seed(1234)
#TelephoneData$流失<-as.factor(TelephoneData$流失)
BoostModel<-boosting(流失~., data=TelephoneData,boos=TRUE,mfinal=30,coeflearn = "Breiman",control = ctrl)
#mfinal:自举次数，默认100，boos=True: 每次自举过程都调整各观测进入训练样本集的权重
#coeflearn: 各模型的权重计算方法，Breiman or Freund

pre.boost<-predict(BoostModel,TelephoneData)
(confusion.boost<-pre.boost$confusion)  #混淆矩阵存放在一个叫confusion 的子对象中

(error.boost<-(sum(confusion.boost)-sum(diag(confusion.boost)))/sum(confusion.boost))

# training & testing
M <- length(TelephoneData[,1])
sub <- sample(1:M,2*M/3)

train.adaboost <- boosting(流失 ~.,data=TelephoneData[sub, ],mfinal=30, coeflearn="Freund",
                           control=rpart.control(maxdepth=3))

test.adaboost.pred <- predict.boosting(train.adaboost,newdata=TelephoneData[-sub, ])

test.adaboost.pred$confusion
test.adaboost.pred$error


#comparing error evolution in training and test set
#errorevol()计算误差演变过程Shows the error evolution of the ensemble
errorevol(train.adaboost,newdata=TelephoneData[sub, ])->evol.train
errorevol(train.adaboost,newdata=TelephoneData[-sub, ])->evol.test

plot.errorevol(evol.test,evol.train)

# find the best mfinal by loop

index <- sample(nrow(TelephoneData),0.7*nrow(TelephoneData))

train <- TelephoneData[index,]

test <-TelephoneData[-index,]

error <- as.numeric()

for(i in 1:30){
  
  data.adaboost <- boosting(流失~., data=train, mfinal=i)
  
  data.pred <- predict.boosting(data.adaboost,newdata = test)
  
  error[i] <- data.pred$error
  print(error[i])
}

best=which.min(error)
best

#ggplot():Default dataset to use for plot. 
#If not already a data.frame, will be converted 
#to one.ggplot()数据必须是数据框形式
# ggplot2提供了8种内置主题.其中 theme_bw()：白色背景，灰色网格，黑色边框
#theme(axis.title...)设置坐标轴标题的格式
#theme(panel.grid = element_blank())

error <- as.data.frame(error)
print(error)

library(ggplot2)

p <- ggplot(error,aes(x=1:30,y=error))+
  
  geom_line(colour="red", linetype="dashed",size = 1)+
  
  geom_point(size=3, shape=18)+
  
  ylim(0.13,0.45) +
  
  xlab("the number of basic classifiers")+
  
  theme_bw()+
  
  theme(panel.grid = element_blank())+
  
  theme(axis.title = element_text(face = "bold"))
p

train.adaboost <- boosting(流失 ~.,data=TelephoneData[sub, ],mfinal=best, coeflearn="Freund",
                           control=rpart.control(maxdepth=3))

train.adaboost.pred <- predict.boosting(train.adaboost,newdata=TelephoneData[sub, ])
train.adaboost.pred$confusion
train.adaboost.pred$error

test.adaboost.pred <- predict.boosting(train.adaboost,newdata=TelephoneData[-sub, ])

test.adaboost.pred$confusion
test.adaboost.pred$error


#*********************  ROC by pROC ********************
#******************************************************
#install.packages("pROC")

# plot ROC curve for Bagging by pROC
library(pROC)

bagModel.pred <- predict.bagging(bagModel,newdata=TelephoneData[-sub,])
bagModel.pred

bagModel.pred.class<-as.numeric(bagModel.pred$class)

roc.bag <- roc(TelephoneData[-sub,]$流失,bagModel.pred.class)
par(mfrow=c(1,3))
plot(roc.bag, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main=' ROC curve for Bagging')


# plot ROC curve for RandomForest by pROC

test.pred <- predict(randomforesModel, TelephoneData[ind == 2,])

#roc() requires numeric vector
test.pred.RF<-as.numeric(test.pred)

RF.roc<- roc(TelephoneData[ind == 2,]$流失, test.pred.RF,plot=TRUE, add=TRUE, percent=roc.bag$percent)

plot(RF.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="pink", print.thres=TRUE,main=' ROC curve for RF')

# plot ROC curve for Adaboosting by pROC
boosting.pred<-as.numeric(test.adaboost.pred$class)
#boosting.roc<- roc(TelephoneData[-sub,]$流失, boosting.pred,plot=TRUE, add=TRUE, percent=roc.bag$percent)

boosting.roc<- roc(TelephoneData[-sub,]$流失, boosting.pred)
plot(boosting.roc,print.auc = T, auc.polygon = T, grid = c(0.1,0.2), grid.col=c("green", "red"), 
     max.auc.polygon=TRUE, auc.polygon.col="green", print.thres=TRUE, main='ROC Curve for Adaboost')


#**************************ROC by ROCR***************************
#*********************************************************
#install.packages("ROCR")
# plot ROC curve by ROCR
par(mfrow=c(1,1))
library(ROCR)

#bagging
bag.predict<-predict(bagModel,newdata=TelephoneData,type="prob")

#typeof(TelephoneData$流失)
#typeof(bag.predict$class)

#as.integer(bag.predict$class)
#as.factor(bag.predict$class)
#as.factor(TelephoneData$流失)

bag.predict = prediction(bag.predict$prob[,2],TelephoneData$流失)

#use performance() to compute tpr and  fpr，needing return value of prediction()
bag.perf<-performance(bag.predict,"tpr","fpr")  

#test.perf<-performance(test.pred,"tpr","fpr")

plot(bag.perf,main="ROC Curve",col = "blue", lty = 1, lwd = 3)
abline(a= 0, b=1)

# ROC for randpomForest
(pre.random<-predict(randomforesModel,TelephoneData,type="prob"))

random.pred = prediction(pre.random[,2],TelephoneData$流失) 

#use performance() to compute tpr and  fpr，needing return value of prediction()
random.perf<-performance(random.pred,"tpr","fpr")  
par(new=T)
plot(random.perf,main="ROC Curve",col = "green", lty = 1, lwd = 3)

#  ROC curve for Boosting
boosting.pred = prediction(pre.boost$prob[,2],TelephoneData$流失) 

#use performance() to compute tpr and  fpr，needing return value of prediction()
boosting.perf<-performance(boosting.pred,"tpr","fpr")  
par(new=T)
plot(boosting.perf,main="ROC Curve",col = "red", lty = 1, lwd = 3)

legend("bottomright",legend=c("bagging","adaboosting","randomForest"),bty="n",lty=c(1,1),col=c("blue","red","green"),cex=0.6)    


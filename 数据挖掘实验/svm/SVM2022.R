

#####   SVM e1071 kernlab:ksvm(), klaR:svmlight(), ##########
#####              svmpath:svmpath()               ##########
#############################################################
#install.packages("e1071")
#svm(formula, data, scale=TRUE/FALSE, type=c-classification,kernel=linear/polynomial
#/radial basis,gamma=0.1,degree==d(多项式核函数的阶数d),cost=C,
#na.action=na.omit/na.fail(遇缺失报错)）
#tune.svm(gamma=c(参数向量），cost=c(参数向量)...)
# SVM return： 支持向量观测在所有变量上的取值
#              index： the number of SV
#              decision.value: 

#############  linear separable  ##########
###########################################
set.seed(12345)
#创建一个服从正态分布的矩阵
x<-matrix(rnorm(n=40*2,mean=0,sd=1),ncol=2,byrow=TRUE)
x
y<-c(rep(-1,20),rep(1,20))#-1重复20次，1重复20次，给出类标号。rep=replicate
y
x[y==1,]<-x[y==1,]+1.5# 对后二十个观测值加上1.5
data_train<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y))  #traning data
data_train
#生成训练样本集,前二十个观测标签为-1，后二十个位+1

#testing data
x<-matrix(rnorm(n=20,mean=0,sd=1),ncol=2,byrow=TRUE)
x
y<-sample(x=c(-1,1),size=10,replace=TRUE)
y
x[y==1,]<-x[y==1,]+1.5
x
data_test<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y)) 
win.graph(width=16, height=16,pointsize=10)
plot(data_train[,1:2],col=as.integer(as.vector(data_train[,3]))+2,pch=8,cex=0.7,main="训练样本集-1和+1类散点图")
#data_train 第三列+2，产生不同的颜色，8是*

library("e1071")
#cost 用于指定C scale:b标准化
SvmFit<-svm(Fy~.,data=data_train,type="C-classification",kernel="linear",cost=10,scale=FALSE)

summary(SvmFit)
SvmFit$index#给出SV的编号
win.graph(width=16, height=16,pointsize=10)
plot(x=SvmFit,data=data_train,formula=Fx1~Fx2,svSymbol="#",dataSymbol="*",grid=100)
SvmFit<-svm(Fy~.,data=data_train,type="C-classification",kernel="linear",cost=0.1,scale=FALSE)
summary(SvmFit)


############## 10 folds validation to choose C ###############

set.seed(12345)
tObj<-tune.svm(Fy~.,data=data_train,type="C-classification",kernel="linear",
               cost=c(0.001,0.01,0.1,1,5,10,100,1000),scale=FALSE)
summary(tObj)
BestSvm<-tObj$best.model
summary(BestSvm)

win.graph(width=16, height=16,pointsize=10)
plot(x=BestSvm,data=data_train,formula=Fx1~Fx2,svSymbol="$",dataSymbol="*",grid=100)#等高线

yPred<-predict(BestSvm,data_test)
(ConfM<-table(yPred,data_test$Fy))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))

############## non linear separable ############# 

set.seed(12345)
x<-matrix(rnorm(n=400,mean=0,sd=1),ncol=2,byrow=TRUE)
x
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(1,150),rep(2,50))
y
data<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y))
data
flag<-sample(1:200,size=100)
data_train<-data[flag,]
data_test<-data[-flag,]

win.graph(width=16, height=16,pointsize=10)
plot(data_train[,1:2],col=as.integer(as.vector(data_train[,3])),pch=8,cex=0.7,main="训练样本集散点图")

#corss validation
library("e1071")
set.seed(12345)
tObj<-tune.svm(Fy~.,data=data_train,type="C-classification",kernel="radial",
               cost=c(0.001,0.01,0.1,1,5,10,100,1000),gamma=c(0.5,1,2,3,4),scale=FALSE)

win.graph(width=16, height=16,pointsize=10)
plot(tObj,xlab=expression(gamma),ylab="损失惩罚参数C",
     main="不同参数组合下的预测错误率",nlevels=10,color.palette=terrain.colors)

summary(tObj)
BestSvm<-tObj$best.model
summary(BestSvm)

win.graph(width=16, height=16,pointsize=10)
plot(x=BestSvm,data=data_train,formula=Fx1~Fx2,svSymbol="#",dataSymbol="*",grid=100)

yPred<-predict(BestSvm,data_test)
(ConfM<-table(yPred,data_test$Fy))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))


############## multiple catagory  one VS one ################

set.seed(12345)
x<-matrix(rnorm(n=400,mean=0,sd=1),ncol=2,byrow=TRUE)
x
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
#纵向合并
x<-rbind(x,matrix(rnorm(n=100,mean=0,sd=1),ncol=2,byrow=TRUE))
y<-c(rep(1,150),rep(2,50))
y<-c(y,rep(0,50))
x[y==0,2]<-x[y==0,2]+3
data<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y))
data

win.graph(width=16, height=16,pointsize=10)
plot(data[,2:1],col=as.integer(as.vector(data[,3]))+1,pch=8,cex=0.7,main="训练样本集散点图")

library("e1071")
set.seed(12345)
tObj<-tune.svm(Fy~.,data=data,type="C-classification",kernel="radial",
               cost=c(0.001,0.01,0.1,1,5,10,100,1000),gamma=c(0.5,1,2,3,4),scale=FALSE)
BestSvm<-tObj$best.model
summary(BestSvm)
plot(x=BestSvm,data=data,formula=Fx1~Fx2,svSymbol="#",dataSymbol="*",grid=100)
SvmFit<-svm(Fy~.,data=data,type="C-classification",kernel="radial",cost=5,gamma=1,scale=FALSE)

#查看各观测点的决策函数值
head(SvmFit$decision.values)

yPred<-predict(SvmFit,data)
(ConfM<-table(yPred,data$Fy))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))

########################### kernlab ##############################

#install.packages("kernlab")
library(kernlab)

data(iris)
irismodel <- ksvm(Species ~ ., data=iris,
                  
                  type="C-bsvc", kernel="rbfdot",
                  
                  kpar=list(sigma=0.1), C=10,
                  
                  prob.model=TRUE) #training
irismodel


################### e1071 multiple catagories ##################
set.seed(12345)
library("e1071")
data(iris)
index = sample(2, nrow(iris), replace = TRUE, prob = c(0.7,0.3))
TrainData = iris[index == 1,]
TestData = iris[index == 2,]

model<-tune.svm(Species~.,data=TrainData,type="C-classification",kernel="radial",gamma=10^(-6:-3),cost=10^(-3:2) )
model
plot(model,xlab=expression(gamma),ylab="损失惩罚参数C",
     main="不同参数组合下的预测错误率",nlevels=10,color.palette=terrain.colors)
summary(model)
BestSvm<-model$best.model
summary(BestSvm)

#predict testing data and compute confusion matrix
Pred<-predict(BestSvm,TestData)
ConfM<-table(Pred,TestData$Species)
ConfM
Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM)
Err

# predict new data
newdata<-data.frame(Sepal.Length=5, Sepal.Width=2.3, Petal.Length=3.3, Petal.Width=1)
predict(BestSvm,newdata)

################### e1071 binary classes##################
#####前三个月的消费行为数据，预测未来一个月是否有订单成交#######

Tmall_train<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/svm/天猫_Train_1.txt",header=TRUE,sep=",")
Tmall_train$BuyOrNot<-as.factor(Tmall_train$BuyOrNot)
Tmall_train

win.graph(width=16, height=16,pointsize=10)
plot(Tmall_train[,3:4],col=as.integer(as.vector(data_train[,3]))+2,pch=8,cex=0.7,main="散点图")

set.seed(12345)
library("e1071")
tObj<-tune.svm(BuyOrNot~.,data=Tmall_train,type="C-classification",kernel="radial",gamma=10^(-6:-3),cost=10^(-3:2))

win.graph(width=16, height=16,pointsize=10)
plot(tObj,xlab=expression(gamma),ylab="损失惩罚参数C",
     main="不同参数组合下的预测错误率",nlevels=10,color.palette=terrain.colors)

BestSvm<-tObj$best.model
summary(BestSvm)
Tmall_test<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/svm/天猫_Test_1.txt",header=TRUE,sep=",")
Tmall_test$BuyOrNot<-as.factor(Tmall_test$BuyOrNot)
yPred<-predict(BestSvm,Tmall_test)
(ConfM<-table(yPred,Tmall_test$BuyOrNot))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))



#消费活跃度（BuyDNactDN）=有成交天数/活跃天数
#活跃度（ActDNTotalDN)=活跃天数/研究周期天数
#成交有效度（BuyBBrand=成交品牌数量/浏览的品牌数
#活动有效度（BuyHit)=订单成交数/商品点击次数
#BuyOrNot:1表示有成交，0表示无成交

######################Classification of the customers of Tianmao###############
########Package: class，caret, kknn####################
#install.packages("class")
library(class)

# Read data 
Tmall_train<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/knn/天猫_Train_1.txt",header=TRUE,sep=",")
Tmall_train
head(Tmall_train)
str(Tmall_train)
#table(Tmall_train$ BuyOrNot)
#prop.table(table(Tmall_train$ BuyOrNot))
round(prop.table(table(Tmall_train$ BuyOrNot))*100,digits = 1) 
#小数点保留一位,查看两个类别样本数量所占百分比
Tmall_test<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/knn/天猫_Test_1.txt",header=TRUE,sep=",")
round(prop.table(table(Tmall_test$ BuyOrNot))*100,digits = 1) #小数点保留一位
Tmall_train.lable<-Tmall_train[,1] 
Tmall_test.lable<-Tmall_test[,1] 
# nomalization: min-max
normalize<-function(x) {return((x-min(x))/(max(x)-min(x)))}
#test the function
normalize(c(1,2,3,4,5))
# 把normaize()函数应用到数据框Tamll_trian/Tmall_test第 2~5列，并把产生的结果列表转化成数据框。
(Tmall_train.n<-as.data.frame(lapply(Tmall_train[,2:5],normalize)))
#lapply : 遍历列表向量内的每个元素，并且使用指定函数来对其元素进行处理。
#返回列表向量。R语言提供了批量处理函数，可以循环遍历某个集合内的所有或部分元素，
#以简化操作,函数底层是通过C来实现的,比起传统的for,while常常能获得更好的性能。

summary(Tmall_train.n)
(Tmall_test.n<-as.data.frame(lapply(Tmall_test[,2:5],normalize)))
summary(Tmall_test.n)


Tmall_train.lable<-as.factor(Tmall_train.lable)
Tmall_test.lable<-as.factor(Tmall_test.lable)

set.seed(123456)
errRatio<-vector()   
for(i in 1:30){
  KnnFit<-knn(train=Tmall_train.n,test=Tmall_test.n,cl=Tmall_train.lable,k=i,prob=FALSE) 
  CT<-table(Tmall_test.lable,KnnFit) 
  errRatio<-c(errRatio,(1-sum(diag(CT))/sum(CT))*100) 
  
}
CT
errRatio 
plot(errRatio,type="b",xlab="近邻个数K",ylab="错判率(%)",main="天猫成交顾客分类预测中的近邻数K与错判率")

#for new data
#KnnFit<-knn(train=Tmall_train.n,test=Tmall_new.n,cl=Tmall_train.lable,k=9,prob=FALSE)


#model by caret package
#install.packages("caret")
library(caret)
set.seed(123456)
trainFit<-train(x=Tmall_train.n,y=Tmall_train.lable,method="knn") 

#model by caret package
#install.packages("kknn")
library(kknn)
set.seed(123456)
kknnFit1<-kknn(BuyOrNot~.,Tmall_train,test=Tmall_train,k=5) 
kknnFit2<-kknn(BuyOrNot~.,Tmall_train,test=Tmall_test,k=5) 
summary(kknnFit1)

################天猫数据KNN分类讨论变量重要性#####################

library("class")  

set.seed(123456)
errRatio<-vector()

# Determine parameter K
for(i in 1:30){
  KnnFit<-knn(train=Tmall_train[,-1],test=Tmall_test[,-1],cl=Tmall_train[,1],k=i,prob=FALSE) 
  CT<-table(Tmall_test[,1],KnnFit) 
  errRatio<-c(errRatio,(1-sum(diag(CT))/sum(CT))*100)    
}
CT
plot(errRatio,type="l",xlab="近邻个数K",ylab="错判率(%)",main="近邻数K与错判率")

errRatio
(errDelteX<-errRatio[7])

# find variable importance，cl:classlabel
for(i in -2:-5){
  fit<-knn(train=Tmall_train[,c(-1,i)],test=Tmall_test[,c(-1,i)],cl=Tmall_train[,1],k=7)
  CT<-table(Tmall_test[,1],fit)
  errDelteX<-c(errDelteX,(1-sum(diag(CT))/sum(CT))*100) 
  #error rate with all variables, error rate without each variable
  #
}
CT
errDelteX
plot(errDelteX,type="l",xlab="剔除变量",ylab="剔除错判率(%)",main="剔除变量与错判率(K=7)",cex.main=0.8)
#cex:缩放比例。cex.main：标题的缩放
xTitle=c("1:全体变量","2:消费活跃度","3:活跃度","4:成交有效度","5:活动有效度")
#legend("topright",legend=xTitle,title="变量说明",lty=1,cex=0.4,y.intersp=0.3)   
legend(x=4.5,y=7,legend=xTitle,lty=1,cex=0.5,y.intersp=0.16,x.intersp=0.1)   
#title：给图例加标题
#x, y:用于定位图例，也可用单键词"bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center"
#x.intersp:图例中文字离图片的水平距离
#y.intersp:图例中文字离图片的垂直距离

FI<-errDelteX[-1]+1/4 #the first element of errDeltex is 
#the error rate with all variables. FI=e+1/p  
wi<-FI/sum(FI)  # standardize     
wi

#Concatenate vectors after converting to character. 向量转换成字符并连接
GLabs<-paste(c("消费活跃度","活跃度","成交有效度","活动有效度"),round(wi,2),sep=":") 
GLabs
pie(wi,labels=GLabs,clockwise=TRUE,main="输入变量权重",cex.main=0.8)
ColPch=as.integer(as.vector(Tmall_test[,1]))+2
plot(Tmall_test[,c(2,4)],pch=ColPch,cex=0.7,xlim=c(0,50),ylim=c(0,50),col=ColPch,
     xlab="消费活跃度",ylab="成交有效度",main="二维特征空间中的观测",cex.main=0.8)
#pch: 指定绘制点时使用的符号,


#################天猫数据基于相似性的KNN分类##############################

#install.packages("kknn")
library("kknn")

Tmall_train<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/knn/天猫_Train_1.txt",header=TRUE,sep=",")
Tmall_train$BuyOrNot<-as.factor(Tmall_train$BuyOrNot)
Tmall_train
fit<-train.kknn(formula=BuyOrNot~.,data=Tmall_train,kmax=11,distance=2,kernel=c("rectangular","triangular","gaussian"),na.action=na.omit())
#na.action=na.omit()表示带有缺失值的观测不参加分析

plot(fit$MISCLASS[,1]*100,type="l",main="不同核函数和近邻个数K下的错判率曲线图",cex.main=0.8,xlab="近邻个数K",ylab="错判率(%)")

#MISCLASS: kmax*n. 存放不同核函数下当近邻个数K依次取1至kmax时，分类预测的留一法判错率

lines(fit$MISCLASS[,2]*100,lty=2,col=1)
lines(fit$MISCLASS[,3]*100,lty=3,col=2)
#legend("topleft",legend=c("rectangular","triangular","gaussian"),lty=c(1,2,3),col=c(1,1,2),cex=0.7)   #给出图例
legend(x=0.8,y=0.6,legend=c("rectangular","triangular","gaussian"),lty=c(1,2,3),col=c(1,1,2),cex=0.6,y.intersp=0.12)   #给出图例

Tmall_test<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/knn/天猫_Test_1.txt",header=TRUE,sep=",")
Tmall_test$BuyOrNot<-as.factor(Tmall_test$BuyOrNot)
fit<-kknn(formula=BuyOrNot~.,train=Tmall_train,test=Tmall_test,k=7,distance=2,kernel="gaussian",na.action=na.omit())
(CT<-table(Tmall_test[,1],fit$fitted.values))
#fitted.values:近邻K依次取1至kmax时，各个观测的预测类别
(errRatio<-(1-sum(diag(CT))/sum(CT)))

###比较
library("class")
fit<-knn(train=Tmall_train,test=Tmall_test,cl=Tmall_train$BuyOrNot,k=7)
CT<-table(Tmall_test[,1],fit)
CT
errRatio<-c(errRatio,(1-sum(diag(CT))/sum(CT))*100)
errGraph<-barplot(errRatio,main="基于相似性K近邻法与K近邻法的错判率对比图(K=7)",cex.main=0.8,xlab="分类方法",ylab="错判率(%)",col="orange",axes=FALSE)

#side表示在图形的哪边绘制坐标轴（1=下，2=左，3=上，4=右）
#tcl：坐标轴刻度线的高度
#at : 需要添加刻度的数值
axis(side=1,at=c(0,errGraph,3),labels=c("","基于相似性K-近邻法","K-近邻法",""),tcl=0.25)
axis(side=2,tcl=0.25)


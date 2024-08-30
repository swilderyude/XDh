#****************************************************************************
#*************   Decision Tree: rpart package  *************
#*******Recursive Partitioning and Regression Tree********
##############################################################################

# Read data 
TelephoneData<-read.csv("/Users/zhao/Desktop/大三课作业/数据挖掘实验/Telephone.csv", head = TRUE, encoding = 'utf-8',fileEncoding = 'GBK')
TelephoneData  #展示数据
head(TelephoneData) #查看属性
sum(is.na(TelephoneData)) #na看是否有缺失值
##understand data structure and relationship between two features
table(TelephoneData$流失) #流失或不流失，table形成表，$是的，看0和1个数
table(TelephoneData[,c("开通月数","流失")]) #取列数，但不是很好的可视化
par(mfrow=c(1,2)) ##将画板变为1行2列的样式，让两张图在同一行分布
hist(TelephoneData$开通月数,main = '开通月数分布',xlab = "开通月数",ylab = "流失") #hist直方图
hist(TelephoneData$开通月数,main = '开通月数分布',xlab = "开通月数",ylab = "套餐类型")

#click the icon X to remove the figure and the style of 1 row and 2 columns

#compare str() and summary()
str(TelephoneData)

summary(TelephoneData)

#check: Are there missing value in this dataset?
sum(!complete.cases(TelephoneData)) #！非complete完整，查看缺失

# fix the seed so that every time running the model we do not work with different samples
set.seed(1234)  #初始化随机数发生器

# To construct any classification/regression model, we need to partition
# data into training and testing data. 
# randomly select instances for train and test data
# select approximately 70% of the data for training and 30% for testing

nrow(TelephoneData)

index = sample(2, nrow(TelephoneData), replace = TRUE, prob = c(0.7,0.3)) #有放回
#采样范围1:2，size：采样次数 nrow()
index
typeof(index)

TrainData = TelephoneData[index == 1, ]
TrainData

nrow(TrainData)

TestData = TelephoneData[index == 2,]
TestData

nrow(TestData)

# Construct a decision tree model by rpart()
# from "rpart" package
#install.packages("rpart")
library(rpart)
CTl<-rpart.control(minsplit = 10, maxcompete=4,xval=10,maxdepth=30,cp = 0)
#minisplit节点的最小样本量，默认20. maxcompete指定按变量重要性
#（使输出变量异质性下降）降序，#输出当前最佳分组变量的前若干个候选变量，
#默认值为4
#xval 交叉验证折数，默认10  maxdepth 树的最大深度，默认30
#cp 复杂度参数，默认初始值0.01，cp=0是在上述参数下的未修剪的最大树

#if have missing value, need surrogate split
#CTl<-rpart.control(minsplit = 10, maxcompete=4,xval=10,
#                   maxdepth=30,minsplit = 20, 
#                   minbucket = round(minsplit/3), 
#                   cp = 0.01, maxsurrogate = 5, 
#                   usesurrogate = 2, xval = 10,
#                   surrogatestyle = 0)
# go to packages of rpart to see rpart.control for 
# details


#Telephone_rpart = rpart(流失~., data = TrainData,method = "class", control = rpart.control(minsplit = 10, cp = 0))
rpart.model = rpart(流失~., data = TrainData,method = "class",parms = list(split="information"), control = CTl) #default
rpart.model
# In "control" we can control the pruning options. 
# To learn about the settings of rpart.control, 
#use help (write "?rpart.control" in console)

# TO plot rpart decision tree we can either use 
# rpart.plot() function from rpart.plot package 
# or fancyRpartPlot() function from "rattle" package.
#install.packages("rpart.plot")
#par(mfrow=c(1,1))
#install.packages("rpart.plot")
library(rpart.plot) #--- If the packages rattle is installed, it has rpart.plot in it
rpart.plot(rpart.model)

summary(rpart.model)

rpart.model$cptable

rpart.model$variable.importance

# or plot tree by prp()
prp(rpart.model, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

#how many leaves
length(rpart.model$frame$var[rpart.model$frame$var == "<leaf>"]) 

# or 
#install.packages("rattle")
#rattle强大数据挖掘工具，图形交互式可视化界面
library(rattle)
fancyRpartPlot(rpart.model,palettes=c("Greys", "Blues"))
#store rules 
sink("E:/sunlei/ExperimentRcodes/decisiontree/rules.txt")
asRules(rpart.model,compact=FALSE)

sink()
#Rule number: 1500 [流失=0 cover=7 (1%) prob=0.00]
#规则1500共有7个样本，覆盖率=7/705约为1%，流失=0，正确率为0.

#Predict the probablity of class: "0" or "1"
(predict(rpart.model))
#?predict

#  Predict TrainData by generated model above
train_predict=predict(rpart.model, type = "class")  #分类，预测
train_predict
is.factor(train_predict)
#因子用来存储类别变量(categorical variables)和有序变量
#这类变量不能用来计算而只能用来分类或者计数
#Check Actual Class for TrainData
typeof(TelephoneData$流失)

train.predict=cbind(TrainData,train_predict) #加上一列
head(train.predict)

#Compare Predicted vs Actual??confusion matrix
(train_confusion=table(train_predict, TrainData$流失, dnn = c("Predicted", "Actual"))) #加括号表示计算完马上显示，混淆矩阵
# dnn adds the label for rows and columns
# dnn stands for dimension names

(Erorr.train=(sum(train_confusion)-sum(diag(train_confusion)))/sum(train_confusion)) #总体错误率计算，diag对角线

# Or use confusionMatrix() to evaluate
#caret: Classification and Regression Training
#install.packages("caret")
library(caret)
pred.train <- predict(rpart.model,TrainData,type = "class") 
CM<-confusionMatrix(pred.train, as.factor(TrainData$流失)) # # 创建训练集数据与其预测结果的混淆矩阵
CM #计算完成了相关值
#Pos Pred Value : 0.9480：TPR=TP/(TP+FP)
#Neg Pred Value : 0.8065 ：FPR=TN/(TN+FN)
#Prevalence : 0.7135: (TP+FN)/(P+N)
#Detection Rate : 0.6533:TP/P+N   
#Detection Prevalence : 0.6891 :(TP+FP)/(P+N) 
#Balanced Accuracy:(sensitivity+specificity)/2
typeof(CM)
(ConfMatrix<-CM$table)
typeof(ConfMatrix)

# predict testing data
test_predict=predict(rpart.model,newdata = TestData,type="class")
test_predict
(test_confusion=table(test_predict, TestData$流失, dnn = c("Predicted", "Actual")))
#or
#(test_confusion=table(actural=TestData$流失,predictedclass=test_predict))

(Erorr.rpart=(sum(test_confusion)-sum(diag(test_confusion)))/sum(test_confusion))
sensitivity(train_predict,as.factor(TrainData$流失))
precision(train_predict,as.factor(TrainData$流失))
recall(train_predict,as.factor(TrainData$流失))
#use confusionMatrix() to get confusion matrix and evaluation
#indicators

#  Classification and Regression Training package
library(caret)

train_predict<-as.factor(train_predict)
is.factor(TrainData$流失)
TrainData$流失<-as.factor(TrainData$流失)
TestData$流失<-as.factor(TestData$流失)
sensitivity(train_predict,TrainData$流失)

specificity(train_predict,TrainData$流失)

sensitivity(test_predict,TestData$流失)

specificity(test_predict,TestData$流失)

# show CP parameters for the tree pruning
#交叉验证的估计误差（“xerror”），标准误差(“xstd”)，
#平均相对误差=xerror±xstd 。
printcp(rpart.model)
plotcp(rpart.model)

grid() #加上网格

#select CP with minimum error by CV
rpart.model$cptable
xerr <-rpart.model$cptable[,"xerror"]
xerr
minxerr <- which.min(xerr)
minxerr
mincp <-rpart.model$cptable[minxerr, "CP"]
mincp

# or you can do as follows instead of 175-181
#opt<-which.min(rpart.model$cptable[,"xerror"])
#cp<-rpart.model$cptable[opt,"CP"]
#Model_prune<-prune(rpart.model,cp=cp)
#print(Model_prune);


# prune the tree 
set.seed(1234)
rpart.prune<-prune(rpart.model,cp=mincp)
rpart.prune  #标记为叶节点
rpart.plot(rpart.prune)
#or
fancyRpartPlot(rpart.prune)

# Evaluate
train.predict.prune<-predict(rpart.prune,type="class")# default data is training data
train.predict.prune
#test.predict<-predict(rpart.prune,newdata = TestData) #default is probability
test.predict.prune<-predict(rpart.prune,newdata = TestData,type="class")
test.predict.prune

(test_confusion=table(actural=TestData$流失,predictedclass=test_predict))
(Erorr.rpart=(sum(test_confusion)-sum(diag(test_confusion)))/sum(test_confusion))

sensitivity(train.predict.prune,TrainData$流失)
specificity(train.predict.prune,TrainData$流失)
sensitivity(test.predict.prune,TestData$流失)
specificity(test.predict.prune,TestData$流失)

# also can use CrossTable() to generate confusion matrix
#install.packages("gmodels")
library(gmodels)
confusion=CrossTable(TestData$流失,test.predict.prune,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual','predicted'))

# Use the model to classify new data
#predict(rpart.prune, NewData)

#***********************ROC***************************************
#*****************************************************************
install.packages("ROCR")
library(ROCR)

#type is default means type="prob"
(train.predict<-predict(rpart.prune))
#train.pre<-ifelse(train.predict[,2]>0.5,1,0) 
#比较predict()的输出，确定预测的类标号
train.pred = prediction(train.predict[,2],TrainData$流失) 
train.perf<-performance(train.pred,"tpr","fpr")  
plot(train.perf,main="ROC Curve",col = "blue", lty = 1, lwd = 3)
#lty = 3, lwd = 3 线型，宽度,cex:字符大小

(test.predict<-predict(rpart.prune,newdat=TestData))
test.pred = prediction(test.predict[,2],TestData$流失)
test.perf<-performance(test.pred,"tpr","fpr")  
test.predict<-predict(rpart.prune,newdata = TestData) 
#use performance() to compute tpr and  fpr??needing return value of prediction()
par(new=T)    #defaulting to FALSE.如果设定为TRUE，
#If set to TRUE, the next plotting should not clean the current
plot(test.perf,main="ROC Curve",col = "red", lty = 1, lwd = 3)
#lty = 3, lwd = 3 线型，宽度
#Add straight lines to a plot (a = intercept and b = slope)
abline(a= 0, b=1)
legend("bottomright",legend=c("training","testing"),bty="n",lty=c(3,3),col=c("blue","red"),cex=1)


# ******************** Gain Chart *****************************************#
#*******************************************************************
train.gain = performance(train.pred, "tpr", "rpp")
test.gain<-performance(test.pred,"tpr","rpp")  

plot(train.gain,main="Gain Chart",col="blue",lty = 1, lwd = 3)
par(new=T)
plot(test.gain,main="Gain chart",col = "red", lty = 1, lwd = 3)
abline(a= 0, b=1)
legend("bottomright",legend=c("training","testing"),bty="n",lty=c(1,1),col=c("blue","red"),cex=1)

# ******************* Lift Chart ******************************************** #
# ************************************************************
train.lift = performance(train.pred, "lift", "rpp")
test.lift<-performance(test.pred,"lift","rpp")  

plot(train.lift,main="Lift Curve",col="blue",lty = 1, lwd = 3)
par(new=T)
plot(test.lift,col = "red", lty = 1, lwd = 3)
legend("topright",legend=c("training","testing"),bty="n",lty=c(1,1),col=c("blue","red"),cex=1)


# ************ DECISION TREES WITH C50 PACKAGE*************************
#######################################################################

#install.packages("C50")
#install.packages("e1071")
#install.packages("irr")
#install.packages("lpSolve")
#install.packages("vcd")
library(C50)
library(caret)
library(irr)
library(lpSolve)
library(vcd)
library(grid)

#typical decision tree with C5.0
set.seed(123)
train_sample<-sample(1000,900) # randomly select 900 samples
train<-TelephoneData[train_sample,]
test<-TelephoneData[-train_sample,]
typical.model<-C5.0(train[,-15],as.factor(train$流失)) ##训练数据框要删除分类因子向量

plot(typical.model)

rule<- C5.0(as.factor(train$流失) ~.,data=train,rules=T,control =treec50)

summary( rule )

typical.pred<-predict(typical.model,test)
typical.confusion=table(typical.pred,test$流失,dnn = c("Predicted", "Actual"))
(Erorr.typical=(sum(typical.confusion)-sum(diag(typical.confusion)))/sum(typical.confusion))

# also can use CrossTable() to generate confusion matrix
#nstall.packages("gmodels")
library(gmodels)
ctree.confusion=CrossTable(test$流失,typical.pred,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual','predicted'))


# ********** DECISION TREES WITH PARTY PACKAGE*******************************************************************************
#######################################################################################

# construct a decision tree using ctree() from "party" package
#install.packages("party")
library(party)
set.seed(123)
train_sample<-sample(1000,900) # randomly select 900 samples
train<-TelephoneData[train_sample,]
test<-TelephoneData[-train_sample,]
# Basic model
ctree.model<-ctree(流失~.,data=train)
plot(ctree.model)
plot(ctree.model,type="simple")

# Summary of predictions on test data 
ctree.predict=predict(ctree.model,newdata=test,type="response")
ctree.predict
(ctree.confusion=table(ctree.predict, test$流失, dnn = c("Predicted", "Actual")))
#or
(test_confusion=table(actural=test$流失,ctree.predict))
(Erorr.ctree=(sum(test_confusion)-sum(diag(test_confusion)))/sum(test_confusion))
#

# new DATA
# Predict the "probability" (instead of classes) of prediction for test data
#predict(ctree.model, newdata = newData, type = "prob")
# The predict function here returns the probablity of 
#predictied value (With what probability or 
#predictions are correct (it is known as confidence))


#install.packages("C50")
library(C50)
ls('package:C50')
tc<-C5.0Control(subset =F,CF=0.25,winnow=F,noGlobalPruning=F,minCases =20)
model <- C5.0(流失~., data = TrainData,rules=F,control =tc)
summary( model )

typeof(TelephoneData$流失)
TelephoneData$流失<-as.factor(TelephoneData$流失)


###********************Multi-class**************************
#***********************************************************
library(C50)
set.seed(2016)
data("iris")
train.indeces <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indeces, ]
iris.test <- iris[-train.indeces, ]

model <- C5.0(formula=Species ~ ., data = iris.train)
results <- predict(object = model, newdata = iris.test, type = "class")

res <- table(results, iris.test$Species)
res

#
library(gmodels)
CrossTable(results, iris.test$Species,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# rpart() package
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages('ggplot2')
#install.packages('ROCR')
#install.packages('caret')
#library(ggplot2)
library(pROC) 
library(rpart)
library(rpart.plot)
library(caret)
s = sample(c(1:nrow(iris)),100,replace = F)
trainset = iris[s,]
testset = iris[-s,]
train_multi = rpart(Species ~ .,data = trainset)
summary(train_multi)
#绘制决策树
rpart.plot(train_multi,type = 2)
pre = predict(train_multi,testset,type = "class") 
#预测准确率
t = table(pre,testset$Species,dnn = c("Predicted", "Actual"))
t
acc = sum(diag(t))/nrow(testset) *100
print(paste("模型准确率为：",round(acc,4),'%',sep=''))


# following is two methods to generate confution matrix
CrossTable(pre,testset$Species,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

confusionMatrix(pre,testset$Species) 

# ROC: multiclass.roc() of package pROC
pre2 = predict(train_multi,testset,type = "prob")
pre2
roc1=multiclass.roc(testset$Species,pre2[,1])
roc1
typeof(roc1)
plot(roc1$rocs[[1]],col='blue',type="b")
plot.roc(roc1$rocs[[3]],add=TRUE,col='red')
plot.roc(roc1$rocs[[2]],add=TRUE,col='green')
auc(roc1)

#这个ROC图呈现的是极端情况，第一个类别基本预测的全对，
#所以蓝色的线呈现直角形状。图中红色的ROC曲线是第三个
#类别，错了一半，对了一半，所以呈现的是条对角线。


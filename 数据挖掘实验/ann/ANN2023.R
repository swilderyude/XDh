
##################################################################################
#  R语言中关于神经网络的包（package）：nnet、neuralnet、RSNNS、caret等。         #
#  nnet提供了最常见的前馈反向传播神经网络算法。                                  #
#  neuralnet包的改进在于提供了弹性反向传播算法和更多的激活函数形式。             #
#  RSNNS包的mlp()函数            
##################################################################################

###################neuralnet package##########################


#install.packages("neuralnet")
library("neuralnet")

#read data. we still input "消费决策数据" .Income: 1 is high,2 is midlle, 3 is low.
#Gender: 1 is male, 2 is female
#purchase:0 is not buy, 1 is buy
Buy<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/ann/消费决策数据.txt", head = TRUE)
typeof(Buy$Purchase)#e
Buy$Purchase
#model
set.seed(1234)
(BPnet1<-neuralnet(Purchase~+Age+Gender+Income,data=Buy,hidden=2,err.fct="sse",linear.output=FALSE))
#one hidden layer and two neurons
#stepmax:最大循环次数，默认10万次
#ce:交叉熵， sse: 误差的平方
#rep(a,b):b hidden layers and a neurons each layer,hidden=rep(2,2)
#hidden=c(3,2,2): 3 hidden layers, 3,2,or 2 neurons in each layer respectivly
#attension: can not write: Purchase~.,data=......

# return of BPnet1() 
BPnet1$result.matrix   #weights and other information
BPnet1$weights  #array that stores weights
BPnet1$response   #actural 
BPnet1$net.result #predict
typeof(BPnet1$net.result)

#########网络及权值参数可视化###########
plot(BPnet1)

#####构建2个隐藏层的网络###########
(BPnet11<-neuralnet(Purchase~+Age+Gender+Income,data=Buy,hidden=c(2,2),act.fct = "logistic",stepmax = 20000))
#2 hidden layer and two neurons of each layer
plot(BPnet11)

# use NeuralNetTools to visualize ANN
#install.packages("NeuralNetTools")
library(NeuralNetTools)
par(cex = 0.8)
plotnet(BPnet11,pos_col = "red", neg_col = "grey")
#正数权重使用红色的线连接、负的权重使用灰色的线连接
#线的粗细则反映权重的取值大小
#####################输入变量重要性及可视化#####################
# use NeuralNetTools to visualize the importance of variables
garson(BPnet1)
#we found Age is the least important

#####################不同输入变量水平组合下的预测#####################
#Effects of different combinations on output  性别和收入对是否购买的影响
(mean(Buy$Age))
#age最不重要，取均值39岁和性别以及收入进行各种组合，
#研究对消费决策的影响
newdata<-matrix(c(39,1,1,39,1,2,39,1,3,39,2,1,39,2,2,39,2,3),nrow=6,ncol=3,byrow=TRUE)
newdata
#predict()计算任意组合下输出节点的预测值
#同时利用predict()也可预测新样本
(BPnet11<-neuralnet(Purchase~+Age+Gender+Income,data=Buy,hidden=2,err.fct="ce",linear.output=FALSE))
#one hidden layer and two neur
newoutput<-predict(BPnet11,newdata)
newoutput
#low income has higher puchase probability.


#####################确定概率分割值 ROC curve#####################
# avoiding conflit
# unload neuralnet package because both ROCR and neuralnet have prediction().
#detach("package:neuralnet") 
library(neuralnet)
#install.packages("ROCR")
library("ROCR")
(BPnet1<-neuralnet(Purchase~+Age+Gender+Income,data=Buy,hidden=2,err.fct="ce",linear.output=FALSE))

summary(BPnet1$net.result[[1]])#预测概率值
# 3rd Qu.:0.4788  上四分位0.48  
detach("package:neuralnet") 
pred<-prediction(predictions = as.vector(BPnet1$net.result),labels=BPnet1$response)

perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,print.cutoffs.at=c(0.2,0.45,0.47,0.48))
# This vector specifies the cutoffs which should 
#be printed as text along the curve at the corresponding curve positions.

#模型的总体精度随概率分割值变化
perf<-performance(pred,measure = "acc")
plot(perf)
# o.48附近的总体预测精确度较高，因此以概率值的上四分位数0.48作为概率分割值
#大于该值预测类别为1，否则为0.
BPnet1$net.result
out<-cbind(BPnet1$response,BPnet1$net.result[[1]])
out
outfinal<-cbind(out,ifelse(out[,2]>0.48,1,0))
outfinal
(confusion<-table(outfinal[,1],outfinal[,3]))
(err.BP<-(sum(confusion)-sum(diag(confusion)))/sum(confusion))

#if probability greater than 50 % then 1 else 0
nn1 = ifelse(BPnet1$net.result[[1]]>0.5,1,0)
(misClasificationError = mean(Buy$Purchase!=nn1))


###########################nnet package建立神经网络###############################
######建立三层网络
#install.packages("nnet")
library("nnet")
#install.packages("devtools",type = "win.binary")
#install.packages("devtools", type = "source")
library(devtools)
#open a window生成一个窗口
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
win.graph(width=16, height=16,pointsize=10)
#png(width=16, height=16,pointsize=10)
#x11(width=16, height=16,pointsize=10)
#pdf(width=16, height=16,pointsize=10)
#jpeg(width=16, height=16,pointsize=10)
set.seed(1000)
Buy<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/ann/消费决策数据.txt", head = TRUE)
is.factor(Buy$Purchase)
(BPnet2<-nnet(Purchase~Age+Gender+Income,data=Buy,size=2,entropy=TRUE,maxit=200))
#nnet()只能构建三层网络，size:隐节点数
plot.nnet(BPnet2)
#对新样本进行预测
#predict（BPnet2，datafilename, type="class")


########  multiclass#############
set.seed(123)

data(iris)
iris
(ind = sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3)))
(trainset = iris[ind == 1,])
testset = iris[ind == 2,]

# call nnet() to construct model,only one hidden layer
iris.nn = nnet(Species ~ .,data = trainset,size = 2,rang = 0.1,decay = 5e-4,maxit = 200)

iris.predict = predict(iris.nn,testset,type = "class")
nn.table = table(testset$Species,iris.predict)
nn.table
#windows的
win.graph(width=16, height=16,pointsize=10)
plot.nnet(iris.nn)

##################################################
#                   RSNNS                        #
##################################################
#install.packages("RSNNS")
library(RSNNS)

set.seed(2)
data(iris)

#shuffle the vector#导入数据并且对数据进行打乱，
#因为原始iris的数据是根据类型排序的，洗牌还是有必要的。
iris <- iris[sample(nrow(iris)),]

irisValues <- iris[,1:4]
#irisTargets <- decodeClassLabels(iris[,5])

#如果是对应类别就置0.9，不是的话就置0.1，
#这个数值根据valTrue和valFalse来设定
irisTargets <- decodeClassLabels(iris[,5], valTrue=0.9, valFalse=0.1)

irisTargets

#将数据集合划分成训练集和测试集，测试集大小根据ratio
#参数确定。取数据集的15%。训练集合又分为inputs（特征值）
#和targets（目标分类结果），测试集也同样分为这两个集合。
iris <- splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
iris
#normalize data
iris <- normTrainingAndTestSet(iris)
iris

#expand.grid()函数让两个向量间组合，免于多层遍历,例如：
sex <- c('female', 'male')
age <- c(10, 20, 30)
major <- c('math', 'physics', 'art')
expanded_data <- expand.grid(sex, age, major)
print(expanded_data)
#以上是说明expand.grid 的用法


#展示在不同的参数下运行的效果，设置了12组参数。
#其中第一列是隐藏神经元个数，第二列是学习率。
parameterGrid <- expand.grid(c(3,5,9,15), c(0.00316, 0.0147, 0.1))
parameterGrid


#mlp()网络训练。size:number of units in the hidden layers
#apply()第一个参数是指要参与计算的矩阵；
#第二个参数是指按行计算还是按列计算，1按行，2按列计算；
#第三个参数是指具体的运算参数。
#mlp():number of units in the hidden layer(s)
#p is parameterGrid
models <- apply(parameterGrid, 1, function(p) {
  
  mlp(iris$inputsTrain, iris$targetsTrain, size=p[1], learnFunc="Std_Backpropagation",
      learnFuncParams=c(p[2], 0.1), maxit=200, inputsTest=iris$inputsTest,
      targetsTest=iris$targetsTest)
})


models

typeof(models)
#the number of model is 12
length(models)

#library(devtools)
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

win.graph(width=16, height=16,pointsize=10)
par(mfrow=c(4,3))
#利用plotIterativeError()函数绘制迭代误差。
#其中IterativeFitError (as black line),IterativeTestError (as red line)。
#绘制迭代次数和方差
for(modInd in 1:length(models)) {
  plotIterativeError(models[[modInd]], main=names(models)[modInd])
}

#“list apply”，即对于一个列表型对象的每一元素应用一个特定的函数，
#返回一个由这些函数的返回值组成的列表对象。
#同时，lapply也支持其他的对象类型，如向量和数据框。
#lapply(X, FUN, …)：X表示列表型对象，FUN表示需要应用的函数
#…代表FUN所需要的参数
trainErrors <- data.frame(lapply(models, function(mod) {
  error <- sqrt(sum((mod$fitted.values - iris$targetsTrain)^2))
  error
}))

testErrors <- data.frame(lapply(models, function(mod) {
  pred <- predict(mod,iris$inputsTest)
  error <- sqrt(sum((pred - iris$targetsTest)^2))
  error
}))

#矩阵转置
t(trainErrors)
t(testErrors)

#which()函数是R语言中的一个基础函数,用于返回满足指定条件
#的元素的位置或索引。
which(min(trainErrors) == trainErrors)

trainErrors[which(min(trainErrors) == trainErrors)]
testErrors[which(min(testErrors) == testErrors)]

model <- models[[which(min(testErrors) == testErrors)]]

model

summary(model)

#install.packages("reshape")
library(reshape)
win.graph(width=16, height=16,pointsize=10)
plot.nnet(model)

predictions = predict(model,iris$inputsTest)

#confusion matrix
CM=confusionMatrix(iris$targetsTest,predictions)
CM

##Draw ROC curve
win.graph(width=16, height=16,pointsize=10)
plotROC(predictions,iris$targetsTest,main="ROC of BP",col='blue')
abline(0,1,lty=2,col='red')


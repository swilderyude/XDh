#############################################
#                  K-means                   #
##############################################

# read data

library(ggplot2)
#dplyr包主要用于数据清洗和整理，主要功能有：行选择、列选择、
#统计汇总、窗口函数、数据框交集等是非常高效、友好的数据处理包。
library(dplyr)

PoData<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/km/环境污染数据.txt",header=TRUE,fileEncoding = 'GBK')
PoData
CluData<-PoData[,2:7]

# model with different k and calculate tot.withinss
#purrr包极大的加速了数据处理流程，减少了code的编写。
#map_dbl:用C编写的，速度快，代码简洁（代替了for循环）
library(purrr)
set.seed(12345)
tot_withinss<-map_dbl(2:10, function(k){
  model<-kmeans(x=CluData,centers=k,nstart=30)
  model$tot.withinss
})
tot_withinss

set.seed(12345)
evaluation<-map_dbl(2:10, function(k){
  model<-kmeans(x=CluData,centers=k,nstart=30)
  (model$betweenss/(k-1))/(model$tot.withinss/(31-k))
})
evaluation


#generate a dataframe containing both k and tot-withinss
elbow_df<-data.frame(k=2:10,tot_withinss)
elbow_df

# draw elbow figure to find a satisfied k
win.graph(width=16, height=16,pointsize=10)
pic<-ggplot(elbow_df,aes(x=k,y=tot_withinss))+geom_line()+scale_x_continuous(breaks=2:10)

pic+ggtitle("elbow picture for kmeans") +
  theme(plot.title = element_text(hjust = 0.5)) #设置标题居中

#labs(x = 'Name', y = 'Value')+
#  theme(axis.title =  element_text(size=12,face = "bold"),
#        axis.text.x =   element_text(angle=90,  # 横坐标文字旋转九十度
#hjust = 1, # 调整横坐标文字位置
#size=15))  # 调整横坐标文字字号

#随着聚类数目增多，每一个类别中数量越来越少，
#距离越来越近，因此WSS值肯定是随着聚类数目增多而
#减少的，所以关注的是斜率的变化，在WSS减少得很缓慢时，
#就认为进一步增大聚类数效果也并不能增强，存在这个
#“肘点”就是最佳聚类数目，从一类到三类下降得很快，
#之后下降得很慢，所以最佳聚类个数选3即可。


#model with k=4 and visualize the result
CluR<-kmeans(x=CluData,centers=4,iter.max=10,nstart=30)
CluR$cluster
CluR$size
CluR$centers
plot(PoData[,c(2,3)],col=PoData$CluR,main="生活污染情况",xlab="生活污水排放量",ylab="生活二氧化硫排放量")
points(CluR$centers[,c(1,2)],col=rownames(CluR$centers),pch=8,cex=2)

plot(PoData[,c(2,4)],col=PoData$CluR,main="生活污染情况",xlab="生活污水排放量",ylab="生活烟尘排放量")
points(CluR$centers[,c(1,3)],col=rownames(CluR$centers),pch=8,cex=2)

plot(PoData[,c(3,4)],col=PoData$CluR,main="生活污染情况",xlab="生活二氧化硫排放量",ylab="生活烟尘排放量")
points(CluR$centers[,c(2,3)],col=rownames(CluR$centers),pch=8,cex=2)

plot(PoData[,c(5,6)],col=PoData$CluR,main="工业污染情况",xlab="工业固体废物排放量",ylab="工业废气排放总量")
points(CluR$centers[,c(4,5)],col=rownames(CluR$centers),pch=8,cex=2)

plot(PoData[,c(5,7)],col=PoData$CluR,main="工业污染情况",xlab="工业固体废物排放量",ylab="工业废水排放量")
points(CluR$centers[,c(4,6)],col=rownames(CluR$centers),pch=8,cex=2)

plot(PoData[,c(6,7)],col=PoData$CluR,main="工业污染情况",xlab="工业废气排放总量",ylab="工业废水排放量")
points(CluR$centers[,c(5,6)],col=rownames(CluR$centers),pch=8,cex=2)


library(dplyr)
#mutate() adds new variables and preserves existing ones;
segment<-mutate(CluData,cluster=CluR$cluster)
segment
head(segment)

count<-count(segment, CluR$cluster)
count
ggplot(segment,aes(x=cluster))+geom_histogram(stat ="count",fill='pink',width=0.6)

#聚类结果多种可视化
#ggfortify对ggplot2包的强力补充。
#install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
autoplot(kmeans(CluData, 4),data=CluData,label=TRUE, label.size=3, frame=TRUE)

autoplot(kmeans(CluData, 5),data=CluData,label=TRUE, label.size=3, frame=TRUE)

PoData$CluR<-CluR$cluster# save the result of clustering to PoData$CluR
plot(PoData$CluR,pch=PoData$CluR,ylab="类别编号",xlab="省市",main="聚类的类成员",axes=FALSE)
# pch：绘图符号设置参数，用4,19,2,6代表的符号绘图不同的簇
##1表示总是水平方向；2表示总是垂直于坐标轴；3表示总是垂直方向。#
axis(1,at=1:31,las=2,labels=PoData$province,cex.axis=0.6) #横坐标是省市名称
axis(2,at=1:4,labels=1:4,cex.axis=0.6) #列坐标是聚类编号
box()
legend("topright",c("第一类","第二类","第三类","第四类"),pch=c(4,19,2,6),cex=0.6)

#display averages for clustered data for each 
#segment or cluster
#magrittr包被定义为一个高效的管道操作工具包，
#让数据或表达式的传递更高效
#其实就是掌握4个操作符的用法：
#向右操作符%>%, 向左操作符%T>%, 
#解释操作符%$% 和复合赋值操作符%<>%。

library(magrittr)
#segment数据传给group_by()传给summarise_all
#x %>% f() %>% g()  等同于 g(f(x))
#summarise_all，则会对数据集中的所有变量进行
#fun(函数)求解。group_by分组
avg<-segment %>% group_by(cluster) %>% summarise_all(funs(mean(.)))
avg

##############################################
#                K的选择方法                #
##############################################

#No.1: WSS
#install.packages("factoextra")
#factoextra:多元统计的可视化，可以做PCA分析并绘图。
#install.packages("factoextra")
library(factoextra)
fviz_cluster(CluR, data=CluData)

#确定最佳聚类个数，使用组内平方误差和法  
set.seed(1234)
#select K.参数xintercept：x轴截距或直线所在位置(geom_vline()可以不要)
fviz_nbclust(CluData, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)

#clustering
km.res <- kmeans(CluData,5)
fviz_cluster(km.res, data = CluData)

# No.2: package of Mclust, provding several modeles 
# to select K

#install.packages("mclust")
# model based clustering的包，mclust提供几种模型拟合数据。
library(mclust)
m_clust <- Mclust(as.matrix(CluData), G=2:10) #聚类数目从2一直试到10
summary(m_clust)

# plot(m_clust, "BIC")#选取最大的BIC对应的K值
#BIC( Bayesian Information Criterion ): 贝叶斯信息判别标准
#Mclucst会选择其中BIC最大的模型和分组作为最终的结果。
plot.Mclust(m_clust, what = "BIC", 
            ylim = range(m_clust$BIC, na.rm = TRUE), 
            legendArgs = list(x = "topright", cex =0.6))

BIC <- mclustBIC(CluData)
mod2 <- Mclust(CluData, G = 5, modelNames = "EEV", x=BIC)
drmod <- MclustDR(mod2, lambda = 1)

####No.3:组内平方误差和——拐点图
#随着聚类数目增多，每一个类别中数量越来越少，
#距离越来越近，因此WSS值肯定是随着聚类数目增多而减少
#的，所以关注的是斜率的变化，但WsS减少得很缓慢时，
#就认为进一步增大聚类数效果也并不能增强，
#存在得这个“肘点”就是最佳聚类数目.
#(nrow(CluData)-1)*sum(apply(CluData,2,var)): 计算
#没有聚类前的方差和
wssplot <- function(CluData, nc=16, seed=1234){
  wss <- (nrow(CluData)-1)*sum(apply(CluData,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(CluData, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(CluData)

###No.4: 轮廓系数silhouette
library(cluster)
library(factoextra)
fviz_nbclust(CluData, kmeans, method = "silhouette")

##OR fcp package
#install.packages("fpc")
#cluster.stats():comparison between clusterings 
#and decision about the number of clusters
#avg.silwidth:平均轮廓值
#kmeans()返回值cluster存放个观测所属的类别编号
#stats、cluster、fpc和mclust是常用的四个聚类分析软件包。
#fpc包的函数cluster.stats()，返回值avg.silwidth:平均轮廓值
#cluster.stats()的输入：d : 距离对象(使用dist(data[,"数值型属性"]))
#clustering:使用kmeans拟合后的对象(如result)的cluster(聚类结果)
library(fpc)
nk=2:10
set.seed(1234)
sw=sapply(nk,function(k){
  result<-kmeans(CluData,centers=k)
  stats <- cluster.stats(dist(CluData), result$cluster)
  sw<- stats$avg.silwidth
})
sw
(max<-nk[which.max(sw)])
plot(nk,sw,type = "l",xlab="number of clusters",ylab = "average silhouette width")


############################层次聚类Hierarchical clustering#################################
############################################################################################

PoData<-read.table(file="/Users/zhao/Desktop/大三课作业/数据挖掘实验/km/环境污染数据.txt",header=TRUE,fileEncoding = 'GBK')
CluData<-PoData[,2:7]# 提取聚类变量
DisMatrix<-dist(CluData,method="euclidean")
CluR<-hclust(d=DisMatrix,method="ward.D")
#类平均法：average   重心法：centroid
#中间距离法:median
# 最长距离法：complete 默认
# 最短距离法：single
# 离差平方和法：ward
###############     层次聚类的树形图   #####################
#par(mfrow=c(2,1))
#(mfrow=c(1,1))
plot(CluR,labels=PoData[,1])
box()
# 添加聚类分类矩形，如分为3类
rect.hclust(CluR, k=3)

###########层次聚类的碎石图
#层次聚类hclust()的返回值height记录了聚类的过程，
#聚成n-1,n-2,...1时的最小类间距离
#层次聚类决定了这个距离不断增大
plot(CluR$height,30:1,type="b",cex=0.7,xlab="距离测度",ylab="聚类数目")

######取4类的聚类解并可视化

PoData$memb<-cutree(CluR,k=4) #聚成4个簇，最小的类间距离的变化幅度不大
PoData$memb
par(family="PingFangSC-Regular")
plot(CluR,labels=PoData[,1])
box()
# 添加聚类分类矩形
rect.hclust(CluR, k=4)
#浏览各类的成员个数
table(PoData$memb)

#另一种可视化
par(family="PingFangSC-Regular")
plot(PoData$memb,pch=PoData$memb,ylab="类别编号",xlab="省市",main="聚类的类成员",axes=FALSE)
par(las=2)
axis(1,at=1:31,labels=PoData$province,cex.axis=0.6)
axis(2,at=1:4,labels=1:4,cex.axis=0.6)
box()


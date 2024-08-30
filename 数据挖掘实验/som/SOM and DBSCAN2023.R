

################################# Kohonen and DBSCAN ############################
#################################################################################

###############模拟数据的SOM聚类
#install.packages("kohonen")
library("kohonen")
set.seed(12345)
Data<-matrix(rnorm(n=100,mean=0,sd=1),ncol=2,byrow=TRUE)  
Data[1:25,1]<-Data[1:25,1]+3            
Data[1:25,2]<-Data[1:25,2]-4

win.graph(width=16, height=16,pointsize=10)
plot(Data,main="模拟数据观测点的分布",xlab="x1",ylab="x2")  

set.seed(12345)
My.som<-som(Data,grid=somgrid(xdim=1,ydim=2,topo="rectangular"))
summary(My.som)           #mean(My.som$distances)
#unit.classif各类别所属类别编号
table(My.som$unit.classif)
#changes:迭代过程中各类质心位置偏移的平均值
plot(My.som, type = "changes")


plot(My.som,type="mapping",main="SOM网络输出层示意图",pchs=My.som$unit.classif)

#衡量SOM聚类过程中的权值是否调整充分，即迭代次数是否合理
plot(My.som,type="changes",main="SOM网络聚类评价图")


#################### 酒品质数据####################
library("kohonen")
set.seed(123)
data(wines)
wines
vintages

#可以使用kmeans 确定最优聚类数目的方法先确定聚类数目，
#然后再执行som聚类
## som model
wines.som <- som(scale(wines), grid = somgrid(xdim=1, ydim=3, topo="rectangular"),rlen=200)
#邻域半径默认为获胜节点与最远节点距离的2/3
summary(wines.som)
wines.som$distances# distance 各观测与各自簇质心的距离，距离越近，越合理。
mean(wines.som$distances)
wines.som$code #各输出节点的连接权值
table(wines.som$unit.classif) # unit.classif#各观测所属聚类类别编号

#plot
plot(wines.som,type="mapping",pchs=wines.som$unit.classif,main="SOM 网络输出层示意图")
#mapping 可视化输出层，各输出节点以不同符号的点表示观测点与簇对应关系。
#各簇内密度的粗略表现。
plot(wines.som,type="changes",main="SOM 网络聚类评价图")
#changes迭代过程中各质心位置偏移的平均值，簇的质心随迭代周期变化的折线图

plot(wines.som,type="counts",main="SOM聚类样本量分布情况")
#颜色深浅表示簇所含样本的多少
plot(wines.som,type="quality",main="SOM聚类类内差异情况图")
#可视化输出层，深浅表示簇内观测与质心距离的平均值大小。 
#平均值小，效果好。比mapping更精确


################ DBSCAN聚类示例#####################
Data<-read.table(file="E:/ExperimentRcodes/SOM and DBSCAN/模式识别数据.txt",sep=",",head=TRUE)
#install.packages("fpc")
library("fpc")
par(mfrow=c(2,3))
plot(Data,cex=0.5,main="观测点的分布图")
(DBS1<-dbscan(data=Data,eps=0.2,MinPts=200,scale = FALSE)) 
plot(DBS1,Data,cex=0.5,main="DBSCAN聚类(eps=0.2,MinPts=200)")
(DBS2<-dbscan(data=Data,eps=0.5,MinPts=80,scale = FALSE)) 
plot(DBS2,Data,cex=0.5,main="DBSCAN聚类(eps=0.5,MinPts=80)")
(DBS3<-dbscan(data=Data,eps=0.2,MinPts=100,scale = FALSE))
plot(DBS3,Data,cex=0.5,main="DBSCAN聚类(eps=0.2,MinPts=100)")
(DBS4<-dbscan(data=Data,eps=0.5,MinPts=300,scale = FALSE))
plot(DBS4,Data,cex=0.5,main="DBSCAN聚类(eps=0.5,MinPts=300)")
(DBS5<-dbscan(data=Data,eps=0.2,MinPts=30,scale = FALSE))
plot(DBS5,Data,cex=0.5,main="DBSCAN聚类(eps=0.2,MinPts=30)")
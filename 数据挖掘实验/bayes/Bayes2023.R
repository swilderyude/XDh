
###############Naive Byes #####################
###############################################
#install.packages("NLP")
#install.packages("tm")
#install.packages("wordcloud2")
#install.packages("tidytext")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("RColorBrewer")
#install.packages("gmodels")
#install.packages("ggpol")
library(NLP)
library(tm)
library(wordcloud2)
library(tidytext)
library(reshape2)
library(dplyr)
library(e1071)
library(ggpol)

###read data
sms_raw <- read.csv("/Users/zhao/Desktop/大三课作业/数据挖掘实验/bayes/sms_spam.csv",header=TRUE,stringsAsFactors=FALSE,encoding = "UTF-8")

str(sms_raw)#查看数据结构

sms_raw$type <-as.factor(sms_raw$type) ####将type设为因子变量
str(sms_raw)#查看数据结构

table(sms_raw$type)#每个类型的数量
prop.table(table(sms_raw$type))#查看每个类型的占的百分比

##############    clean data    ############
############################################

#1. Create 语料库corpus,文本文档的集合
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])


# Remove invalid characters
sms_corpus_clean <- gsub("[^[:print:]]", "", sms_corpus)

#2.清理语料库
#2.1 #  upper case to lower case

# Create a Corpus object from the cleaned text
sms_corpus <- Corpus(VectorSource(sms_corpus_clean))

# Apply the tolower transformation using tm_map
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(sms_corpus, tolower)





# 2.2 remove numbers
corpus_clean <- tm_map(corpus_clean, removeNumbers)
# 2.3 remove stop words, such as and,or,until...
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
# 2.4  remove Punctuations
corpus_clean <- tm_map(corpus_clean, removePunctuation)
# 2.5 remove extra spaces，使单词之间只保留一个空格
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
# 2.6 remove the useless characters
toSpace <- content_transformer(function(x,pattern) gsub(pattern,' ',x))
corpus_clean <- tm_map(corpus_clean,toSpace,'/')
# see effect
inspect(corpus_clean[1:3])


##########  create doc-word matrix   ##########
##########     wordcloud  词云图     ##########
###############################################
corpus_dtm <- TermDocumentMatrix(corpus_clean)
corpus_dtm_matrix <- as.matrix(corpus_dtm)
v <- sort(rowSums(corpus_dtm_matrix),decreasing = T)
corpus_dtm_frame <- data.frame(word=names(v),freq=v)

wordcloud2(corpus_dtm_frame, size = 0.3, shape = 'star')
#data：词云生成数据，包含具体词语以及频率；
#size：字体大小，默认为1，一般来说该值越小，
#生成的形状轮廓越明显；
#shape：词云形状选择，默认是‘circle’，
#‘cardioid’（苹果形或心形），‘diamond’，‘triangle-forward’
#‘triangle’），‘pentagon’ 
wordcloud2(corpus_dtm_frame, size = 0.5, fontFamily = "微软雅黑",  
           
           color = "random-light", backgroundColor = "grey")  

############## change spars matrix into the structure for bayes classifier  ###########
#为了减少特征的数量，剔除训练数据中出现在少于5条短信中或者少于记录总数的0.1%的所有词
# find the words DTM 用单词命名列,文档单词矩阵
corpus_dtm <-DocumentTermMatrix(corpus_clean)
col(corpus_dtm)

#查看行列数
dim(corpus_dtm)

# Subset the DTM using valid indices
subset_dtm <- corpus_dtm[1:10, ]  # Subset the first 10 rows

# Perform further operations on the subsetted DTM
inspect(corpus_dtm[1:10, 1200:1204])
#inspect(sms_tdm[1:10, 1200:1204])
corpus_freq_words<-findFreqTerms(corpus_dtm,5)

corpus_dtm_train <- corpus_dtm[1:4169,]
corpus_dtm_test <- corpus_dtm[4170:5571,]

#at least 5 messages include the same word
corpus_freq_words_train<-findFreqTerms(corpus_dtm_train,5)
str(corpus_freq_words_train)

corpus_freq_words_test<-findFreqTerms(corpus_dtm_test,5)
  

# Testing sets and training sets only contain the words that appear more than 
# or equle to  5 times  过滤掉包含频率低的单词的短信
corpus_dtm_freq_train<-corpus_dtm_train[,corpus_freq_words_train]
corpus_dtm_freq_test<-corpus_dtm_test[,corpus_freq_words_test]

# define a function   NB just know weather the word appear in the message or not
# NB只想知道这个单词出现了或者没出现，不需要具体出现的次数，因此需要改成分类变量：yes, no。
convert_counts<-function(x){x<-ifelse(x>0,"Yes","No")}
# if the word does not appear in the message, use No instad of zero.
# if the word does appears in the message, use Yes instad of frequency.

# apply convert_counts() to each colum of sparse matrix: 以上函数应用于稀疏矩阵的每一列
# apply() can be used to each row and each colum of a matrix. MARGIN=2 means colum
# MARGIN=1 means row
#需要把convert_counts应用于稀疏矩阵的每一列。apply()课作用于行或列，margin=1表示行
sms_train<-apply(corpus_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test<-apply(corpus_dtm_freq_test,MARGIN = 2,convert_counts)
sms_test
str(sms_train)
#########   Creat Bayes Classifier ##########
#############################################
# Compute the probability that the short message is spam or not 
# based on the existence or non-existence of the word in the message

#install.packages("e1071")
library(e1071)
sms_train_lables<-sms_raw[1:4169,]$type
sms_classifier<-naiveBayes(sms_train,sms_train_lables)

#assess the model
sms_test_lables<-sms_raw[4170:5571,]$type
sms_test_pred<-predict(sms_classifier,sms_test)

#install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred,sms_test_lables,prop.chisq=FALSE,prop.t=FALSE,dnn=c('predicted','actual'))
#prop.t:表比例是否加入
#prop.chisq:每个单元的卡方值是否加入


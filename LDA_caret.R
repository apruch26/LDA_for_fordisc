#load in MASS, caret: the packages that will be needed
library(MASS)
library(caret)

# read data as a csv in and store it as the data which is a list
data<-read.csv('GenDataCont1.csv')
# make sure it is a dataframe
data<-as.data.frame(data)

# change output to factors
# w/ column names
data$o1<-factor(data$o1)
data$o2<-factor(data$o2)
data$o3<-factor(data$o3)
# generic w/ column numbers
data[,93]<-factor(data[,93])
data[,94]<-factor(data[,94])
data[,95]<-factor(data[,95])

# Shuffle data for analysis
n <- nrow(data)
data <- data[sample(n),]

# create training, test sets 
train_indices<-1:round(0.7*n)
train<-data[train_indices,]
test_indices<-(round(0.7*n)+1):n
test<-data[test_indices,]


# set up for LDA 
train_o1<-train[,c(1:91,93)]
train_o2<-train[,c(1:91,94)]
train_o3<-train[,c(1:91,95)]
test_o1<-test[,c(1:91,93)]
test_o2<-test[,c(1:91,94)]
test_o3<-test[,c(1:91,95)]

#LDA ----- lda,lda2, or stepLDA -- what to use? 
# using lda
# o1
modelFit_o1<- train(o1~.,method='lda',preProcess=c('scale','center'), data=train_o1)
matrix_o1<-confusionMatrix(test_o1$o1, predict(modelFit_o1,test_o1)) # Accuracy : 0.90
table_o1<-table(test_o1$o1, predict(modelFit_o1,test_o1))
table_o1_perct<-round(prop.table(table_o1, 1),3)*100
# o2
modelFit_o2<- train(o2~.,method='lda',preProcess=c('scale','center'), data=train_o2)
matrix_o2<-confusionMatrix(test_o2$o2, predict(modelFit_o2,test_o2)) # Accuracy : 0.8778 
table_o2<-table(test_o2$o2, predict(modelFit_o2,test_o2))
table_o2_perct<-round(prop.table(table_o2, 1),3)*100
# o3
modelFit_o3<- train(o3~.,method='lda',preProcess=c('scale','center'), data=train_o3)
matrix_o3<-confusionMatrix(test_o3$o3, predict(modelFit_o3,test_o3)) # Accuracy : 0.7667
table_o3<-table(test_o3$o3, predict(modelFit_o3,test_o3))
table_o3_perct<-round(prop.table(table_o3, 1),3)*100

# using lda
# o1
modelFit_o1_step<- train(o1~.,method='stepLDA',preProcess=c('scale','center'), data=train_o1)
matrix_o1_step<-confusionMatrix(test_o1$o1, predict(modelFit_o1,test_o1)) 

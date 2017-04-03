library(MASS)
library(caret)

data3<-read.csv('FlexData3.csv')
data3<-as.data.frame(data3)
testdata = na.omit(data3)

testdata$o1<-factor(testdata$o1)
testdata$o2<-factor(testdata$o2)
testdata$o3<-factor(testdata$o3)

lda_avg3<-function(sample_number){
  
  o1_1_vec<-numeric()
  o1_2_vec<-numeric()
  o2_1_vec<-numeric()
  o2_2_vec<-numeric()
  o2_3_vec<-numeric()
  o3_1_vec<-numeric()
  o3_2_vec<-numeric()
  
  for(i in 1:sample_number){
    
    
    n <- nrow(testdata)
    testdata <- testdata[sample(n),]
    
    
    train_indices<-1:round(0.8*n)
    train<-testdata[train_indices,]
    test_indices<-(round(0.8*n)+1):n
    test<-testdata[test_indices,]
    
    train_o1<-train[,c(2:10,11)]
    train_o2<-train[,c(2:10,12)]
    train_o3<-train[,c(2:10,13)]
    test_o1<-test[,c(2:10,11)]
    test_o2<-test[,c(2:10,12)]
    test_o3<-test[,c(2:10,13)]
    # o1
    modelFit_o1<- train(o1~.,method='lda',preProcess=c('scale','center'), data=train_o1)
    #matrix_o1<-confusionMatrix(test_o1$o1, predict(modelFit_o1,test_o1)) # Accuracy : 0.90
    table_o1<-table(test_o1$o1, predict(modelFit_o1,test_o1))
    table_o1_perct<-round(prop.table(table_o1, 1),3)*100
    # o2
    modelFit_o2<- train(o2~.,method='lda',preProcess=c('scale','center'), data=train_o2)
    #matrix_o2<-confusionMatrix(test_o2$o2, predict(modelFit_o2,test_o2)) # Accuracy : 0.8778 
    table_o2<-table(test_o2$o2, predict(modelFit_o2,test_o2))
    table_o2_perct<-round(prop.table(table_o2, 1),3)*100
    # o3
    modelFit_o3<- train(o3~.,method='lda',preProcess=c('scale','center'), data=train_o3)
    #matrix_o3<-confusionMatrix(test_o3$o3, predict(modelFit_o3,test_o3)) # Accuracy : 0.7667
    table_o3<-table(test_o3$o3, predict(modelFit_o3,test_o3))
    table_o3_perct<-round(prop.table(table_o3, 1),3)*100
    
    o1_1_vec<-c(o1_1_vec,table_o1_perct[1,1])
    o1_2_vec<-c(o1_2_vec,table_o1_perct[2,2])
    o2_1_vec<-c(o2_1_vec,table_o2_perct[1,1])
    o2_2_vec<-c(o2_2_vec,table_o2_perct[2,2])
    o2_3_vec<-c(o2_3_vec,table_o2_perct[3,3])
    o3_1_vec<-c(o3_1_vec,table_o3_perct[1,1])
    o3_2_vec<-c(o3_2_vec,table_o3_perct[2,2])
    
  }
  
  print(paste("o1_1 average =", mean(o1_1_vec)))
  print(paste("o1_1 sd =", sd(o1_1_vec)))
  print(paste("o1_2 average =", mean(o1_2_vec)))
  print(paste("o1_2 sd =", sd(o1_2_vec)))
  print(paste("o2_1 average =", mean(o2_1_vec)))
  print(paste("o2_1 sd =", sd(o2_1_vec)))
  print(paste("o2_2 average =", mean(o2_2_vec)))
  print(paste("o2_2 sd =", sd(o2_2_vec)))
  print(paste("o2_3 average =", mean(o2_3_vec)))
  print(paste("o2_3 sd =", sd(o2_3_vec)))
  print(paste("o3_1 average =",  mean(o3_1_vec)))
  print(paste("o3_1 sd =", sd(o3_1_vec)))
  print(paste("o3_2 average =", mean(o3_2_vec)))
  print(paste("o3_2 sd =", sd(o3_2_vec)))
 
}
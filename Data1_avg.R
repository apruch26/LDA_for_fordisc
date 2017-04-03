library(MASS)
library(caret)

data<-read.csv('GenDataCont1.csv')
data<-as.data.frame(data)

data$o1<-factor(data$o1)
data$o2<-factor(data$o2)
data$o3<-factor(data$o3)


lda_avg1<-function(sample_number){
  
  o1_1<-0
  o1_2<-0
  o2_1<-0
  o2_2<-0
  o3_1<-0
  o3_2<-0
  o3_3<-0
  o3_4<-0
  
  o1_1_vec<-numeric()
  o1_2_vec<-numeric()
  o2_1_vec<-numeric()
  o2_2_vec<-numeric()
  o3_1_vec<-numeric()
  o3_2_vec<-numeric()
  o3_3_vec<-numeric()
  o3_4_vec<-numeric()
  
  for(i in 1:sample_number){
    
    n <- nrow(data)
    data <- data[sample(n),]
    
     
    train_indices<-1:round(0.8*n)
    train<-data[train_indices,]
    test_indices<-(round(0.8*n)+1):n
    test<-data[test_indices,]
    
    train_o1<-train[,c(1:91,93)]
    train_o2<-train[,c(1:91,94)]
    train_o3<-train[,c(1:91,95)]
    test_o1<-test[,c(1:91,93)]
    test_o2<-test[,c(1:91,94)]
    test_o3<-test[,c(1:91,95)]
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
    
    #o1 results
    o1_1<-o1_1+table_o1_perct[1,1]
    o1_1_vec<-append(o1_1_vec,table_o1_perct[1,1])
    o1_2<-o1_2+table_o1_perct[2,2]
    o1_2_vec<-append(o1_2_vec,table_o1_perct[2,2])
    #o2 results
    o2_1<-o1_1+table_o2_perct[1,1]
    o2_1_vec<-append(o2_1_vec,table_o2_perct[1,1])
    o2_2<-o1_2+table_o2_perct[2,2]
    o2_2_vec<-append(o2_2_vec,table_o2_perct[2,2])
    #o3 results
    o3_1<-o1_1+table_o3_perct[1,1]
    o3_1_vec<-append(o3_1_vec,table_o3_perct[1,1])
    o3_2<-o1_2+table_o3_perct[2,2]
    o3_2_vec<-append(o3_2_vec,table_o3_perct[2,2])
    o3_3<-o1_1+table_o3_perct[3,3]
    o3_3_vec<-append(o3_3_vec,table_o3_perct[3,3])
    o3_4<-o1_2+table_o3_perct[4,4]
    o3_4_vec<-append(o3_4_vec,table_o3_perct[4,4])
    
  }

print(paste("o1_1 average =", o1_1/sample_number))
print(paste("o1_1 sd =", sd(o1_1_vec)))
print(paste("o1_2 average =", o1_2/sample_number))
print(paste("o1_2 sd =", sd(o1_2_vec)))
print(paste("o2_1 average =", o2_1/sample_number))
print(paste("o2_1 sd =", sd(o2_1_vec)))
print(paste("o2_2 average =", o2_2/sample_number))
print(paste("o2_2 sd =", sd(o2_2_vec)))
print(paste("o3_1 average =", o3_1/sample_number))
print(paste("o3_1 sd =", sd(o3_1_vec)))
print(paste("o3_2 average =", o3_2/sample_number))
print(paste("o3_2 sd =", sd(o3_2_vec)))
print(paste("o3_3 average =", o3_3/sample_number))
print(paste("o3_3 sd =", sd(o3_3_vec)))
print(paste("o3_4 average =", o3_4/sample_number))
print(paste("o3_4 sd =", sd(o3_4_vec)))
}


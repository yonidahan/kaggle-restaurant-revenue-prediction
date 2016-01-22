

library(caret);library(dplyr)

train<-read.csv("train.csv")
test<-read.csv("test.csv")
train<-train[-c(17),] ##Removes the right outlier

n<-nrow(train)

test$revenue<-1
data<-rbind(train, test)

#Tranforms Time
data$elapsed<-as.POSIXlt("01/01/2015",format="%m/%d/%Y")-as.POSIXlt(data$Open.Date,
                                                                    format="%m/%d/%Y")
data$elapsed<-as.numeric(data$elapsed/10^3)#Scales

#Removes unimportant features
data<-select(data,-Type,-City,-City.Group,-Open.Date)

#Log Transforms
data[,paste("P",1:37,sep="")]<-log(1+data[,paste("P",1:37,sep="")])#P1:37
data$revenue<-log(data$revenue)#revenue
data$elapsed<-log(data$elapsed)#elapsed


#Random Forest
#10-fold cross validation repeated 5 times
#5000 trees
set.seed(1306)
trainCtrl<-trainControl(method="repeatedcv",number=10,repeats=5)##CV argument               
model<-train(revenue~.,trControl=trainCtrl,n.tree=5000,
               data=data[1:n,c("elapsed","P2","P16","P17","P25","P23",
                                        "P28","P34","revenue")])

#Predicts on Test
pred<-predict(model,data[-c(1:n),])

#csv Submission file
submission<-as.data.frame(cbind(seq(0,length(pred)-1,by=1),exp(pred)))
colnames(submission)<-c("Id","Prediction")
write.csv(submission,"submission.csv",row.names=FALSE)

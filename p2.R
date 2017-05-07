lgd <- read.csv("F:\\Data mining 2\\project\\dat.csv", header=T)

lgd_values <- lgd$lgd

lgd<-lgd[ , !(names(lgd) %in% c('id','year','index'))]

  
col_names <- sapply(lgd, function(col) length(unique(col)) < 4)
lgd[ , col_names] <- lapply(lgd[ , col_names] , factor)

lgd[ , 'ind']<- as.factor(lgd[ , 'ind'])
nums <- sapply(lgd, is.numeric)
lgd[ , nums]

str(lgd)

range <- function(x){(x-min(x))/(max(x)-min(x))}
maxs <- apply(lgd[ , nums], 2, max)
mins <- apply(lgd[ , nums], 2, min)
lgd[ , nums]<- as.data.frame( apply(lgd[ , nums], 2, range) )

lgd$lgd<-lgd_values

summary(lgd)

library(nnet)

set.seed(14);
subset = sample(nrow(lgd), nrow(lgd) * 0.75)
lgd_train = lgd[subset, ]
lgd_validation = lgd[-subset, ]

##
set.seed(14);
subset = sample(nrow(lgd_train), nrow(lgd_train) * 0.75)
lgd_training = lgd_train[subset, ]
lgd_test = lgd_train[-subset, ]


test_mse<-0
train_mse<-0
layer_size<-0
# train networks with sizes of hidden units ranging from 0 to 20
for (n in 1:20)
{
  
  train_predict<-0;
  test_predict<-0;
  # for each size, train 10 networks with different random starting points
  for(i in 1:5)
  {
    set.seed(i);
    net<-nnet(lgd~.,size = n, data=lgd_training, rang = 0.00001, 
              linout = TRUE, maxit = 5000, decay = 0, skip = TRUE);
    train_predict<-train_predict+predict(net, lgd_training);
    test_predict<-test_predict+predict(net, lgd_test);
  }
  # average outcomes of 10 networks
  lgd_training$pred_lgd<-train_predict/i;
  lgd_test$pred_lgd<-test_predict/i;
  # calculate the sum of squared residuals for training and validation sets, SSEs
  test_mse<-rbind(test_mse, mean((lgd_test$lgd-lgd_test$pred_lgd)^2));
  train_mse<-rbind(train_mse, mean((lgd_training$lgd-lgd_training$pred_lgd)^2));
  layer_size<-rbind(layer_size,n);
  train_predict<-0;
  test_predict<-0;
}

result<-data.frame(test_mse,train_mse,layer_size)
result = result[result$test_mse > 0,]







View(result)


dev.off()
library(ggplot2)
library(reshape2)
d <- melt(result2, id.vars="layer_size")

# Everything on the same plot
ggplot(d, aes(layer_size,value, col=variable)) + 
  geom_point() + 
  geom_line()



test_sse<-0
train_sse<-0
layer_size<-0
decay_value<-0
# train networks with sizes of hidden units ranging from 0 to 20
for (n in 1:6)
{
  
  for (w in c(0, 0.01, 0.001, 0.0001))
  {
    train_predict<-0;
    test_predict<-0;
    # for each size, train 10 networks with different random starting points
    for(i in 1:10)
    {
      set.seed(i);
      net<-nnet(lgd~.,size = n, data=lgd_training, rang = 0.00001, 
                linout = TRUE, maxit = 5000, decay = w, skip = TRUE);
      train_predict<-train_predict+predict(net, lgd_training);
      test_predict<-test_predict+predict(net, lgd_test);
    }
    
    # average outcomes of 10 networks
    lgd_training$pred_lgd<-train_predict/i;
    lgd_test$pred_lgd<-test_predict/i;
    # calculate the sum of squared residuals for training and validation sets, SSEs
    test_sse<-rbind(test_sse, mean((lgd_test$lgd-lgd_test$pred_lgd)^2));
    train_sse<-rbind(train_sse, mean((lgd_training$lgd-lgd_training$pred_lgd)^2));
    layer_size<-rbind(layer_size,n);
    decay_value<-rbind(decay_value,w);
    train_predict<-0;
    test_predict<-0;
  }
}


result2<-data.frame(test_sse,layer_size,decay_value)

result2$decay_value<-as.factor(result2$decay_value)

result2 = result2[result2$layer_size > 0,]

d2 <- melt(result2, id.vars="layer_size")

# Everything on the same plot
ggplot(result2, aes(layer_size,test_sse,group=decay_value, col=decay_value)) + 
  geom_point() + 
  geom_line()



#final model
n<-4;
decay_value<-0.0001


test_sse<-0
train_sse<-0
seed<-0

for( i in 1:40 ){
  set.seed(i)
  net<-nnet(lgd~.,size = n, data=lgd_training, rang = 0.00001, 
            linout = TRUE, maxit = 10000, decay = decay_value, skip = TRUE);
  test_sse<-rbind(test_sse, mean((lgd_test$lgd-predict(net, lgd_test))^2));
  train_sse<-rbind(train_sse, mean((lgd_training$lgd-predict(net, lgd_training))^2));
  seed<-rbind(seed,i);
}


result3<-data.frame(test_sse,train_sse,seed)

result3 = result3[result3$seed > 0,]


d2 <- melt(result3, id.vars="seed")

# Everything on the same plot
ggplot(d2, aes(seed,value, col=variable)) + 
  geom_point() + 
  geom_line()



set.seed(15)
net<-nnet(lgd~.,size = n, data=lgd_train, rang = 0.00001, 
          linout = TRUE, maxit = 10000, decay = decay_value, skip = TRUE);



summary(net)
#in sample mse
pred<-predict(net, lgd_train)
mean((lgd_train$lgd-pred)^2)
#0.06149982

#out sample mse
pred<-predict(net, lgd_validation)
mean((lgd_validation$lgd-pred)^2)
#0.0785984



#random forest


install.packages(
  'randomForest'
)

install.packages(
  'miscTools'
)
library(randomForest)
library(miscTools)
library(ggplot2)

lgd <- read.csv("F:\\Data mining 2\\project\\dat.csv", header=T)


lgd<-lgd[ , !(names(lgd) %in% c('id','year','index'))]


col_names <- sapply(lgd, function(col) length(unique(col)) < 4)
lgd[ , col_names] <- lapply(lgd[ , col_names] , factor)

lgd[ , 'ind']<- as.factor(lgd[ , 'ind'])


library(nnet)

set.seed(14);
subset = sample(nrow(lgd), nrow(lgd) * 0.75)
lgd_train = lgd[subset, ]
lgd_validation = lgd[-subset, ]



rf <- randomForest(lgd~., data=lgd_train,importance=TRUE, ntree=200)


(mse <- mean((lgd_train$lgd - predict(rf, lgd_train))^2))
# [1] 0.01370928

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=lgd_train$lgd, pred=predict(rf,lgd_train)))
p + geom_point() +
  geom_abline(color="red")

(mse <- mean((lgd_validation$lgd - predict(rf, lgd_validation))^2))
#0.04545992


imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(axis.text=element_text(size=12),plot.title=element_text(size=18))
p


final<-data.frame(actual=lgd_validation$lgd, predicted= predict(rf, lgd_validation))



range_finder = function(score) {
  return(ifelse(score<=.1,"0 - 0.1",
                            ifelse(score<=.2,"0.1 - 0.2",
                                   ifelse(score<=.3,"0.2 - 0.3",
                                          ifelse(score<=.4,"0.3 - 0.4",
                                                 ifelse(score<=.5,"0.4 - 0.5",
                                                        ifelse(score<=.6,"0.5 - 0.6",
                                                               ifelse(score<=.7,"0.6 - 0.7",
                                                                      ifelse(score<=.8,"0.7 - 0.8",
                                                                             ifelse(score<=.9,"0.8 - 0.9","0.9 - 1")
                                                                         )))
                                                 ))))))
  }


final$Actual <- range_finder(final$actual)
final$Predicted <- range_finder(final$predicted)

t<-table(final$actual_range,final$predicted_range)

#generate random data 
data = final

#compute frequency of actual categories
actual = as.data.frame(table(data$Actual))
names(actual) = c("Actual","ActualFreq")

#build confusion matrix
confusion = as.data.frame(table(data$Actual, data$Predicted))
names(confusion) = c("Actual","Predicted","Freq")

#calculate percentage of test cases based on actual frequency
confusion = merge(confusion, actual, by=c("Actual"))
confusion$Percent = confusion$Freq/confusion$ActualFreq*100
attach(confusion)
#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
  labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile

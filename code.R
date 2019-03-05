##################################R e g r e s s i o n#############################
data <− read.csv(”/Users/apple/Desktop/OnlineNewsPopularity.csv”) data <− data[,−c(1 ,2)]
train <− sample(nrow(data) ,0.7∗round(nrow(data)) ,replace = F) ######################################## L a s s o : v a r i a b l e s e l c t i o n###############
set . seed (666)
library ( glmnet )
x <− model.matrix(data$shares ̃.−1, data=data) y <− data$shares
fit . lasso <−glmnet(x,y)
plot(fit.lasso, xvar=”lambda”, label=TRUE) plot(fit.lasso, xvar=”dev”, label=TRUE) cv.lasso <−cv.glmnet(x, y)
plot ( cv . l a s s o )
abline(v=log(3.522e−02), col=”red”)
## coefficent vector corresponding to the mse which is within one standard error #of the lowest mse using the best lambda.
coef ( cv . l a s s o )
## coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x,y, lambda=cv. lasso$lambda.min))
coef(glmnet(x,y, lambda=3.522e−02))
# the varaible left after doing the lasso
name<− c(”num hrefs” , ”num imgs” , ”num keywords” , ”data channel is entertainment” , ”data   channel   is   bus” ,”data   channel   is   socmed” , ”data   channel   is   tech” , ”data channel is world”, ”kw min min”, ”kw avg avg”,
          ”self reference min shares”,”self reference avg sharess”,
          ”is weekend”,”LDA 02”,”shares”) data Lasso <− data[ ,name]
############################l i n e a r r e g r e s s i o n ###################################
lr<− lm(shares ̃., data = data Lasso[train ,]) predict. test <− predict(lr , data Lasso[−train ,−15])
#MSE 55.1201
mean((predict.test−data[−train ,15])ˆ2)
########################### E l a s t i c Net ########################################
x <− model.ma\begin{lstlisting }[language=R] #############)ˆ2)
res [ train ]
a<−seq(0.1, 0.9, 0.05)
4
search <− foreach(i = a, .combine = rbind) %dopar% {
  cv <− cv.glmnet(x, y, family = ”gaussian”, nfold = 10,
                  type . measure = ”deviance” , paralle = TRUE, alpha = i ) data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se] ,
                                                                                       lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <− search[search$cvm == min(search$cvm), ] cv. elastic <− glmnet(x, y, family = ”gaussian”,
                                                                     lambda = cv3$lambda.1se, alpha = cv3$alpha)
coef ( cv . e l a s t i c )
x test<−model.matrix(shares ̃.−1, data=data[−train ,]) predict.test <− predict(cv.elastic , x test)
#MSE 1.081562
mean((predict.test−data[−train ,59])ˆ2)
## Bagging and Random Forest ##################################
library ( randomForest )
set . seed (666)
## Recall that bagging is simply a special case of a random forest with m=p # the dataset was selected by lasso
bag new<− randomForest(shares  ̃. , data=data Lasso, subset=train , importance=TRUE)
plot(bag new)
## Predicted values on the testing data using bagging
yhat bag <−predict(bag new, newdata=data Lasso[−train ,−15]) plot(yhat   bag , new   test )
abline (0 ,1)
# MSE
mean(( yhat bag − data Lasso[−train ,15])ˆ2)
## We can view the importance of each variable
importance(bag new)
varImpPlot(bag new)
set . seed (666)
rf new <−randomForest(shares  ̃. , data=data Lasso ,
                      subset=train , mtry=4, importance=TRUE, ntree = 500) rf new all <−randomForest(shares  ̃. , data=data,
                                                                                                     subset=train , mtry=7, importance=TRUE, ntree = 500)
plot(rf new)
plot(rf new all)
yhat.rf <−predict(rf new, newdata=data Lasso[−train ,−15]) yhat.rf all <−predict(rf new all , newdata=data[−train ,−59])
##MSE RF 0.7175809
mean((yhat.rf all − data[−train,59]) ˆ 2) #MSE RF 0.7398845
mean((yhat.rf − data Lasso[−train ,15]) ˆ 2)
importance(rf new) varImpPlot(rf new) plot.randomForest(rf new)
sum(yhat.rf==data Lasso[−train ,17])
library (ROCR)
predictions=as.vector(rf new$votes [ ,2]) pred=prediction(predictions ,data Lasso[train ,15])
perf AUC=performance(pred,”auc”) #Calculate the AUC value
5

AUC=perf AUC@y.values[[1]]
perf ROC=performance(pred,”tpr”,”fpr”) #plot the actual ROC curve plot(perf ROC, main=”ROC plot”)
text(0.5,0.5,paste(”AUC = ”,format(AUC, digits=5, scientific=FALSE)))
## Perform Boosting
## Please learn the topic of Boosting from Session 8.3.4 of the textbook ISLR
library (gbm)
set . seed (666)
boost new = gbm(shares ̃. , data=data Lasso[train ,] , distribution
                = ”gaussian” ,
                n.trees = 500
)
summary(boost new)
yhat.boost = predict(boost new, newdata = data Lasso[−train ,−15], n. trees = 500, type = ”response”)
# MSE 0.7393814
mean((yhat.boost − data Lasso[−train ,15]) ˆ 2)
###################################################################### ########################C l a s s i f i c a i t o n################################ ######################################################################
data <− read.csv(”/Users/apple/Desktop/OnlineNewsPopularity.csv”) data <− data[,−c(1 ,2)]
data$shares [ data$shares <= 2800] <− 0
data$shares [data$shares > 2800] <− 1
#70 percent of dataset in the trainning data
set . seed (666)
library ( dplyr )
# The heatmap for correlation
library ( mlbench )
library ( caret )
# calculate correlation matrix
correlationMatrix <− cor(data[ ,1:58])
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <− findCorrelation ( correlationMatrix , cutoff =0.75) # print indexes of highly correlated attributes
print( highlyCorrelated )
colnames(data[ , highlyCorrelated ])
plot( correlationMatrix )
heatmap( correlationMatrix )
library ( reshape2 )
melt cor <− melt(correlationMatrix)
library ( ggplot2 )
ggplot(data = melt cor, aes(x=factor(Var1), y= factor(Var2), fill=value)) +
  geom tile()
## Lasso : variable selction
library ( glmnet )
x <− model.matrix(data$shares ̃.−1, data=data) y <− data$shares
fit . lasso <−glmnet(x,y)
plot(fit.lasso, xvar=”lambda”, label=TRUE) plot(fit.lasso, xvar=”dev”, label=TRUE) cv.lasso <−cv.glmnet(x, y)
6

plot ( cv . l a s s o )
abline(v=log(4.054e−03), col=”red”)
## coefficent vector corresponding to the mse which is within one standard #error of the lowest mse using the best lambda.
coef ( cv . l a s s o )
## coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x,y, lambda=cv. lasso$lambda.min))
coef(glmnet(x,y, lambda=4.054e−03))
# the varaible left after doing the lasso
name <− colnames(data) del<−c(
  grep(”n tokens title”,name),
  grep(”n   unique   tokens” , name) ,
  grep(”n   non   stop   unique   tokens” , grep(”num   videos” , name) ,
        grep(”data channel is lifestyle”, name), grep(”kw min avg”, name),
        grep(”self reference max shares”, name), grep(”weekday   is   wednesday” , name) , grep(”weekday is friday”, name) , grep(”weekday   is   saturday” , name) , grep(”weekday   is   sunday” , name) , grep(”LDA 04”, name),
        grep(”global   sentiment   polarity” , name) grep(”global   rate   positive   words” , name) grep(”global rate negative words” , name) grep(”rate   positive   words” , name) , grep(”rate negative words”, name) ,
        name) ,
  ,
  ,
  ,
  grep(”avg   positive   polarity” , grep(”max   positive   polarity” , grep(”max   negative   polarity” ,
                                                                              name) , name) , name)
)
data Lasso <− data[,−del]
data Lasso <− rbind(sample frac(data Lasso[data Lasso$shares==0,],1/3),
                    data Lasso[data Lasso$shares==1,])
#checked the group is balanced
sum(data Lasso$shares==0) sum(data Lasso$shares==1)
train <− sample(nrow(data Lasso ) ,0.7∗round(nrow(data Lasso )) , replace = F)
##################### Perform L o g i s t i c r e g r e s s i o n#########################
set . seed (666)
m log <− glm(shares ̃. ,data = data Lasso[train ,] , family = binomial())
m log prob <− predict(m log , newdata = data Lasso[−train , 1:39] , type = ”response”) # Select 0.5 as threshold
m log prob [m log prob <=0.5] <− 0
m log prob[m log prob>0.5]<− 1
library (pROC)
table<− table(data Lasso[−train ,39] , m log prob) # accuracy 0.6348667
mean(m log prob == data Lasso[−train ,39]) #recall 0.5956265
7

recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) #precison 0.6350851
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) #F1 0.6147233
2∗precison∗recall/( precison+recall ) #auc 0.634
roc(data Lasso[−train ,39] , m log prob)
####################### Quadratic Discriminant Analysis################### # Perform QDA on the traning data set using only two predictors ,
#1st lag and 2nd lag
qda fit = qda(shares ̃. , data = data Lasso , subset = train) qda pred = predict(qda fit , data Lasso[−train ,−39])$class
# Confusion matrix
table(qda pred , data Lasso[−train ,39]) # 0.5425225
mean(qda pred == data Lasso[−train ,39])
table<− table(data Lasso[−train ,39] , qda pred) #recall 0.1065602
recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) #precison 0.7172897
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) #F1 0.1855545
2∗precison∗recall/( precison+recall ) #auc 0.5332
roc(data Lasso[−train ,39] , as.numeric(qda pred))
####################### Perform K−nearest neighbours #######################
library ( class )
# Create training data for X
train X = data Lasso [ train , 1:38] # Create testing data for X
test X = data Lasso[−train ,1:38] # Create training data for Y
train Direction = data Lasso$shares [ train ] Direction test = data Lasso$shares[−train ] set . seed (666)
plo <− c()
for (k in c(1 ,3 ,5 ,10 ,20 ,50)){
  knn pred = knn( train X, test X, train Direction , k = k) table(knn   pred , Direction   test )
  plo[k]<−mean(knn pred == Direction test)
}
plot(plo)
# when k= 20, the accuracy is highest , which is 0.5888644
knn pred = knn( train X, test X, train Direction , k = 20) table<− table(knn pred , Direction test )
# accuary 0.5859786
mean(knn pred == Direction test)
# Bagging accuracy 0.6550671
mean(yhat bag == data Lasso[−train ,39]) #recall 0.5878378
recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) #precison 0.5133634
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) 8

#F1 0.5480823
2∗precison∗recall/( precison+recall ) #auc 0.5844
roc(Direction test , as.numeric(knn pred))
####################### Bagging and Random Forest ######################
library ( randomForest )
set . seed (666)
## Recall that bagging is simply a special case of a random forest with m=p
# the dataset was selected by lasso
bag new<− randomForest(factor(shares)  ̃. , data=data Lasso, subset=train , importance=TRUE)
plot(bag new)
## Predicted values on the testing data using bagging
yhat bag <−predict(bag new, newdata=data Lasso[−train ,−39]) plot(yhat   bag , new   test )
abline (0 ,1)
table<− table(data Lasso[−train ,39] , yhat bag)
# Bagging accuracy 0.6550671
mean(yhat bag == data Lasso[−train ,39]) #recall 0.6532454
recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) #precison 0.6456261
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) #F1 0.6494134
2∗precison∗recall/( precison+recall ) #auc 0.655
roc(data Lasso[−train ,39] , as.numeric(yhat bag))
## We can view the importance of each variable
importance(bag new) varImpPlot(bag new)
############################ Random Forest ############################
set . seed (666)
rf new <−randomForest(factor(shares)  ̃. , data=data Lasso ,
                      subset=train , mtry=6, importance=TRUE, ntree = 500) rf new all <−randomForest(factor(shares)  ̃. , data=data,
                                                                                                     subset=train , mtry=7, importance=TRUE, ntree = 500)
plot(rf new)
plot(rf new all)
yhat.rf <−predict(rf new, newdata=data Lasso[−train ,−39]) yhat.rf all <−predict(rf new all , newdata=data[−train ,−59])
##Compute the test accuracy for the whole dataset
mean(yhat.rf all==data[−train,59])
#Compute the test accuracy for the Lasso dataset mean(yhat.rf == data Lasso[−train ,39])
table<− table(data Lasso[−train,39], yhat.rf) # accuracy 0.6550671
mean(yhat bag == data Lasso[−train ,39]) #recall 0.644915
recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) 9

#precison 0.6446912
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) #F1 0.6448031
2∗precison∗recall/( precison+recall ) #auc 0.6524
roc(data Lasso[−train ,39], as.numeric(yhat.rf))
importance(rf new)
varImpPlot(rf new)
df<− data.frame(x = c(”kw avg avg”, ”kw max avg”,”self reference avg sharess”,
                       ”LDA 02”, ”kw avg max”), MeanDecreaseGini = c(457.28062 , 413.12311 , 318.57822 , 317.03859 , 301.64292))
library ( ggplot2 )
positions <− df[ ,1]
ggplot(data= df, aes(x=x,y=MeanDecreaseGini)) +
  geom bar(stat=”identity”, width=0.5) +
  scale x discrete(limits = positions) +
  xlab(” ”) +theme(axis.text.x = element text(size = 12))
sum(yhat.rf==data Lasso[−train ,17])
#################### P e r f o r m B o o s t i n g ############################# ## Please learn the topic of Boosting from Session
#8.3.4 of the textbook ISLR
library (gbm)
set . seed (666)
boost new=gbm(shares ̃. , data=data Lasso[train,], distribution = ”bernoulli”,
              n.trees = 5000
)
summary(boost new)
yhat.boost = predict(boost new,
                     newdata = data Lasso[−train ,−39], n. trees = 5000,
                     type = ”response”) yhat . boost [ yhat . boost >0.5] <− 1
yhat . boost [ yhat . boost <=0.5] <− 0
table<− table(data Lasso[−train ,39] , yhat.boost) # accuracy 0.6550671
mean(yhat bag == data Lasso[−train ,39])
#recall 0.6286012
recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) #precison 0.6431108
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) #F1 0.6357732
2∗precison∗recall/( precison+recall ) #auc 0.6474
roc(data Lasso[−train ,39] , as.numeric(yhat.boost))
################################SVM###################### # install .packages(”e1071”)
library ( e1071 )
data Lasso$shares [data Lasso$shares==0] <− −1
test <− sample(nrow(data),0.7∗nrow(data),replace = F) dat = data.frame(x = data Lasso[test,−39],
                                                                       y = as.factor(data Lasso[test ,39])) 10

## We now use tune () function to perform 10−fold corss #validation to determine an optimal cost parameter
set . seed (666)
tune.out = tune(svm, y ̃., data = dat, kernel = ”linear”,
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)),
                scale=T)
summary( tune . out )
## We see that cost = 0.1 results in the lowest cross
# validation error rate
## tune () function stores the best model obtained , which # can be assessed as follows
bestmod = tune . out$best .model summary( bestmod )
## The predict () function is used to predict the class label #on a set of test observations
## First generate the test observations
xtest = matrix(rnorm(20∗2) , ncol = 2)
ytest = sample(c(−1,1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
ttest <− sample(nrow(data),0.05∗nrow(data),replace = F)
testdat = data.frame(x = data Lasso[−test ,−39], y = as.factor(data Lasso[−test ,39]))
## Now we predict the class labels of these test obervations . #Here we use the best model obtained through cross #validation in order to make predictions
ypred = predict(bestmod, testdat) table(predict = ypred , truth = testdat$y)
mean(ypred==testdat$y) #
table<− table(testdat$y, ypred) # accuracy 0.6304016 mean(testdat$y == ypred) #recall 0.6309116
recall<−table[2 ,2]/(table[2,2]+table[2 ,1]) #precison 0.6251273
precison<−table[2 ,2]/(table[2,2]+table[1 ,2]) #F1 0.6280061
2∗precison∗recall/( precison+recall ) #auc 0.6304
roc(testdat$y, as.numeric(ypred))
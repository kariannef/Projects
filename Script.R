
library(devtools)
library(ggplot2)
library(tidyverse) 
library(reprtree)
library(caret)
library(caTools)
library(randomForest)
library(plyr)
library(foreign)
library(tidyverse)
library(readxl)
library(margins)
library(VIM)
library(mice)
library(PerformanceAnalytics)
library(naniar)
library(GGally)
library(MASS)
library(car)
library(skedastic)
library(lmtest)
library(lares)
library(tidyverse)
library(ggplot2)
library(GGally)
library(caret) 
library(sandwich)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(pROC)
library(repr)
library(dplyr)
library(glmnet)
library(lares)
rm(list=ls())

options(repr.plot.width=16, repr.plot.height=8)



d1 <- read.csv("https://query.data.world/s/nzqfojybv37vepvo43uwkqbuneyl27", sep=",",header=TRUE)
d2 <- read.csv("https://query.data.world/s/lyvebpj3b5udytvusulfgngrayiisj", sep=",",header=TRUE)

d3=merge(d1,d2,by=c("age","internet", "absences", "goout", "studytime", "Pstatus"))

#plot
f1  <- ggplot(d3, aes(x=G3.x, y=G3.y)) + labs(title = "Grade Distributions") + geom_point()
f1$labels$x  <- 'Math'
f1$labels$y  <- 'Portuguese'
f1
#plot
d4 <- table(d3$goout, d3$age)
barplot(d4, main="Goout and Age",
        xlab="Age", col=c("darkblue","red", "firebrick", "white", "grey"),
        legend = rownames(d4), args.legend=list(title="Goout"))
#plot
d5 <- table(d3$internet, d3$studytime)
barplot(d5, main="Internet and Studytime",
        xlab="Studytime", col=c("magenta","cyan"),
        legend = rownames(d5), args.legend=list(title="Internet"))



df_str(d3, return = "plot")



#plot
boxplot(d3$absences,d3$G3.x,d3$G3.y, d3$goout,col=rgb(0,0,0,1/4),
        names=c('Absences','Math','Portuguese', 'Goout')) 

sum(is.na(d3))


d3$failx <- as.numeric(d3$G3.x < 7)
d3$failx <- as.factor(d3$failx)
d3$failx <- as.numeric(d3$failx)
d3$faily <- as.numeric(d3$G3.y < 7)
d3$faily <- as.factor(d3$faily)
d3$faily <- as.numeric(d3$faily)
d3$fail <- rowMeans(d3[ , c("faily","failx")], na.rm=TRUE)
d3$fail <- as.factor(d3$fail > 1)
d3$fail = ifelse(d3$fail=="TRUE",1,0)
d3$g1 <- rowMeans(d3[ , c("G1.x","G1.y")], na.rm=TRUE)

#plot
dev.off()
f1  <- ggplot(d3, aes(x=G3.x, y=G3.y)) + labs(title = "Grade Distributions and Fail") + geom_point(aes(col = as.factor(fail)))
f1$labels$x  <- 'Math'
f1$labels$y  <- 'Portuguese'
f1$labels$colour  <- 'Fail'
f1

d3$g1 <- rowMeans(d3[ , c("G1.x","G1.y")], na.rm=TRUE)

#plot
corr_var(d3, # name of dataset
         fail, # name of variable to focus on
         max_pvalue = 0.10
         ,top = 20) 

sum(d3$age == 20)

ELSE <- TRUE
start.time <- Sys.time()
d3 <- d3 %>% mutate(.,result = with(.,case_when(
  (age == 15) ~ 0,
  (age == 16) ~ 1,
  (age == 17) ~ 2,
  (age == 18) ~ 3,
  ELSE ~ 4
)))

d3$age <- d3$result
d3$age <- as.factor(d3$age)
d3$internet = ifelse(d3$internet=="no",1,0)
d3$internet <- as.factor(d3$internet)
d3$goout <- as.numeric(d3$goout)
d3$paid.x = ifelse(d3$paid.x=="yes",1,0)
d3$paid.x <- as.factor(d3$paid.x)
d3$high1 <- d3$higher.x
d3$high1 = ifelse(d3$high1=="yes",1,0)
d3$high2 <- d3$higher.y
d3$high2 = ifelse(d3$high2=="yes",1,0)
d3$high <- rowMeans(d3[ , c("high1","high2")], na.rm=TRUE)
d3$high <- as.factor(d3$high > .99)
d3$high = ifelse(d3$high=="TRUE",1,0)
d3$failures <- rowMeans(d3[ , c("failures.y","failures.x")], na.rm=TRUE)
d3$Pstatus = ifelse(d3$Pstatus=="A",1,0)
data = data.frame(matrix(nrow = 536, ncol = 0))
data$goout <- d3$goout
data$fail <- (d3$fail)
data$internet <- d3$internet
data$age <- d3$age
data$pstatus <- as.numeric(d3$Pstatus)
data$studytime <- d3$studytime
data$absences <- d3$absences
data$first <- d3$g1
data$paid <- d3$paid.x
data$medu <- d3$Medu.x
data$high <- as.numeric(d3$high)
data$failures <- d3$failures
dataa <- data

#plot
dataa <- data
corr_var(dataa, # name of dataset
         fail, # name of variable to focus on
         max_pvalue = 0.10
         ,top = 15) 
##################################################
#    BINOMIAL MODEL
logit  <- glm(fail ~ . , data = data , family = binomial(logit))
summary(logit)

fitted_values <- predict(logit,type='response')
head(fitted_values)
data$fitted_values = fitted_values
data$y_hat <- data$fitted_values>0.5

cm  <- data.frame(table('True'=data$fail,'Predicted'=as.double(data$y_hat)))
cm
#plot
ggplot(data = cm, mapping = aes(x = ordered(True,c(1,0)), y = Predicted, fill=Freq)) +  geom_tile() +
  geom_text(aes(label=(Freq)),col='white') + xlab('True') + labs(title = "Frequency of True and False Predictions:Classification Model")

data$y_hatnew  <- as.double(fitted_values > 0.66)
cm  <- data.frame(table('True'=data$fail,'Predicted'=as.double(data$y_hatnew)))
cm  <- cm %>% group_by(True) %>% mutate(Realized_pct = Freq/sum(Freq))
cm
#plot
ggplot(data = cm, mapping = aes(x = ordered(True,c(1,0)), y = Predicted, fill=Realized_pct)) +  geom_tile() +
  geom_text(aes(label=round(Realized_pct,2)),col='white') + xlab('True') + labs(title = "Relative Frequency of True and False Predictions:Classification Model")



roc <- roc(data$fail, data$fitted_values)
auc <- round(auc(data$fail, data$fitted_values),4)
auc
(1 - mean(data$fail != (data$fitted_values>0.5))) * 100

#plot
ggroc(roc, color = 'darkblue', size = 1, legacy.axes = TRUE) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) + ylab('True Positive Rate') + xlab('False Positive Rate') + 
  geom_point(aes(x=0,y=1,color='Perfect predictor')) + 
  geom_abline(aes(slope=1,intercept=0,color='Random guess'))

MSE <- c()
fitted_values <- as.character(fitted_values)
fitted_values <- as.numeric(fitted_values)
data$fail1 <- as.character(data$fail)
data$fail1 <- as.numeric(data$fail1)
MSE['Binomial']  <- mean(((data$fail1)-fitted_values)^2)
print(MSE)
##################################################
data = subset(data, select = -c(y_hat, y_hatnew, fitted_values, fail1) )
##################################################
#RANDOM FOREST MODEL
set.seed(1234)
split <- sample.split(data, SplitRatio = 0.8) 
split 

data_train <- subset(data, split == "TRUE") 
data_test <- subset(data, split == "FALSE") 
dim(data_train) 
head(data_train)
dim(data_test)  
head(data_test)

data$fail <- as.factor(data$fail)    
data_train$fail <- as.factor(data_train$fail)

bestmtry <- tuneRF(data_train,data_train$fail,stepFactor = 1.2, improve = 0.01, trace=T, plot= T)
set.seed(1234)
model <- randomForest(fail~.,data= data_train)
model 

importance(model)    
varImpPlot(model)

pred_test <- predict(model, newdata = data_test, type= "class")
pred_test

confusionMatrix(table(pred_test,data_test$fail)) 
#plot
reprtree:::plot.getTree(model)


pred_test <- as.character(predict(model, newdata = data_test, type= "class"))
pred_test <- as.numeric(pred_test)
data_test$fail1 <- as.character(data_test$fail)
data_test$fail1 <- as.numeric(data_test$fail1)

MSE['RandomForest']  <- mean(((data_test$fail1)-pred_test)^2)
print(MSE)

##################################################
#LASSO MODEL
set.seed(1234)
split1 <- sample.split(data, SplitRatio = 0.7) 
split1 
data_train1 <- subset(data, split1 == "TRUE") 
data_test1 <- subset(data, split1 == "FALSE") 
dim(data_train1) 
head(data_train1)
dim(data_test1)  
y <- data_train1$fail
y <- as.character(y)
y <- as.numeric(y)
x <- data.matrix(data_train1)
x <- x[, colnames(x) != "fail"]
new <- data.matrix(data_test1)
y_test <- data_test1$fail
y_test <- as.character(y_test)
y_test <- as.numeric(y_test)
new <- new[, colnames(new) != "fail"]
cv_model <- cv.glmnet(x, y, alpha = 1, family="binomial")
best_lambda <- cv_model$lambda.min
best_lambda


plot(cv_model) 
set.seed(1234)
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family="binomial")
coef <- coef(best_model)

print(coef)
lassopred <- predict(best_model, s = best_lambda, newx = new)
lassopred[lassopred>=0.5]  <- 1
lassopred[lassopred<0.5] <- 0
mat  <- as.matrix(data)
confusion_matrix  <- ftable(y_test, lassopred)
accuracy <- 100* (sum(diag(confusion_matrix)) / length(lassopred))
print(accuracy)

MSE['Lasso']  <- mean((y_test-lassopred)^2)
print(MSE)







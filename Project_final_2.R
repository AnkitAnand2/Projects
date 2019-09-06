rm(list = ls())

setwd("D:/Data Science/Project2")
getwd()
#Load Libraries
x = c("ggplot2","dplyr", "corrgram", "DMwR",  "randomForest", "unbalanced",  "e1071",
      "MASS", "rpart", "gbm", 'DataCombine', 'inTrees','caret','C50','mice')
lapply(x, require, character.only = TRUE)
rm(x)

# load the data
train_data = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))[,-1]
summary(train_data)

str(train_data)
sum(is.na(train_data))
View(train_data)

par(mfrow=c(1,1))
par(mar = rep(2,4))
hist(train_data$dteday)
hist(train_data$season)
hist(train_data$yr)
hist(train_data$mnth)
hist(train_data$holiday)
hist(train_data$weekday)
hist(train_data$workingday)
hist(train_data$weathersit)
hist(train_data$temp)
hist(train_data$atemp)
hist(train_data$hum)
hist(train_data$windspeed)
hist(train_data$casual)
hist(train_data$registered)
hist(train_data$cnt)

train_data[,1:8]= lapply (train_data[, 1:8], as.factor)
train_data[,9:15]= lapply (train_data[, 9:15], as.numeric)

str(train_data)

cnames = colnames(train_data[,c(9:12)])
# 
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt", group = 1), data = train_data)+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Num")+
           ggtitle(paste("Box plot of count",cnames[i])))
}
# 
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4, ncol=2 )


# replacing outlier with NA
for(i in cnames){
  val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
  #print(length(val))
  train_data[,i][train_data[,i] %in% val] = NA
}
######################
init = mice(train_data, maxit=0) 
meth = init$method
predM = init$predictorMatrix
set.seed(103)
train_data = mice(train_data, method='cart', predictorMatrix=predM, m=1)
train_data = complete(train_data)


sub=data.frame(train_data$registered,train_data$casual,train_data$cnt,train_data$temp,train_data$hum,train_data$atemp,train_data$windspeed)
cor(sub)
## Correlation Plot 
corrgram(train_data[,9:15], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot"
)

#to check the degree of association of categorical variable
## Chi-squared Test of Independence
factor_index = sapply(train_data,is.factor)
factor_data = train_data[,factor_index]
library(GoodmanKruskal)
plot(GKtauDataframe( factor_data), corrColors = 'blue')

## Chi-squared Test of Independence
factor_index = sapply(train_data,is.factor)
factor_data = train_data[,factor_index]

for (i in 1:6)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$season,factor_data[,i])))
}


############################################################3

train_data = subset(train_data,select=-c(dteday,atemp,hum,casual,registered,workingday ))

df = train_data
#train_data = df
###############################
  
  
set.seed(123)
train_index = sample(1:nrow(train_data), 0.7 * nrow(train_data))
colnames(train_data)

train = train_data[train_index,]
test = train_data[-train_index,]


#Decision Tree
fit = rpart(cnt  ~. , data = train_data, method = "anova")
summary(fit)
predictions_DT = predict(fit, test[,-9])

# Define MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y*100))
}


MAPE(test[,9], predictions_DT)
#[1] 22.79342

# R native funcitons

fmae = MAE(predictions_DT, test[,9])
# caret package functions 
frmse = RMSE(predictions_DT, test[,9])
fr2 = R2(predictions_DT, test[,9], form = "traditional")
cat( "MAE:", fmae, "\n", 
    "RMSE:", frmse, "\n",
    "R-squared:", fr2
    )
#MAE: 642.6354 
#RMSE: 851.9674 
#R-squared: 0.8230155

#Random Forest
RF_model = randomForest(cnt ~.  , train_data, importance = TRUE, ntree=200, mtry=2)
RF_Predictions = predict(RF_model, test[,-9])
importance(RF_model, type = 1)
MAPE(test[,9], RF_Predictions)
#[1] 12.75168
# R native funcitons
fmae = MAE(RF_Predictions, test[,9])
# caret package functions 
frmse = RMSE(RF_Predictions, test[,9])
fr2 = R2(RF_Predictions, test[,9], form = "traditional")
cat( "MAE:", fmae, "\n", 
     "RMSE:", frmse, "\n",
     "R-squared:", fr2
)
#MAE: 358.1391 
#RMSE: 486.2647 
#R-squared: 0.9423453




#Random Forest
RF_model = randomForest(cnt ~.  , train_data, importance = TRUE, ntree=500, mtry=7,nodesize=10)
RF_Predictions = predict(RF_model, test[,-9])
importance(RF_model, type = 1)

MAPE(test[,9], RF_Predictions)
# [1] 10.57839
# R native funcitons

fmae = MAE(RF_Predictions, test[,9])
# caret package functions 
frmse = RMSE(RF_Predictions, test[,9])
fr2 = R2(RF_Predictions, test[,9], form = "traditional")
cat( "MAE:", fmae, "\n", 
     "RMSE:", frmse, "\n",
     "R-squared:", fr2
)
#MAE: 295.9641 
#RMSE: 428.6273 
#R-squared: 0.955203
################################
df = subset(train_data, select = -c(holiday) )
set.seed(123)
train_index = sample(1:nrow(df), 0.7 * nrow(df))
colnames(df)

train = df[train_index,]
test = df[-train_index,]

#Random Forest_df
RF_model = randomForest(cnt ~.  , df, importance = TRUE, ntree=500, mtry=7,nodesize=10)
RF_Predictions = predict(RF_model, test[,-8])
importance(RF_model, type = 1)
MAPE(test[,8], RF_Predictions)
#[1] 10.35213
# R native funcitons

fmae = MAE(RF_Predictions, test[,8])
# caret package functions 
frmse = RMSE(RF_Predictions, test[,8])
fr2 = R2(RF_Predictions, test[,8], form = "traditional")
cat( "MAE:", fmae, "\n", 
     "RMSE:", frmse, "\n",
     "R-squared:", fr2
)

#MAE: 292.6396 
#RMSE: 422.5737 
#R-squared: 0.9564594
##############################################
#Linear Regression
set.seed(123)

train_index = sample(1:nrow(train_data), 0.7 * nrow(train_data))
colnames(train_data)

train = train_data[train_index,]
test = train_data[-train_index,]

lm_model = lm(cnt ~. , data = train_data)
summary(lm_model)
predictions_LR = predict(lm_model, test[,-9])
MAPE(test[,9], predictions_LR)
#[1] 19.68449


###############################################################


##################################################
##KNN Implementation
library(class)
#Predict test data
KNN_Predictions = knn(train[, 1:8], test[, 1:8], train$cnt, k = 5)
#convert the values into numeric
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))
#Calculate MAPE
MAPE(test[,9], KNN_Predictions)

# R native funcitons

fmae = MAE(KNN_Predictions, test[,9])
# caret package functions 
frmse = RMSE(KNN_Predictions, test[,9])
fr2 = R2(KNN_Predictions, test[,9], form = "traditional")
cat( "MAE:", fmae, "\n", 
     "RMSE:", frmse, "\n",
     "R-squared:", fr2
)
#MAE: 1052.705 
#RMSE: 1421.543 
#R-squared: 0.5072696
#[1] 33.04551




#######################
#K fold cross validation_Random Forest
model <- train(
  cnt ~ ., 
  train_data,
  method = "rf",
  trControl = trainControl(
    method = "cv", 
    number = 7,
    verboseIter = TRUE
  )
)
print(model)
################################################3
#K fold cross validation_KNN
model <- train(
  cnt ~ ., 
  train_data,
  method = "knn",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)
print(model)

########################################################

#K fold cross validation_Linear Regression
model <- train(
  cnt ~ ., 
  train_data,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)
print(model)



#################################
#view(test)
#test_result = test[,-c(9)]
#View(test_result)
#test_result$count = round(RF_Predictions, digits = 0)
#write.csv(test_result,"final_2_R.csv",row.names = F)









library(tidyverse)
library(scales)
library(gridExtra)
library(corrplot)
library(caret)
library(plotly)
library(plyr)
library(ggplot2)
library(gdata)
library(xgboost)
library(prettydoc)
library(magrittr)
library(RColorBrewer)
library(ROSE)
library(pROC)
library(ggplot2)     # to plot
library(gridExtra)   # to put more
library(grid)  
install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
## Loading Data
setwd("c://DATA//")

test <- read.csv("c://DATA//test.csv")
train <- read.csv('c://DATA//train.csv')
submission <- read.csv('c://DATA//sample_submission.csv')


## Helper Functions ??

ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }

##Determine the dimension of our dataset:
dim(train)
## View the contents and structure of our dataset:
head(train)
## 
head(test)
## View the main structure of the raw data
str(train)

### EDA  ?? {.tabset .tabset-fade .tabset-pills}

#We can draw a portrait of the ideal customer for our insurance company.

#The interested customer: 
  
#  - **Doesn’t have currently active insurance** 
#  - **Have had an accident / damaged vehicle in the past**
#  - **His vehicle is 1-2 years old**
#  - **He is between 30 and 55 years old**
#  - **He pay an annual premium between 30K and 40K**
#  - **He is more likely to use channels such as channel 25 or channel 125 than channel 155**

## Customer Response 

a1 <- train %>% mutate(c_resp = factor(Response)) %>%
  mutate(c_resp = revalue(c_resp,c("0"= "Not Interested","1"="Interested"))) %>%
  ggplot(aes(c_resp,fill=c_resp)) + 
  geom_bar(color='black') +
  scale_y_continuous(labels=ks) +
  labs(title='Customer Response',x='Customer response') + 
  theme(legend.title=element_blank()) +
  scale_fill_manual(values= c('#3C3838','#338076'),name='') +
  theme_classic() 

fig1 <- ggplotly(a1)
fig1
#

## What affect customer response? 


# Response by previous insurance
a2 <- train %>% mutate(c_resp=factor(Response),Insured=factor(Previously_Insured)) %>%
  mutate(c_resp=revalue(c_resp,c("0"='Not Interested',"1"='Interested')),Insured=revalue(Insured,c("0"='Not Insured',"1"='Insured'))) %>%
  ggplot(aes(c_resp,fill=Insured)) + 
  geom_bar(position='fill',color='black') + 
  scale_y_continuous(labels=percent) + 
  theme(legend.title = element_blank()) + 
  labs(x='Customer response',title='Response by previous insurances') +
  scale_fill_manual(values= c('#3C3838','#338076')) +
  theme_classic()

# Response by past vehicle damage
a3 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested"))) %>%
  ggplot(aes(c_resp,fill=Vehicle_Damage)) +
  geom_bar(color="black",position='fill') +
  scale_y_continuous(labels=percent) + 
  labs(x='Customer response',title='Response by vehicle history') +
  scale_fill_manual(values= c('#3C3838','#338076'),name="Damaged?") +
  theme_classic()

# Response by gender
a4 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested"))) %>%
  ggplot(aes(c_resp,fill=Gender)) +
  geom_bar(color="black",position='fill') +
  scale_y_continuous(labels=percent) + 
  labs(x='Customer response',title='Response by gender') +
  scale_fill_manual(values= c('#3C3838','#338076'),name="Gender") +
  theme_classic()

# Response by vehicle age
a5 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested"))) %>%
  ggplot(aes(c_resp,fill=Vehicle_Age)) +
  geom_bar(color="black",position='fill') +
  scale_y_continuous(labels=percent) + 
  labs(x='Customer response',title='Response by vehicle age') +
  scale_fill_manual(values= c('#3C3838','#338076',"#333B80"),name='Vehicle age') +
  theme_classic()

grid.arrange(a1,a3,a4,a5)

## Response by age

a6 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested"))) %>%
  ggplot(aes(Age,fill=c_resp)) +
  geom_density(alpha=0.7) +
  labs(title='Customer response by age') +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values= c('#3C3838','#338076'),name='Response')+
  theme_classic()
fig2 <- ggplotly(a6)
fig2



## Response by Annual Premium

a7 =  train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested"))) %>%
  ggplot(aes(Annual_Premium,fill=c_resp)) +
  geom_histogram(color='black',bins=50) +
  labs(title='Customer response by annual premium') +
  scale_x_continuous(breaks=scales::pretty_breaks(n = 25),labels=ks) +
  scale_y_continuous(labels=ks) +
  scale_fill_manual(values= c('#3C3838','#338076'),name='Response') +
  theme_classic() +
  theme(legend.position="top",axis.text.x = element_text(size =8 ,angle = 30)) 


fig3 = ggplotly(a7)
fig3


## What Affect vehicle damage?


# Age by Vehicle Damage
b1 <- train %>% ggplot(aes(Age,fill=Vehicle_Damage)) + 
  geom_density(alpha=0.7) + labs(title='Who had a vehicle damaged?') + 
  scale_y_continuous(labels=percent) + 
  scale_fill_manual(values= c('#3C3838','#338076'),name="Damaged?") +
  theme_classic()

# Vehicle damage by vehicle age
b2<- train %>% ggplot(aes(Vehicle_Damage,fill=Vehicle_Age)) + 
  geom_bar(position='fill',color='black') + 
  scale_y_continuous(labels=percent) + 
  labs(title='Vehicle damaged by age') +
  scale_fill_manual(values= c('#3C3838','#338076',"#333B80"),name="Vehicle age") +
  theme_classic()

# vehicle damage by gender
b3 <- train %>% ggplot(aes(Vehicle_Damage,fill=Gender)) + 
  geom_bar(position = 'fill',color='black') + 
  scale_y_continuous(labels=percent) + 
  labs(title='Vehicle damage by gender') +
  scale_fill_manual(values= c('#3C3838','#338076')) +
  theme_classic()

grid.arrange(b1,b2,b3,layout_matrix=rbind(c(1),c(2,3)))


## Annual Premium

# annual premium by vehicle damage
c1 <- train %>% ggplot(aes(Annual_Premium,fill=Vehicle_Damage)) + 
  geom_histogram(color='black',bins=30) + 
  scale_x_continuous(breaks=scales::pretty_breaks(n =30),labels=ks) +
  scale_y_continuous(labels=ks) +
  labs(title='Annual Premium based on past vehicle damages') + 
  scale_fill_manual(values= c('#3C3838','#338076'),name='Damaged?') +
  theme_classic() +
  theme(legend.position="top",axis.text.x = element_text(size =10 ,angle = 30))

c2 <- train %>% ggplot(aes(Annual_Premium,fill=Vehicle_Age)) + 
  geom_histogram(color='white',bins=) + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 30),labels=ks) +
  scale_y_continuous(labels=ks) +
  labs(title='Annual Premium based on Vehicle Age') + 
  scale_fill_manual(values= c('#3C3838','#338076',"#333B80"),name="Vehicle age")+
  theme_classic() +
  theme(legend.position="top",axis.text.x = element_text(size =10 ,angle = 30))

c3 <- train %>% mutate(insured=factor(Previously_Insured)) %>% 
  mutate(insured=revalue(insured,c("0"="Not Insured","1"="Insured"))) %>% 
  ggplot(aes(Annual_Premium,fill=insured)) +
  geom_histogram(color='black') + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 30),labels=ks) +
  scale_y_continuous(labels=ks) +
  labs(title='Annual Premium based on Vehicle Age') + 
  scale_fill_manual(values= c('#338076','#3C3838'),name="Status")+
  theme_classic() +
  theme(legend.position="top",axis.text.x = element_text(size =10 ,angle = 30))



grid.arrange(c1,c2)

c3
## Age distribution by Vehicle Age

d3 <- train %>% 
  ggplot(aes(Age,fill=Vehicle_Age)) + 
  geom_density(alpha=0.8) + 
  labs(title='Age distribution by Vehicle Age') + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 22)) + 
  scale_fill_manual(values=c('#3C3838','#338076',"#333B80")) + 
  theme_classic()

fig10 <- ggplotly(d3)

fig10

## Policy sales channel

train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested"))) %>%
  ggplot(aes(Policy_Sales_Channel,fill=c_resp),) + 
  geom_density(alpha=0.5,color="white") + 
  labs(title='Response by sales channel') + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 22)) + 
  scale_fill_manual(values=c('#3C3838','#338076'),name='Response') +
  theme_classic()

train %>% mutate(insured=factor(Previously_Insured)) %>% mutate(insured=revalue(insured,c("0"="Not Insured","1"="Insured"))) %>% 
  ggplot(aes(Policy_Sales_Channel,fill=insured)) + 
  geom_density(alpha=0.5,color="white") + 
  labs(title='Distribution of sales channel by Insured/Not insured customers') + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 22)) + 
  scale_fill_manual(values=c('#3C3838','#338076'),name='Have insurance?') +
  theme_classic()

train  %>% ggplot(aes(Policy_Sales_Channel,fill=Vehicle_Age)) + 
  geom_density(alpha=0.5,color="white") + 
  labs(title='Distribution of sales channel by Vehicle age') + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 22)) + 
  scale_fill_manual(values=c('#3C3838','#338076','red'),name='Vehicle age') +
  theme_classic()

train  %>% ggplot(aes(Policy_Sales_Channel,fill=Vehicle_Damage)) + 
  geom_density(alpha=0.5,color="white") + 
  labs(title='Distribution of sales channel by Vehicle Damage') + 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 22)) + 
  scale_fill_manual(values=c('#3C3838','#338076','red'),name='Damaged?') +
  theme_classic()


# Correlation  {.tabset .tabset-fade .tabset-pills}

#Remove 'id' and 'Response' columns before doing correlation matrix
numeric_cols = sapply(train, is.numeric)
train_num_only= train[, numeric_cols]

train_num_only$id = NULL
train_num_only$Response = NULL

#Correlation matrix 1 not good, since correlation can be perform on numeric only

cor_result = cor(as.matrix(train_num_only))
class(train_corr)
head(train_corr)
train_corr <- train_num_only %>% mutate_if(is.numeric, list(scale))
cor(train_corr, use="pairwise.complete.obs", method = "spearman")   %>%  
  
  corrplot( method = "pie", outline = T, addgrid.col = "darkgray", mar = c(0.1,0,0.1,0), order = 'AOE', type = "full",rect.col = "black", rect.lwd = 5, cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex =.5,tl.srt=50,col=c('#3C3838',"gray",'#338076'),number.cex = 7/ncol(train_corr))


## Correlation matrix 2
numeric_cols1 = sapply(train, is.numeric)
train_num_only1= train[, numeric_cols]

train_num_only$id = NULL
train_num_only$Response = NULL
class(train_corr)
head(train_corr)
train_corr <- train_num_only %>% mutate_if(is.numeric, list(scale))
cor(train_num_only1, use="pairwise.complete.obs", method = "spearman")   %>%  
  
  corrplot( method = "pie", outline = T, addgrid.col = "darkgray", mar = c(0.1,0,0.1,0), order = 'AOE', type = "full",rect.col = "black", rect.lwd = 5, cl.pos = "b", tl.col = "black", tl.cex = 1, cl.cex =.5,tl.srt=50,col=c('#3C3838',"gray",'#338076'),number.cex = 7/ncol(train_corr))

# XGB MODEL {.tabset .tabset-fade .tabset-pills}
#**How deal with am imbalanced dataset? Well there are many ways, most of them are correct, but, this time I tried a different approach**
  
#  **How we proceed:**
  
#  - We will tune some XGB parameters with gridsearch.
#- We will launch XGB Classifier on imbalanced dataset(the original dataset),fixing scale_pos_weight : sum(negative)/sum(positive)
#- We will evaluate XGB Classifier on a balanced dataset of the same length of the train dataset
#- Submit results

#**Since all customers in submission dataset are not interested we will take in consideration only metric evaulation of model**
  
  ## XGB Gridsearch
  
#Oversampling for gridsearch
ov_sampling <- ovun.sample(formula = Response ~. ,data=train,method='both',N=100,seed=12)
train_2<- ov_sampling$data
# GRID SEARCH
xgb_gridsearch <- function(train,ntrees)
{
  set.seed(1)
  print("Best hyperparameters combination:")
  
  # One hot encoding
  dmy <- dummyVars(" ~ .", data = train)
  train_data <- data.frame(predict(dmy, newdata = train))
  
  # as.numeric
  for (i in ncol(train_data)){
    train_data[[i]] = as.numeric(train_data[[i]])
  }
  
  # Factor Response for grid search
  grid_train = train_data
  grid_train$Response = factor(grid_train$Response)
  levels(grid_train$Response) <- c("X0","X1")
  
  ntrees <- ntrees
  
  # parameters grid
  xgb_grid_1 = expand.grid(
    nrounds = ntrees,
    eta = seq(2,10,by=1)/ntrees,
    max_depth = c(6, 8, 10),
    gamma = 0,
    subsample = c(0.5, 0.75, 1),
    min_child_weight = c(1,2) ,
    colsample_bytree = c(0.3,0.5)
  )
  
  xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 10,
    verboseIter = FALSE,
    returnData = FALSE,
    returnResamp = "all", # save losses across all models
    classProbs = TRUE, # set to TRUE for AUC to be computed
    summaryFunction = twoClassSummary,
    allowParallel = TRUE,
    
  )
  
  xgb_train_1 = train(
    x = as.matrix(grid_train %>% select(-Response)),
    y = factor(grid_train$Response),
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree",
  )
  
  out= xgb_train_1$bestTune
  print(out)
  return(out)  
}

best_tune = xgb_gridsearch(train= train_2,ntrees = 100)
```

## XGB model
**scale_pos_weight= 334399/46710 = 7.15**
  
  ```{r}
knitr::kable(table(train$Response),
             col.names = c('Response','#'))

#### Launch Xgboost 
train_xgt <- function(train_data,test_data,best_tune)
{
  set.seed(123)
  print("XGB Train:")
  # One hot encoding
  dmy <- dummyVars(" ~ .", data = train_data)
  train_data <- data.frame(predict(dmy, newdata = train_data))
  
  # Select data & label
  train_data <- as.matrix(train_data)
  d_ata <- ncol(train_data)
  data_train <- train_data[,1:d_ata-1]
  label_train <- train_data[,d_ata]
  # Transform data
  data_train <- as.matrix(data_train)
  label_train <- as.numeric(label_train)
  #XGB Model
  dtrain = xgb.DMatrix(data_train,label=label_train)
  machine = xgboost(data= dtrain,  objective = "binary:logistic",
                    # paramaters
                    max_depth = best_tune$max_depth,
                    nrounds=1000,
                    colsample_bytree = best_tune$colsample_bytree,
                    gamma = best_tune$gamma,
                    min_child_weight = best_tune$min_child_weight,
                    eta = best_tune$eta, 
                    subsample = best_tune$subsample,
                    print_every_n = 200,
                    scale_pos_weight=7.15,
                    max_delta_step=1,
                    # others
                    verbose=1,
                    nthread = 4,
                    eval.metric = "auc")
  # Cross Validation
  print('XGB Cross Validation:')
  cv  <-  xgb.cv(data = dtrain, nround = 1000,
                 print_every_n = 200,
                 nthread = 4,
                 verbose = TRUE,
                 eval_metric= "auc",
                 nfold = 5, 
                 objective = "binary:logistic")
  # Encoding Test data & Transform
  dmy1 <- dummyVars(" ~ .", data = test_data)
  test_data <- data.frame(predict(dmy1, newdata = test_data))
  test_data <- as.matrix(test_data)
  
  # Predictions
  pred <- predict(machine, test_data)
  # Importance plot
  importance_matrix <- xgb.importance(colnames(data_train), model = machine)
  gg <- xgb.ggplot.importance(importance_matrix, rel_to_first = TRUE,xlab="Relative Importance")
  gg <- gg + ggplot2::ylab("Relative importance")
  
  out <- list("Features Importance:" = gg,
              Predictions=pred)
  
  return(out)  
}
train_xgt(train_data=train,test_data = test,best_tune=best_tune)
test_predictions <- train_xgt(train_data=train,
                              test_data = test,
                              best_tune=best_tune)$Predictions

``` 

## Test XGB on a balanced dataset
**As a further test for our model, we will rebalance the original dataset and evaluate the model performance on a balanced dataset**
  ```{r}
# Rebalance dataset
ov_sampling <- ovun.sample(formula = Response ~. ,data=train,method='both',N=381109,seed=12)
train_3<- ov_sampling$data
paste("Length of train_3 dataset(Balanced dataset):",nrow(train_3))
knitr::kable(prop.table(table(train_3$Response)),
             col.names = c('Response','%'),)

#### Launch Xgboost ####
train_xgt <- function(train_data,best_tune)
{
  set.seed(123)
  print("XGB is starting...")
  # One hot encoding
  dmy <- dummyVars(" ~ .", data = train_data)
  train_data <- data.frame(predict(dmy, newdata = train_data))
  
  # Select data and target
  train_data <- as.matrix(train_data)
  d_ata <- ncol(train_data)
  data_train <- train_data[,1:d_ata-1]
  label_train <- train_data[,d_ata]
  
  # Transform data  
  data_train <- as.matrix(data_train)
  label_train <- as.numeric(label_train)
  print("XGB Train:")
  # XGB Model  
  dtrain = xgb.DMatrix(data_train,label=label_train)
  machine = xgboost(data= dtrain,  objective = "binary:logistic",
                    # paramaters
                    max_depth = best_tune$max_depth,
                    nrounds=1000,
                    colsample_bytree = best_tune$colsample_bytree,
                    gamma = best_tune$gamma,
                    min_child_weight = best_tune$min_child_weight,
                    eta = best_tune$eta, 
                    subsample = best_tune$subsample,
                    print_every_n = 200,
                    max_delta_step=1,
                    # others
                    verbose=1,
                    nthread = 4,
                    eval.metric = "auc")
  print("XGB Cross Validation:")
  # Cross Validation
  cv  <-  xgb.cv(data = dtrain, nround = 1000, 
                 print_every_n= 200,
                 verbose = TRUE,
                 eval_metric= "auc",
                 nfold = 5, 
                 nthread = 4,
                 objective = "binary:logistic",
                 prediction=T)
  # Prediction
  # # predict on test label
  pred <- predict(machine, dtrain)
  prediction <- as.numeric(pred > 0.5)
  # Confusion Matrix
  cm = confusionMatrix(factor(prediction),factor(label_train),positive="1")
  
  cm_d <- as.data.frame(cm$table)
  # confusion matrix statistics as data.frame
  cm_st <-data.frame(cm$overall)
  # round the values
  cm_st$cm.overall <- round(cm_st$cm.overall,2)
  # here we also have the rounded percentage values
  cm_p <- as.data.frame(prop.table(cm$table))
  cm_d$Perc <- round(cm_p$Freq*100,2)
  
  # plotting the matrix
  cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
    geom_tile() +
    geom_text(aes(label = paste("",Freq,",",Perc,"%")), color = 'white', size = 4) +
    theme_light() +
    scale_fill_gradient(low = "#3C3838", high = "#338076") + 
    scale_x_discrete(position = "top") +
    guides(fill=FALSE) 
  
  # plotting the stats
  colnames(cm_st) <- ('Statistics')
  
  cm_st_p <-  tableGrob(cm_st)
  
  # all together
  conf_mat = grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
                          top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))
  # Roc Curve
  roc.curve = roc(response = label_train,predictor = cv$pred,levels=c(0, 1)) 
  plot_roc = plot(roc.curve,main="ROC Curve",col="darkgreen")
  # Importance plot
  importance_matrix <- xgb.importance(colnames(data_train), model = machine)
  gg <- xgb.ggplot.importance(importance_matrix, rel_to_first = TRUE,xlab="Relative Importance")
  gg <- gg + ggplot2::ylab("Relative importance")
  
  
  # Out
  out <- list("Features Importance:" = gg,
              "Confusion Matrix"= conf_mat,
              "Roc Curve" = plot_roc)
  
  return(out)  
}

train_xgt(train_data=train_3,
          best_tune=best_tune)

```

# Submission (Checking Results) {.tabset .tabset-fade .tabset-pills}
## Submit XGB predictions
```{r}
last_submission <- cbind(submission,predicted_response=round(test_predictions),real_predictions=test_predictions)
head(last_submission,5)
```







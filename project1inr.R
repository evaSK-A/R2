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
```{r}
test <- read.csv("c://DATA//test.csv")
train <- read.csv('c://DATA//train.csv')
submission <- read.csv('c://DATA//sample_submission.csv')
```

## Helper Functions
```{r}
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }


```
head(train)

## Customer Response 
```{r}
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
```
## What affect customer response? 
```{r}

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

```

## Response by age
```{r}
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

```

## Response by Annual Premium
```{r}
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
```
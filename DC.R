##########################################################
# Load the necessary libraries
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(curl)) install.packages("curl", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)
library(ggthemes)
library(GGally)
library(purrr)
library(curl)


#Multiplot Function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
##########################################################
#Load the dataset:
#You can find the dataset in the link below, but the code runs well as it is linked to Google Drive:
#1. URL: https://github.com/Farmentano12/FinalProject_DC/blob/main/drivers_churn_full.csv
##########################################################
#The file is hosted in Google Drive:
id <- "1fk-_ZEso96zG3F-Ox27Xn01URRDtTe_u" # google file ID
dc <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
dc <- dc %>% select(-ID)
head(dc,20)
names(dc)
summary(dc)

##########################################################
#A. Explanation of the dataset and context:
##########################################################
#It is a dataset of drivers working for a ride-hailing APP during a complete month in a city of LATAM.
  #Drivers have been anonimized by removing column 'DriverID' and adding 'ID' instead.
  #No information of the month or year is given, as well as the country or region.
  #The objective is to predict churn, and better understand drivers motivations, in order to set a bonus or other incentive in order to prevent churn. 

##########################################################
#B. The explanation of fields of the dataset are the followings:
##########################################################
#1. DriverTenure (character): It is related to how much time the driver has been driving with the APP.
dc %>% group_by(DriverTenure) %>% summarize(drivers = n()) %>%
  ggplot(aes(DriverTenure,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to their tenure") + theme_economist() +
  xlab("Driver Tenure")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        legend.margin = element_text(margin = margin(b = 6, unit = "cm")),
        axis.text.x = 
          element_text(size = 8, margin = margin(t = -1, unit = "cm"), angle = 45),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = 1, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(DriverTenure) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

  #2. FleetRole (character): Wheter the drivers is the owner of the car (DWC) or is a driving somebody else's car (DWOC)
dc %>% group_by(FleetRole) %>% summarize(drivers = n()) %>%
  ggplot(aes(FleetRole,drivers, label = drivers, vjust=-1)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to their fleet role") + theme_economist() +
  xlab("Drivers with and without car")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(margin = margin(b = .25, unit = "cm")),
        legend.margin = element_text(margin = margin(t = 1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .1, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(FleetRole) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))


  #3. DriverType (character): There three types of drivers:
      #. New: The ones driving in their first month.
      #. Recurrent: Drivers with journeys in month - 1.
      #. Reactivated: Drivers without journeys in month -1 but with previous journeys.
dc %>% group_by(DriverType) %>% summarize(drivers = n()) %>%
  ggplot(aes(DriverType,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to their type") + theme_economist() +
  xlab("Driver Type")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(DriverType) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

  #4. DO (numeric): Quantity of journeys completed in the month.
dc %>%  ggplot(aes(x = DO)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of drivers according to their journeys") + theme_economist() +
  xlab("Journeys (10 classes)")  +
  ylab("Quantity of drivers in class")  +
  theme(legend.position='bottom',
        title  = 
          element_text(margin = margin(b = .5, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

  #5. WorkingHours (numeric): Quantity of hours of connection in the month.
dc %>%  ggplot(aes(x = WorkingHours)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of drivers according to their working hours") + theme_economist() +
  xlab("Working Hours (10 classes)")  +
  ylab("Quantity of drivers in class")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

  #6. AVG Ticket (numeric): Amount of earning per journey of the driver.
p1 <- dc %>%  ggplot(aes(y = AVGTicket)) +
  geom_boxplot() +
  #geom_histogram(bins = 15, color = "black") +
  #stat_bin(bins = 15, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Boxplot of average earning per journey") + theme_economist() +
  ylab("Average earning per journey")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

p2 <- dc %>%  ggplot(aes(x = AVGTicket)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of average earning per journey") + theme_economist() +
  ylab("Drivers in classes")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(l = -1, r = 1, b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 8, margin = margin(t = .2, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

multiplot(p1, p2, cols=2)

  #7. Cost (numeric): Amount of income generated by the driver in the month.
p1 <- dc %>%  ggplot(aes(y = Cost)) +
  geom_boxplot() +
  ggtitle("Boxplot of income per driver") + theme_economist() +
  ylab("Income per driver")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 10, margin = margin(r = .25, unit = "cm")))

p2 <- dc %>%  ggplot(aes(x = Cost)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of income per driver") + theme_economist() +
  ylab("Drivers in classes")  +
  xlab("Drivers income")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(l = -1, r = 1, b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 8, margin = margin(t = .2, unit = "cm")),
        axis.title.y =  
          element_text(size = 10, margin = margin(r = .25, unit = "cm")))

multiplot(p1, p2, cols=2)

  #8. LoadFactor (numeric): proportion of connection time in which the driver is executing a journey.
dc %>%  ggplot(aes(x = LoadFactor)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of load factor of drivers") + theme_economist() +
  xlab("Classes")  +
  ylab("Quantity of drivers in class")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

  #9. Frequent Moment (character): Binary variable that is HDM (High Demand Moment) if the 50% or more of his connected time, the driver does it while High Demand moments and LDM (Low Demand Moments) in any other way.
dc %>% group_by(FrequentMoment) %>% summarize(drivers = n()) %>%
  ggplot(aes(FrequentMoment,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to moment of connection") + theme_economist() +
  xlab("Frequent Connection Moment")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(FrequentMoment) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

  #10. CorpProd (character): Binary variable that is YES if driver is elegible for corporate journeys.
dc %>% group_by(CorpProd) %>% summarize(drivers = n()) %>%
  ggplot(aes(CorpProd,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers with corporate journeys") + theme_economist() +
  xlab("Corporate Journeys")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(CorpProd) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

  #11. Frequent Zone (character): Binary variable that is HDZ (High Demand Zone) if the 50% or more of his connected time, the driver does it in a High Demand zone and LDZ (Low Demand Zone) in any other way.
dc %>% group_by(FrequentZone) %>% summarize(drivers = n()) %>%
  ggplot(aes(FrequentZone,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to zone of connection") + theme_economist() +
  xlab("Frequent Connection Zone")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(FrequentZone) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

  #12. Churn (predictor): whether the driver worked (NO, it means the driver "churned") or not (YES) in month + 1.

##########################################################
#C. Dataset prevalence
##########################################################
# Scatter plot matrix with GGally, coloring by cyl
dc %>% group_by(Churn) %>% summarize(Drivers = n()) %>%
  ggplot(aes(Churn,Drivers, label = Drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers churning") + theme_economist() +
  xlab("Churn")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(Churn) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

##########################################################
#D. Numeric variables and churn
##########################################################
# Scatter plot matrix with GGally, coloring by cyl
dc %>%  ggpairs(columns = c("DO", "WorkingHours", "AVGTicket", "Cost"), 
          aes(color = Churn),
          upper = list(continuous = wrap('cor', size = 5)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

#D.Create data partition
dc_2 <- dc %>% mutate(#DriverTenure = as.factor(DriverTenure),
                      #FleetRole = as.factor(FleetRole),
                      #DriverType = as.factor(DriverType),
                      #FrequentMoment = as.factor(FrequentMoment),
                      #CorpProd = as.factor(CorpProd),
                      #FrequentZone = as.factor(FrequentZone),
                      Churn = as.factor(Churn))
summary(dc_2) 

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
set.seed(1)
test_index <- createDataPartition(dc_2$Churn, times = 1, p = 0.1, list = FALSE)
training_set <- dc_2[-test_index,]
dim(training_set)
test_set <- dc_2[test_index,]
dim(test_set)

##########################################################
#E.Define models
##########################################################
#"lda","qda","glm", "knn_bootstrap", "knn_crosvalidation",
#"ctm","rf","naive_bayes", "svmLinear","gamLoess", "multinom", "adaboost"
##########################################################
#F. Train models
##########################################################
#F.1. LDA
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_lda <- train(Churn ~ ., data = training_set, method = "lda")
train_lda$results['Accuracy']
e1 <- as.numeric(train_lda$results['Accuracy'][1])
e1

  #F.2. QDA --> No puedo entrenarlo
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_qda <- train(Churn ~ DO + WorkingHours + AVGTicket + Cost,
                  data = training_set, method = "qda")
train_qda$results['Accuracy']
e2 <- as.numeric(train_qda$results['Accuracy'][1])
e2

  #F.3. Logistic Regression
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_glm <- train(Churn ~ ., data = training_set, method = "glm")
train_glm$results['Accuracy']
e3 <- as.numeric(train_glm$results['Accuracy'][1])
e3

  #F.4. KNN --> Default bootstrap
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_knn <- train(Churn ~ ., data = training_set, 
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(180, 310, 5))) #This is for k

ggplot(train_knn, highlight = TRUE) +
  ggtitle("Default KNN Bootstrap") + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(train_knn$bestTune[1]),
           y = as.numeric(max(train_knn$results['Accuracy']))*1.0003,
          label = paste("Max Accuracy",round(as.numeric(max(train_knn$results['Accuracy'])),4))) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

train_knn$bestTune
max(train_knn$results['Accuracy'])
train_knn$finalModel
e4 <- as.numeric(max(train_knn$results['Accuracy']))
e4

  #F.5. KNN --> Cross Validation
control <- trainControl(method = "cv", number = 10, p = .9)
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_knn_2 <- train(Churn ~ ., data = training_set, 
                     method = "knn", 
                     tuneGrid = data.frame(k = seq(150, 310, 5)),
                     trControl = control) #This is for k

ggplot(train_knn_2, highlight = TRUE) +
  ggtitle("KNN with Cross-Validation") + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(train_knn_2$bestTune[1]),
           y = as.numeric(max(train_knn_2$results['Accuracy']))*1.0003,
           label = paste("Max Accuracy",round(as.numeric(max(train_knn_2$results['Accuracy'])),4))) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))
train_knn_2$bestTune
max(train_knn_2$results['Accuracy'])
train_knn_2$finalModel
e5 <- as.numeric(max(train_knn_2$results['Accuracy']))
e5

  #F.6. Classification tree model (CTM)
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_rpart <- train(Churn ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = training_set)
ggplot(train_rpart) + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(train_rpart$results$cp[which.max(train_rpart$results$Accuracy)]),
           y = as.numeric(train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)])*1.005,
           label = paste("Max Acc",
                         paste(round(train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)],4),
                               "CP =", as.numeric(train_rpart$results$cp[which.max(train_rpart$results$Accuracy)]))), size = 3) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))
train_rpart$finalModel$cptable
plot(train_rpart$finalModel, margin = .01)
text(train_rpart$finalModel, 1 )
#QUÉ NÚMERO DE ITERACIÓN - ÍNDICE FUE EL MAYOR
which.max(train_rpart$results$Accuracy)
#PARA QUÉ VALOR DE CP
train_rpart$results$cp[which.max(train_rpart$results$Accuracy)]
#QUÉ VALOR DE ACCURACY DIO
train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)]
e6 <- as.numeric(train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)])
e6

  #F.7. Random Forest
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_rf <- train(Churn ~ ., method = "rf",
                  tuneGrid = data.frame(mtry = seq(1:5)),
                  data = training_set,
                  ntree = 200)

ggplot(train_rf) + theme_economist() +
  ggtitle("random Forests") + 
  annotate(geom = "text",
           x = as.numeric(which.max(train_rf$results$Accuracy)[1]),
           y = as.numeric(train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)])*1.0007,
           label = paste("Max Accuracy",round(train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)],4))) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))
varImp(train_rf)
train_rf$results$Accuracy

which.max(train_rf$results$Accuracy)
train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)]
e7 <- as.numeric(train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)])
e7

  #F.8. Naive Bayes
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_nb <- train(Churn ~ ., method = "naive_bayes",
                  data = training_set)
which.max(train_nb$results$Accuracy)
train_nb$results$Accuracy[which.max(train_nb$results$Accuracy)]
e8 <- as.numeric(train_nb$results$Accuracy[which.max(train_nb$results$Accuracy)])
e8

  #F.9. svmLinear
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_smv <- train(Churn ~ ., method = "svmLinear",
                  data = training_set)
train_smv$results['Accuracy']
e9 <- as.numeric(train_smv$results['Accuracy'])
e9

  #F.10. gamLoess
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_gamLoess <- train(Churn ~ DO + WorkingHours + AVGTicket + Cost, method = "gamLoess",
                   data = training_set)
train_gamLoess$results['Accuracy']
e10 <- as.numeric(train_gamLoess$results['Accuracy'])
e10

  #F.11. multinom
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_multinom <- train(Churn ~ ., method = "multinom",
                        data = training_set, trace = FALSE)
which.max(train_multinom$results$Accuracy)
train_multinom$results$Accuracy[which.max(train_multinom$results$Accuracy)]
e11 <- as.numeric(train_multinom$results$Accuracy[which.max(train_multinom$results$Accuracy)])
e11

##########################################################
#G. Build a table with models and results
##########################################################
model_name <- c("lda","qda","glm", "knn_bootstrap", "knn_crosvalidation",
            "ctm","rf","naive_bayes", "svmLinear",
            "gamLoess", "multinom")

models <- c("train_lda","train_qda","train_glm","train_knn","train_knn_2",
            "train_rpart","train_rf","train_nb","train_smv",
            "train_gamLoess","train_multinom")

results <- c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11)
results
results <- as.data.frame(results)
row.names(results) <- NULL
results

##########################################################
#H. Pick models with average results above the mean
##########################################################
selected_models <- cbind(model_name,models,results)
selected_models <- as.data.frame(selected_models)
row.names(selected_models) <- NULL
selected_models
mean_accuracy <- mean(selected_models$results)
mean_accuracy
#As accuracy of qda is so low, and represents an outlier, we drop it to calculate the mean and the selected models:
selected_models <- selected_models %>% filter(results > .75)
selected_models
mean_accuracy <- mean(selected_models$results)
mean_accuracy
selected_models <- selected_models %>% mutate(selected = 
                                                ifelse(results > mean(results),"YES","NO"))

selected_models

final_models <- selected_models %>% filter(selected == "YES") %>%
                   select(-selected) %>% rename(model = model_name, train_name = models)

final_models

##########################################################
#I. Make predictions
##########################################################
#We evaluate the results in the test set:
  #I.a. KNN bootstrap:
prediction_knn_bootstrap <- predict(train_knn, newdata = test_set)
knn_bootstrap_acc_test <- as.numeric(confusionMatrix(data = prediction_knn_bootstrap, reference = test_set$Churn)$overall["Accuracy"][1])
knn_bootstrap_acc_test

  #I.b. KNN crossvalidation:
prediction_knn_cv <- predict(train_knn_2, newdata = test_set)
prediction_knn_cv <- as.data.frame(prediction_knn_cv)
prediction_knn_cv
knn_cv_acc_test <- as.numeric(confusionMatrix(data = prediction_knn_cv, reference = test_set$Churn)$overall["Accuracy"][1])
knn_cv_acc_test

  #I.c. CTM:
prediction_ctm <- predict(train_rpart, newdata = test_set)
prediction_ctm <- as.data.frame(prediction_ctm)
prediction_ctm
knn_ctm_acc_test <- as.numeric(mean(prediction_ctm == test_set$Churn))
knn_ctm_acc_test

  #I.d. RF:
prediction_rf <- predict(train_rf, newdata = test_set)
prediction_rf <- as.data.frame(prediction_rf)
prediction_rf
knn_rf_acc_test <- as.numeric(confusionMatrix(data = prediction_rf, reference = test_set$Churn)$overall["Accuracy"][1])
knn_rf_acc_test

  #I.e. GL:
prediction_gl <- predict(train_gamLoess, newdata = test_set)
prediction_gl <- as.data.frame(prediction_gl)
prediction_gl
knn_gl_acc_test <- as.numeric(confusionMatrix(data = prediction_gl, reference = test_set$Churn)$overall["Accuracy"][1])
knn_gl_acc_test

Accuracy_Test_Set <- c(knn_bootstrap_acc_test,knn_cv_acc_test,
                      knn_ctm_acc_test,knn_rf_acc_test,knn_gl_acc_test)

Accuracy_Test_Set

final_table <- cbind(final_models,Accuracy_Test_Set) 
final_table <- as.data.frame(final_table)
final_table <- final_table %>% rename(Accuracy_Train_Set = results)
final_table

##########################################################
#J. Combine models
##########################################################
ensemble_predictions <- cbind(prediction_knn_bootstrap,prediction_knn_cv,
                              prediction_ctm,prediction_rf,prediction_gl)

head(ensemble_predictions,20)

ensemble_predictions <- ensemble_predictions %>% mutate(
                        prediction_knn_bootstrap = ifelse(prediction_knn_bootstrap == "NO",0,1),
                        prediction_knn_cv = ifelse(prediction_knn_cv == "NO",0,1),
                        prediction_ctm = ifelse(prediction_ctm == "NO",0,1),
                        prediction_rf = ifelse(prediction_rf == "NO",0,1),
                        prediction_gl = ifelse(prediction_gl == "NO",0,1),
)

head(ensemble_predictions,20)

ensemble_predictions <- ensemble_predictions %>% mutate(
                        Final_Decision = ifelse(prediction_knn_bootstrap+prediction_knn_cv+
                          prediction_ctm+prediction_rf+prediction_gl >= 3,"YES","NO"))


head(ensemble_predictions,20)

mean(ensemble_predictions$Final_Decision == test_set$Churn)

confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)

TClass <- factor(c("NO","YES","YES","NO"))
PClass <- factor(c("NO","NO","YES","YES"))
Y      <- c(confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[1],
            confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[3],
            confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[4],
            confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[2])
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) + 
  ggtitle("Confusion Matrix for Balanced Dataset") + 
  xlab("Actual Class - Churn")  +
  ylab("Predicted Class - Churn")  +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

##########################################################
#--------- ALL THE SAME FOR A BALANCED DATASET
##########################################################
#Load the dataset:
#You can find the dataset in the link below, but the code runs well as it is linked to Google Drive:
#2. URL: https://github.com/Farmentano12/FinalProject_DC/blob/main/drivers_churn.csv
##########################################################
#Load the dataset:
#The file is hosted in Google Drive:
id <- "1bfPvLbhAd3Pp_69UTXhDu0tbN9L-IFLE" # google file ID
dc <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
dc <- dc %>% select(-ID)
head(dc)
names(dc)
summary(dc)

#A. Explanation of the dataset and context:
#It is a dataset of drivers working for a ride-hailing APP during a complete month in a city of LATAM.
#Drivers have been anonimized by removing column 'DriverID' and adding 'ID' instead.
#No information of the month or year is given, as well as the country or region.
#The objective is to predict churn, and better understand drivers motivations, in order to set a bonus or other incentive in order to prevent churn. 

#B. The explanation of fields of the dataset are the followings:
#1. DriverTenure (character): It is related to how much time the driver has been driving with the APP.
dc %>% group_by(DriverTenure) %>% summarize(drivers = n()) %>%
  ggplot(aes(DriverTenure,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to their tenure") + theme_economist() +
  xlab("Driver Tenure")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        legend.margin = element_text(margin = margin(b = 6, unit = "cm")),
        axis.text.x = 
          element_text(size = 8, margin = margin(t = -1, unit = "cm"), angle = 45),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = 1, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(DriverTenure) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

#2. FleetRole (character): Wheter the drivers is the owner of the car (DWC) or is a driving somebody else's car (DWOC)
dc %>% group_by(FleetRole) %>% summarize(drivers = n()) %>%
  ggplot(aes(FleetRole,drivers, label = drivers, vjust=-1)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to their fleet role") + theme_economist() +
  xlab("Drivers with and without car")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(margin = margin(b = .25, unit = "cm")),
        legend.margin = element_text(margin = margin(t = 1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .1, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(FleetRole) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))


#3. DriverType (character): There three types of drivers:
#. New: The ones driving in their first month.
#. Recurrent: Drivers with journeys in month - 1.
#. Reactivated: Drivers without journeys in month -1 but with previous journeys.
dc %>% group_by(DriverType) %>% summarize(drivers = n()) %>%
  ggplot(aes(DriverType,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to their type") + theme_economist() +
  xlab("Driver Type")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(DriverType) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

#4. DO (numeric): Quantity of journeys completed in the month.
dc %>%  ggplot(aes(x = DO)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of drivers according to their journeys") + theme_economist() +
  xlab("Journeys (10 classes)")  +
  ylab("Quantity of drivers in class")  +
  theme(legend.position='bottom',
        title  = 
          element_text(margin = margin(b = .5, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

#5. WorkingHours (numeric): Quantity of hours of connection in the month.
dc %>%  ggplot(aes(x = WorkingHours)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of drivers according to their working hours") + theme_economist() +
  xlab("Working Hours (10 classes)")  +
  ylab("Quantity of drivers in class")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

#6. AVG Ticket (numeric): Amount of earning per journey of the driver.
p1 <- dc %>%  ggplot(aes(y = AVGTicket)) +
  geom_boxplot() +
  #geom_histogram(bins = 15, color = "black") +
  #stat_bin(bins = 15, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Boxplot of average earning per journey") + theme_economist() +
  ylab("Average earning per journey")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

p2 <- dc %>%  ggplot(aes(x = AVGTicket)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of average earning per journey") + theme_economist() +
  ylab("Drivers in classes")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(l = -1, r = 1, b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 8, margin = margin(t = .2, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

multiplot(p1, p2, cols=2)

#7. Cost (numeric): Amount of income generated by the driver in the month.
p1 <- dc %>%  ggplot(aes(y = Cost)) +
  geom_boxplot() +
  ggtitle("Boxplot of income per driver") + theme_economist() +
  ylab("Income per driver")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 10, margin = margin(r = .25, unit = "cm")))

p2 <- dc %>%  ggplot(aes(x = Cost)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of income per driver") + theme_economist() +
  ylab("Drivers in classes")  +
  xlab("Drivers income")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 6,margin = margin(l = -1, r = 1, b = .5, unit = "cm")),
        axis.title.x =  
          element_text(size = 8, margin = margin(t = .2, unit = "cm")),
        axis.title.y =  
          element_text(size = 10, margin = margin(r = .25, unit = "cm")))

multiplot(p1, p2, cols=2)

#8. LoadFactor (numeric): proportion of connection time in which the driver is executing a journey.
dc %>%  ggplot(aes(x = LoadFactor)) +
  geom_histogram(bins = 10, color = "black") +
  stat_bin(bins = 10, geom="text", aes(label=..count..), vjust=-.5) +
  ggtitle("Histogram of load factor of drivers") + theme_economist() +
  xlab("Classes")  +
  ylab("Quantity of drivers in class")  +
  theme(legend.position='bottom',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .25, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

#9. Frequent Moment (character): Binary variable that is HDM (High Demand Moment) if the 50% or more of his connected time, the driver does it while High Demand moments and LDM (Low Demand Moments) in any other way.
dc %>% group_by(FrequentMoment) %>% summarize(drivers = n()) %>%
  ggplot(aes(FrequentMoment,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to moment of connection") + theme_economist() +
  xlab("Frequent Connection Moment")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(FrequentMoment) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

#10. CorpProd (character): Binary variable that is YES if driver is elegible for corporate journeys.
dc %>% group_by(CorpProd) %>% summarize(drivers = n()) %>%
  ggplot(aes(CorpProd,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers with corporate journeys") + theme_economist() +
  xlab("Corporate Journeys")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(CorpProd) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

#11. Frequent Zone (character): Binary variable that is HDZ (High Demand Zone) if the 50% or more of his connected time, the driver does it in a High Demand zone and LDZ (Low Demand Zone) in any other way.
dc %>% group_by(FrequentZone) %>% summarize(drivers = n()) %>%
  ggplot(aes(FrequentZone,drivers, label = drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers according to zone of connection") + theme_economist() +
  xlab("Frequent Connection Zone")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(FrequentZone) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

#12. Churn (predictor): whether the driver worked (NO, it means the driver "churned") or not (YES) in month + 1.

#B. Dataset prevalence
# Scatter plot matrix with GGally, coloring by cyl
dc %>% group_by(Churn) %>% summarize(Drivers = n()) %>%
  ggplot(aes(Churn,Drivers, label = Drivers, vjust=-.5)) +
  geom_bar(stat="identity",position="dodge") +
  ggtitle("Quantity of drivers churning") + theme_economist() +
  xlab("Churn")  +
  ylab("Drivers")  +
  geom_text_repel() +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

dc %>% group_by(Churn) %>% summarize(Drivers = n()) %>% mutate(Prop = Drivers/sum(Drivers))

#C. Numeric variables and churn
# Scatter plot matrix with GGally, coloring by cyl
dc %>%  ggpairs(columns = c("DO", "WorkingHours", "AVGTicket", "Cost"), 
                aes(color = Churn),
                upper = list(continuous = wrap('cor', size = 5)),
                diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

#D.Create data partition
dc_2 <- dc %>% mutate(#DriverTenure = as.factor(DriverTenure),
  #FleetRole = as.factor(FleetRole),
  #DriverType = as.factor(DriverType),
  #FrequentMoment = as.factor(FrequentMoment),
  #CorpProd = as.factor(CorpProd),
  #FrequentZone = as.factor(FrequentZone),
  Churn = as.factor(Churn))
summary(dc_2) 

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
set.seed(1)
test_index <- createDataPartition(dc_2$Churn, times = 1, p = 0.1, list = FALSE)
training_set <- dc_2[-test_index,]
dim(training_set)
test_set <- dc_2[test_index,]
dim(test_set)

#E.Define models
#"lda","qda","glm", "knn_bootstrap", "knn_crosvalidation",
#"ctm","rf","naive_bayes", "svmLinear","gamLoess", "multinom", "adaboost"

#F. Train models


#F.1. LDA
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_lda <- train(Churn ~ ., data = training_set, method = "lda")
train_lda$results['Accuracy']
e1 <- as.numeric(train_lda$results['Accuracy'][1])
e1

#y_hat <- predict(train_lda, test_set)
#confusionMatrix(data = y_hat, reference = test_set$Churn)$overall["Accuracy"]

#F.2. QDA --> No puedo entrenarlo
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_qda <- train(Churn ~ DO + WorkingHours + AVGTicket + Cost,
                   data = training_set, method = "qda")
train_qda$results['Accuracy']
e2 <- as.numeric(train_qda$results['Accuracy'][1])
e2

#F.3. Logistic Regression
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_glm <- train(Churn ~ ., data = training_set, method = "glm")
train_glm$results['Accuracy']
e3 <- as.numeric(train_glm$results['Accuracy'][1])
e3

#F.4. KNN --> Default bootstrap
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_knn <- train(Churn ~ ., data = training_set, 
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(100, 300, 5))) #This is for k

ggplot(train_knn, highlight = TRUE) +
  ggtitle("Default KNN Bootstrap") + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(train_knn$bestTune[1]),
           y = as.numeric(max(train_knn$results['Accuracy']))*1.0003,
           label = paste("Max Accuracy",round(as.numeric(max(train_knn$results['Accuracy'])),4))) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))


train_knn$bestTune
max(train_knn$results['Accuracy'])
train_knn$finalModel
e4 <- as.numeric(max(train_knn$results['Accuracy']))
e4

#F.5. KNN --> Cross Validation
control <- trainControl(method = "cv", number = 10, p = .9)
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_knn_2 <- train(Churn ~ ., data = training_set, 
                     method = "knn", 
                     tuneGrid = data.frame(k = seq(100, 300, 5)),
                     trControl = control) #This is for k

ggplot(train_knn_2, highlight = TRUE) + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(train_knn_2$bestTune[1]),
           y = as.numeric(max(train_knn_2$results['Accuracy']))*1.0003,
           label = paste("Max Accuracy",round(as.numeric(max(train_knn_2$results['Accuracy'])),4))) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))
train_knn_2$bestTune
max(train_knn_2$results['Accuracy'])
train_knn_2$finalModel
e5 <- as.numeric(max(train_knn_2$results['Accuracy']))
e5

#F.6. Classification tree model (CTM)
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_rpart <- train(Churn ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = training_set)
ggplot(train_rpart) + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(train_rpart$results$cp[which.max(train_rpart$results$Accuracy)]),
           y = as.numeric(train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)])*1.005,
           label = paste("Max Acc",
                         paste(round(train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)],3),
                               "CP =", as.numeric(train_rpart$results$cp[which.max(train_rpart$results$Accuracy)]))), size = 3) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))
train_rpart$finalModel$cptable
plot(train_rpart$finalModel, margin = .01)
text(train_rpart$finalModel, 1 )
#QUÉ NÚMERO DE ITERACIÓN - ÍNDICE FUE EL MAYOR
which.max(train_rpart$results$Accuracy)
#PARA QUÉ VALOR DE CP
train_rpart$results$cp[which.max(train_rpart$results$Accuracy)]
#QUÉ VALOR DE ACCURACY DIO
train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)]
e6 <- as.numeric(train_rpart$results$Accuracy[which.max(train_rpart$results$Accuracy)])
e6

#F.7. Random Forest
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_rf <- train(Churn ~ ., method = "rf",
                  tuneGrid = data.frame(mtry = seq(1:5)),
                  data = training_set,
                  ntree = 200)

ggplot(train_rf)  + theme_economist() +
  annotate(geom = "text",
           x = as.numeric(which.max(train_rf$results$Accuracy)[1]),
           y = as.numeric(train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)])*1.0007,
           label = paste("Max Accuracy",round(train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)],4))) +
  theme(legend.position='top',
        title  = 
          element_text(size = 10,margin = margin(b = .5, unit = "cm")),
        legend.margin = element_text(margin = margin(t = -1, unit = "cm")),
        axis.text.x = 
          element_text(size = 10, margin = margin(t = .25, unit = "cm")),
        axis.title.x =  
          element_text(size = 12, margin = margin(t = .5, unit = "cm")),
        axis.title.y =  
          element_text(size = 12, margin = margin(r = .25, unit = "cm")))

varImp(train_rf)
train_rf$results$Accuracy

which.max(train_rf$results$Accuracy)
train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)]
e7 <- as.numeric(train_rf$results$Accuracy[which.max(train_rf$results$Accuracy)])
e7

#F.8. Naive Bayes
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_nb <- train(Churn ~ ., method = "naive_bayes",
                  data = training_set)
which.max(train_nb$results$Accuracy)
train_nb$results$Accuracy[which.max(train_nb$results$Accuracy)]
e8 <- as.numeric(train_nb$results$Accuracy[which.max(train_nb$results$Accuracy)])
e8

#F.9. svmLinear
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_smv <- train(Churn ~ ., method = "svmLinear",
                   data = training_set)
train_smv$results['Accuracy']
e9 <- as.numeric(train_smv$results['Accuracy'])
e9

#F.10. gamLoess
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_gamLoess <- train(Churn ~ DO + WorkingHours + AVGTicket + Cost, method = "gamLoess",
                        data = training_set)
train_gamLoess$results['Accuracy']
e10 <- as.numeric(train_gamLoess$results['Accuracy'])
e10

#F.11. multinom
set.seed(1)
set.seed(1, sample.kind="Rounding")
train_multinom <- train(Churn ~ ., method = "multinom",
                        data = training_set, trace = FALSE)
which.max(train_multinom$results$Accuracy)
train_multinom$results$Accuracy[which.max(train_multinom$results$Accuracy)]
e11 <- as.numeric(train_multinom$results$Accuracy[which.max(train_multinom$results$Accuracy)])
e11

#G. Build a table with models and results
model_name <- c("lda","qda","glm", "knn_bootstrap", "knn_crosvalidation",
                "ctm","rf","naive_bayes", "svmLinear",
                "gamLoess", "multinom")

models <- c("train_lda","train_qda","train_glm","train_knn","train_knn_2",
            "train_rpart","train_rf","train_nb","train_smv",
            "train_gamLoess","train_multinom")

results <- c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11)
results
results <- as.data.frame(results)
row.names(results) <- NULL
results

#H. Pick models with average results above the mean
selected_models <- cbind(model_name,models,results)
selected_models <- as.data.frame(selected_models)
row.names(selected_models) <- NULL
selected_models
mean_accuracy <- mean(selected_models$results)
mean_accuracy
#As accuracy of qda is so low, and represents an outlier, we drop it to calculate the mean and the selected models:
selected_models <- selected_models %>% filter(results > .75)
selected_models
mean_accuracy <- mean(selected_models$results)
mean_accuracy
selected_models <- selected_models %>% mutate(selected = 
                                                ifelse(results > mean(results),"YES","NO"))

selected_models

final_models <- selected_models %>% filter(selected == "YES") %>%
  select(-selected) %>% rename(model = model_name, train_name = models)

final_models

#I. Make predictions
#We evaluate the results in the test set:
#I.a. GLM:
prediction_glm <- predict(train_glm, newdata = test_set)
knn_glm_acc_test <- as.numeric(confusionMatrix(data = prediction_glm, reference = test_set$Churn)$overall["Accuracy"][1])
knn_glm_acc_test
prediction_glm <- as.data.frame(prediction_glm)
head(prediction_glm)


#I.b. CTM:
prediction_ctm <- predict(train_rpart, newdata = test_set)
knn_ctm_acc_test <- as.numeric(mean(prediction_ctm == test_set$Churn))
knn_ctm_acc_test
prediction_ctm <- as.data.frame(prediction_ctm)
head(prediction_ctm)


#I.c. RF:
prediction_rf <- predict(train_rf, newdata = test_set)
knn_rf_acc_test <- as.numeric(confusionMatrix(data = prediction_rf, reference = test_set$Churn)$overall["Accuracy"][1])
knn_rf_acc_test
prediction_rf <- as.data.frame(prediction_rf)
head(prediction_rf)


#I.d. Multinom:
prediction_multinom <- predict(train_multinom, newdata = test_set)
knn_multinom_acc_test <- as.numeric(confusionMatrix(data = prediction_multinom, reference = test_set$Churn)$overall["Accuracy"][1])
knn_multinom_acc_test
prediction_multinom <- as.data.frame(prediction_multinom)
head(prediction_multinom)


Accuracy_Test_Set <- c(prediction_glm,prediction_ctm,
                       prediction_rf,prediction_multinom)

Accuracy_Test_Set

final_table <- cbind(final_models,Accuracy_Test_Set) 
final_table <- as.data.frame(final_table)
final_table <- final_table %>% rename(Accuracy_Train_Set = results)
final_table

#J. Combine models
ensemble_predictions <- cbind(prediction_glm,prediction_ctm,
                              prediction_rf,prediction_multinom)

head(ensemble_predictions,20)

ensemble_predictions <- ensemble_predictions %>% mutate(
  prediction_glm = ifelse(prediction_glm == "NO",0,1),
  prediction_ctm = ifelse(prediction_ctm == "NO",0,1),
  prediction_rf = ifelse(prediction_rf == "NO",0,1),
  prediction_multinom = ifelse(prediction_multinom == "NO",0,1),
)

head(ensemble_predictions,20)

ensemble_predictions <- ensemble_predictions %>% mutate(
  Final_Decision = ifelse(prediction_glm+prediction_ctm+
                            prediction_rf+prediction_multinom >= 3,"YES","NO"))


head(ensemble_predictions,20)

mean(ensemble_predictions$Final_Decision == test_set$Churn)

confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)

TClass <- factor(c("NO","YES","YES","NO"))
PClass <- factor(c("NO","NO","YES","YES"))
Y      <- c(confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[1],
            confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[3],
            confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[4],
            confusionMatrix(as.factor(ensemble_predictions$Final_Decision), reference = test_set$Churn)$table[2])
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) + 
  ggtitle("Confusion Matrix for Balanced Dataset") + 
  xlab("Actual Class - Churn")  +
  ylab("Predicted Class - Churn")  +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")

#********************* Loading Required Libraries *****************************
library(data.table)
library(readxl)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(dplyr)
library(DataExplorer)
library(PerformanceAnalytics)
library(GGally)
library(psych)
library(reshape2)
library(tidyverse)
library(ggcorrplot)
library(caret)
library(randomForest)

#------------------------------------------------------------------------------#
#******************** Load csv file and checking structure of data *************
Incidents_service <- read_excel(file.choose()) # Import the original Client data
View(Incidents_service)
dim(Incidents_service)  #141712     25
summary(Incidents_service)
str(Incidents_service)
# char varaible : 16
# int varaible : 3
# logical : 3
# POSIXct : 3

#------------------------------------------------------------------------------#
#********************************* EDA ****************************************
#Checking the unique values in each columns
attach(Incidents_service)
length(unique(Incidents_service$ID)) #24918
rapply(Incidents_service, function(x) length(unique(x)))
#------------------------------------------------------------------------------#

#Graphical Representation of incident state with count
ggplot(data = Incidents_service, aes(x = ID_status, 
                                     y = stat(count), 
                                     fill = ID_status, 
                                     label = stat(count))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  labs(x = 'ID Status', y = 'Count', fill = 'ID_status') +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1,size=rel(1.1)),
        panel.border=element_rect(fill=NA))
#------------------------------------------------------------------------------#

# Unique values in each variable in ordered way
uni <- sort(lengths(lapply( Incidents_service,unique)),decreasing = T)

# 01_Barplot of Unique Values in each column
b1=barplot(uni, col = rainbow(7), las=2, ylim = c(0,55000),cex.axis = 0.60,cex.names = 0.60,
           ylab = 'Count', main = 'Unique Value Frequency')
text(x=b1, y= uni, labels = uni,pos = 3,cex = 0.6,col = 'black')

#------------------------------------------------------------------------------#

#Graphical Representation of Impact with %
Incidents_service %>% 
  count(impact = factor(impact)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = impact, y = Percentage, fill = impact, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)
#------------------------------------------------------------------------------#

#Graphical Representation of Type Contact with %
Incidents_service %>% 
  count(type_contact = factor(type_contact)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = type_contact, y = Percentage, fill = type_contact, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)
#------------------------------------------------------------------------------#

#Graphical Representation of Active with %
Incidents_service %>% 
  count(active = factor(active)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = active, y = Percentage, fill = active, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)
#------------------------------------------------------------------------------#

#Graphical Representation of Notify with %
Incidents_service %>% 
  count(notify = factor(notify)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = notify, y = Percentage, fill = notify, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)
#------------------------------------------------------------------------------#

#Graphical Representation of Doc_Knowledge with %
Incidents_service %>% 
  count(Doc_knowledge = factor(Doc_knowledge)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = Doc_knowledge, y = Percentage, fill = Doc_knowledge, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)

#------------------------------------------------------------------------------#
Incidents_service %>% 
  count(confirmation_check = factor(confirmation_check)) %>% 
  mutate(Percentage = prop.table(n)) %>% 
  ggplot(aes(x = confirmation_check, y = Percentage, fill = confirmation_check, label = scales::percent(Percentage))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)

#------------------------------------------------------------------------------#
#Checking proportion 
plot_bar(Incidents_service)

#------------------------------------------------------------------------------#
#Changing ? in to NA
Incidents_service$problem_id<-gsub("?", NA,Incidents_service$problem_id,fixed = TRUE)
Incidents_service$`change request`<-gsub("?",NA,Incidents_service$`change request`,fixed = TRUE)

#------------------------------------------------------------------------------#

#Checking NA Values
sum(is.na(Incidents_service)) #280138
sapply(Incidents_service,function(x) sum(is.na(x)))
#------------------------------------------------------------------------------#

#Graphical Representation of Missing Values
plot_missing(Incidents_service)
#Problem ID 98.38%
#Change Request 99.3%

#------------------------------------------------------------------------------#
#**************************** Data Cleaning ************************************
#Dropping column 9,11,13,24,25
Incidents_service<-Incidents_service[,-c(9,11,13,24,25)]

#Checking missing values
sum(is.na(Incidents_service)) #0
#------------------------------------------------------------------------------#
#Converting few variables to factor
Incidents_service$ID_status <- as.factor(Incidents_service$ID_status)
Incidents_service$active <- as.factor(Incidents_service$active)
Incidents_service$type_contact <- as.factor(Incidents_service$type_contact)
Incidents_service$impact <- as.factor(Incidents_service$impact)
Incidents_service$Doc_knowledge <- as.factor(Incidents_service$Doc_knowledge)
Incidents_service$confirmation_check <- as.factor(Incidents_service$confirmation_check)
Incidents_service$notify <- as.factor(Incidents_service$notify)

str(Incidents_service)

table(Incidents_service$impact)
# 1 - High  2 - Medium    3 - Low 
#   3491       134335       3886 

#------------------------------------------------------------------------------#
# Use of Substring function to extract required string

# Variable "ID" :
Incidents_service$ID <- substr(Incidents_service$ID, start=4, stop=12)
Incidents_service$ID <- as.numeric(Incidents_service$ID)

# Variable "ID_ caller" :
Incidents_service$ID_caller <- substr(Incidents_service$ID_caller, start=7, stop=14)
Incidents_service$ID_caller <- as.numeric(Incidents_service$ID_caller)

# Variable "opened_by" :
Incidents_service$opened_by <- substr(Incidents_service$opened_by, start=10, stop=16)
Incidents_service$opened_by <- as.numeric(Incidents_service$opened_by)

# Variable "Created_by" :
Incidents_service$Created_by <- substr(Incidents_service$Created_by, start=11, stop=16)
Incidents_service$Created_by <- as.numeric(Incidents_service$Created_by)

# Variable "updated_by" :
Incidents_service$updated_by <- substr(Incidents_service$updated_by, start=11, stop=16)
Incidents_service$updated_by <- as.numeric(Incidents_service$updated_by)

# Variable "location" :
Incidents_service$location <- substr(Incidents_service$location, start=9, stop=16)
Incidents_service$location <- as.numeric(Incidents_service$location)

# Variable "category_ID" :
Incidents_service$category_ID <- substr(Incidents_service$category_ID, start=9, stop=16)
Incidents_service$category_ID <- as.numeric(Incidents_service$category_ID)

# Variable "user_symptom" :
Incidents_service$user_symptom <- substr(Incidents_service$user_symptom, start=8, stop=16)
Incidents_service$user_symptom <- as.numeric(Incidents_service$user_symptom)

# Variable "Support_group" :
Incidents_service$Support_group <- substr(Incidents_service$Support_group, start=6, stop=16)
Incidents_service$Support_group <- as.numeric(Incidents_service$Support_group)

# Variable "support_incharge" :
Incidents_service$support_incharge <- substr(Incidents_service$support_incharge, start=9, stop=16)
Incidents_service$support_incharge <- as.numeric(Incidents_service$support_incharge)

#-----------------------------------------------------------------
####################### Corelation ####################
library(GoodmanKruskal) # not applicable to numeric
gkmatrix <- GKtauDataframe(Incidents_service)
plot(gkmatrix,diagSize = 1)

####################### Model Building ####################
# Data Partitioning
set.seed(123)
ind <- sample(2,nrow(Incidents_service), replace = TRUE, prob = c(0.7,0.3))
train <- Incidents_service[ind==1,]
test <- Incidents_service[ind==2,]

# Random Forest Model
memory.size()
memory.limit(400000)

set.seed(999)
rf <- randomForest(impact~.,data = train, importance=TRUE)
print(rf) # mtree=500, ntry=4, OOB error rate=1.71%

# Prediction & Confusion Matrix - Train Data
p1 <- predict(rf, train)
confusionMatrix(p1, train$impact) # Accuracy=99.92%

# Prediction & Confusion Matrix - Test Data
p2 <- predict(rf, test)
confusionMatrix(p2, test$impact) 
# Accuracy=98.23%
#                 Class: 1 - High Class: 2 - Medium Class: 3 - Low
# Sensitivity            0.60973            0.9981        0.77365

# Variable Importance Graph
varImpPlot(rf, sort=T, n.var = 10, main = 'Top 10 Variable Importance using 20 Var w/o balancing')

#----------------------------------------------------------------
# Error Rate of Random Forest
plot(rf) # 300

# Tune mtry
train <- as.data.frame(train)
t <- tuneRF(train[,-15], train[,15], stepFactor = 0.5, plot = TRUE, 
            ntreeTry = 300, trace = TRUE, improve = 0.05)
t # 16

# Fine Tune RF Model
set.seed(333)
rf1 <- randomForest(impact~.,data = train, ntree=300, mtry= 16,
                    importance= TRUE)
print(rf1) # ntree=300, mtry=16, OOB error rate=0.97%

# Prediction & Confusion Matrix - Train Data
p3 <- predict(rf1, train)
confusionMatrix(p3, train$impact) # Accuracy = 100%

# Prediction & Confusion Matrix - Test Data
p4 <- predict(rf1, test)
confusionMatrix(p4, test$impact)
# Accuracy = 99.04 %
#                   Class: 1 - High   Class: 2 - Medium   Class: 3 - Low
# Sensitivity            0.78149            0.9980        0.91807

# Variable Importance Graph
varImpPlot(rf1, sort=T, n.var = 10, main = 'Top 10 Variable Importance using FT model on 20 Var w/o balancing')
# ---------------------------------------------------------------
# Model Building based on Top 5 Variables
train_top5 <- train[,c('opened_by','ID','ID_caller','location','user_symptom','impact')]
test_top5 <- test[,c('opened_by','ID','ID_caller','location','user_symptom','impact')]

table(train_top5$impact)
# 1 - High 2 - Medium    3 - Low 
#     2443      94008       2702 

# Balance the target variable by using upsampling method
set.seed(123)
train_top5_up <- upSample(x=train_top5[,],y=train_top5$impact)
train_top5_up <- train_top5_up[,-7]

table(train_top5_up$impact)
# 1 - High 2 - Medium    3 - Low 
#   94008      94008      94008 
#----------------------------------------------------------------------
##### Random Forest Model (Train without Upsample Data)
memory.size()
memory.limit(500000)

set.seed(999)
rf2 <- randomForest(impact~.,data = train_top5)
print(rf2) # ntee=500, mtry=2, OOB error rate=0.93%

# Prediction & Confusion Matrix - Train Data
p11 <- predict(rf2, train_top5)
confusionMatrix(p11, train_top5$impact) # Accuracy=99.40%

# Prediction & Confusion Matrix - Test Data (without upsampling data)
p12 <- predict(rf2, test_top5)
confusionMatrix(p12, test_top5$impact) 
# Accuracy = 99.18%
#                         Class: 1 - High Class: 2 - Medium Class: 3 - Low
# Sensitivity                  0.87405            0.9960        0.95017

#-----------------------------------------------------------------
##### Random Forest Model (Train Upsampling Data)
memory.limit(500000)
set.seed(999)
rf_up <- randomForest(impact~.,data = train_top5_up)
print(rf_up) # ntee=500, mtry=2, OOB error rate=0.94%

# Prediction & Confusion Matrix - Train Data 
p1_up <- predict(rf_up, train_top5_up)
confusionMatrix(p1_up, train_top5_up$impact) # Accuracy=99.07%

# Prediction & Confusion Matrix - Test Data (without upsampling data)
p2_up <- predict(rf_up, test_top5)
confusionMatrix(p2_up, test_top5$impact) 
# Accuracy = 99.18%
#                         Class: 1 - High Class: 2 - Medium Class: 3 - Low
# Sensitivity                  0.97996            0.9926        0.97635

# -------------------------------------------------------
# Error Rate of Random Forest
plot(rf_up) # 200

# Tune mtry
train_top5_up <- as.data.frame(train_top5_up)
t1 <- tuneRF(train_top5_up[,-6], train_top5_up[,6], stepFactor = 0.5, plot = TRUE, 
             ntreeTry = 200, trace = TRUE, improve = 0.05)
t1 # 4

# Fine Tune RF Model
set.seed(555)
rf_up_1 <- randomForest(impact~.,data = train_top5_up, ntree=200, mtry= 4,
                        importance= TRUE)
print(rf_up_1) # ntree=200, mtry=4, OOB error rate=0.92%

# Prediction & Confusion Matrix - Train Data
p21 <- predict(rf_up_1, train_top5_up)
confusionMatrix(p21, train_top5_up$impact) # Accuracy = 99.10%

# Prediction & Confusion Matrix - Test Data
p22 <- predict(rf_up_1, test_top5)
confusionMatrix(p22, test_top5$impact)
# Accuracy = 99.26 %
#               Class: 1 - High Class: 2 - Medium Class: 3 - Low
# Sensitivity        0.98378            0.9933        0.97804

# -------------------------------------------------------------------
save(rf_up_1,file = "rf_up_1.R")
write.csv(train_top5_up,file ="train_top5_up.csv",row.names = FALSE)
write.csv(test_top5,file ="test_top5.csv",row.names = FALSE)

################################ END ##############################################

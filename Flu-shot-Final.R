###################################
#Capstone Project
#Flu shot learning 
#SusanSunny
##################################


knitr::opts_chunk$set(echo = TRUE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

##Import packages
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(dplyr)
library(igraph)
library(e1071)
library(pROC)
library(glmnet)
library(ggpubr)

#########################
##Import data from github
#########################

#Import training features 
url<- "https://raw.githubusercontent.com/SusanSunny/Movielens/main/Flu_Shot_Learning_Predict_H1N1_and_Seasonal_Flu_Vaccines_-_Training_Features.csv"
destination_file<- "train_features.csv"
download.file(url, destination_file)
flu_features<- read_csv("train_features.csv")
file.remove("train_features.csv")

#Import training labels 
url2<- "https://raw.githubusercontent.com/SusanSunny/Movielens/main/Flu_Shot_Learning_Predict_H1N1_and_Seasonal_Flu_Vaccines_-_Training_Labels.csv"
destination_file<- "train_labels.csv"
download.file(url2, destination_file)

flu_labels<- read_csv("train_labels.csv")
flu_labels
file.remove("train_labels.csv")

#have a look at data frames
str(as.data.frame(flu_features), give.attr=FALSE)
str(as.data.frame(flu_labels), give.attr=FALSE)


####################
##Removal of NAs
####################

#how many NAs in each column 
number_of_NAs<-sapply(flu_features, function(x) sum(is.na(x)))
as.data.frame(number_of_NAs)

#how many rows with no missing data
row_na<- vector(mode="logical", length= nrow(flu_features))
for (i in 1:nrow(flu_features)){
  row_na[i]<-all(!is.na(flu_features[i,]))
}
#6437 rows with no missing data 
sum(row_na)


#change NAs in numeric columns to mean of column 
change_na<- function(x){
  x[is.na(x)]<-mean(x,na.rm=TRUE)
  x
}
numeric_cols<- c(1:22, 33:34) #columns with numerics
#apply function to the columns with numerics
flu_features[numeric_cols]<-lapply(flu_features[numeric_cols], change_na)#apply function to the columns with numerics

#change NAs in string columns to empty string 
change_na_char<- function(x){
  x[is.na(x)]<- ""
  x
}
string_cols<- c(23:32, 35:36) #columns which contain strings 
flu_features[string_cols]<-lapply(flu_features[string_cols], change_na_char) #apply function to the columns that contain strings


#create tibble with features and labels for data exploration 
flu_all<- left_join(flu_features, flu_labels)
sum(is.na(flu_all)) #-> all NAs are removed 


############################
##Plots for data exploration
############################

####Age group####
#proportion of h1n1 vaccinations each age group
h1<-flu_all%>%group_by(age_group)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(age_group, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

####Education####
#proportion of h1n1 vaccinations depending on education
h2<-flu_all%>%group_by(education)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(education, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

####Race####
#proportion of h1n1 vaccinations depending on race
h3<-flu_all%>%group_by(race)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(race, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

####Sex####
#proportion of h1n1 vaccinations depending on race
h4<-flu_all%>%group_by(sex)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(sex, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

####Income poverty####
#proportion of h1n1 vaccinations depending on income poverty
h5<-flu_all%>%group_by(income_poverty)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(income_poverty, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, size = 7))

####Census_msa####
#proportion of h1n1 vaccinations depending on income poverty
h6<-flu_all%>%group_by(census_msa)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(census_msa, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, size = 7))

####Rent_or_own####
#proportion of h1n1 vaccinations depending on income poverty
h7<-flu_all%>%group_by(rent_or_own)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(rent_or_own, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

####Marital_status####
#proportion of h1n1 vaccinations depending on income poverty
h8<-flu_all%>%group_by(marital_status)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(marital_status, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

####Employment status####
#proportion of h1n1 vaccinations depending on income poverty
h9<-flu_all%>%group_by(employment_status)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(employment_status, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, size=5))

####Employment industry####
#proportion of h1n1 vaccinations depending on income poverty
h10<-flu_all%>%group_by(employment_industry)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(employment_industry, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, size= 5))

####Employment occupation####
#proportion of h1n1 vaccinations depending on income poverty
h11<-flu_all%>%group_by(employment_occupation)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(employment_occupation, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, size= 5))

####region####
#proportion of h1n1 vaccinations depending on income poverty
h12<-flu_all%>%group_by(hhs_geo_region)%>%
  summarize(prop_h1n1 = sum(h1n1_vaccine)/n())%>%
  ggplot(aes(hhs_geo_region, prop_h1n1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, size= 5))

ggarrange(h1, h2, h3, h4, h5, h6, ncol=3, nrow=2)
ggarrange(h7, h8, h9, h10, h11, h12, ncol=3, nrow=2)

###################################
#add levels to columns with strings 
###################################

#in order to use xgboost and convert the character strings into factors later
#the factors are ordered either in a natural way or, if not possible, in the order of the proportions of vaccinations beginning with the lowest
#ordered by hand getting the order from the ggplot graphs 
flu_features$age_group<-as.factor(ordered(flu_features$age_group, levels=c("18 - 34 Years", "35 - 44 Years","45 - 54 Years", "55 - 64 Years", "65+ Years")))
flu_features$education<-as.factor(ordered(flu_features$education, levels=c("", "< 12 Years","12 Years", "Some College","College Graduate" )))
flu_features$race<-as.factor(ordered(flu_features$race, levels=c("Black", "Hispanic", "Other or Multiple", "White")))
flu_features$sex<-as.factor(ordered(flu_features$sex, levels=c("Male", "Female")))
flu_features$income_poverty<-as.factor(ordered(flu_features$income_poverty, levels=c("", "Below Poverty", "<= $75,000, Above Poverty", "> $75,000")))
flu_features$marital_status<-as.factor(ordered(flu_features$marital_status, levels=c("", "Not Married", "Married")))
flu_features$rent_or_own<-as.factor(ordered(flu_features$rent_or_own, levels=c("Rent", "", "Own")))
flu_features$employment_status<-as.factor(ordered(flu_features$employment_status, levels=c("Unemployed", "", "Employed", "Not in Labor Force")))
flu_features$census_msa<-as.factor(ordered(flu_features$census_msa, levels=c("MSA, Principle City", "Non-MSA", "MSA, Not Principle  City")))

#order hss_geo_region, employment_occupation and change values to Letters 
#hhs_geo_region
levels_geo<-flu_all%>%group_by(hhs_geo_region)%>%
  summarize(proportion=sum(h1n1_vaccine==1)/n())%>%
  arrange(proportion)%>%.$hhs_geo_region
flu_features$hhs_geo_region<- as.factor(ordered(flu_features$hhs_geo_region, levels= levels_geo, labels = LETTERS[1:length(levels_geo)]))

#employment_occupation
levels_occupation<-flu_all%>%group_by(employment_occupation)%>%
  summarize(proportion=sum(h1n1_vaccine==1)/n())%>%
  arrange(proportion)%>%.$employment_occupation
flu_features$employment_occupation<- as.factor(ordered(flu_features$employment_occupation, levels= levels_occupation, labels = LETTERS[1:length(levels_occupation)]))

#Employment_industry
levels_industry<-flu_all%>%group_by(employment_industry)%>%
  summarize(proportion=sum(h1n1_vaccine==1)/n())%>%
  arrange(proportion)%>%.$employment_industry
flu_features$employment_industry<- as.factor(ordered(flu_features$employment_industry, levels= levels_industry, labels = LETTERS[1:length(levels_industry)]))


#########
##xgboost
#########

#factors as integers
flu_features_int1<- flu_features
flu_features_int1[string_cols]<-lapply(flu_features[string_cols], function(x) as.integer(x))
#remove respondent_id
flu_features_int<- flu_features_int1[,2:36]

#construct matrix for model
data=xgb.DMatrix(data=as.matrix(flu_features_int), label = flu_labels$h1n1_vaccine)
#train model 
set.seed(1) 
xgb<-xgb.train(data=data, objective= "binary:logistic", eval_metric="logloss", 
               nrounds=500, eta = 0.02, gamma = 4, subsample = 0.9, 
               colsample_bytree = 0.5, max_depth=7)

#model complexity: number of leafs and weighted cover against leaf depth 
xgb.ggplot.deepness(xgb)

#plot max depthness
xgb.plot.deepness(xgb, "max.depth")
#the majority of trees have leaf depth 8, very few have leaf depth 7

#plot med depth 
xgb.plot.deepness(xgb, "med.depth") #median leaf depths of most trees is 7

#show importance of variables 
feature_names<- colnames(flu_features_int)
imp<-xgb.importance(feature_names=feature_names, model=xgb)
imp
##-> most important is doctor recommendation, opinion h1n1 risk and health insurance 
xgb.plot.importance(imp, cex = 0.4)


#Calculate AUROC
set.seed(1)
xbgcv<- xgb.cv(data=data, verbose=TRUE, print_every_n=100, nfold=5, 
               objective="binary:logistic", eval_metric="auc", nrounds=500, 
               eta=0.02, gamma=4, max_depth=7, subsample=0.9, colsample_bytree=0.5)


xbgcv$evaluation_log[500]
auc_xgb<-xbgcv$evaluation_log[500]$test_auc_mean
paste0("out-of-sample AUC:", auc_xgb)

##########################
##Linear Model with glmnet
##without one-hot-encoding
##########################

#Create test set 
set.seed(1) #set seed to always get the same result 
test_index <- createDataPartition(y = flu_labels$h1n1_vaccine, times = 1, p = 0.1, list = FALSE)
test_index<- as.vector(test_index)
x_train <- flu_features_int[-test_index,]
x_test <- flu_features_int[test_index,]
y_train <- flu_labels[-test_index,]
y_test <- flu_labels[test_index,]

##without one-hot-encoding (assume linearity because I ordered the variables by the mean response)
#create matrix for cv.glmnet 
glm_matrix1<-as.matrix(x_train)

#model with 10-fold cross-validation
set.seed(1)
alpha<- 0.9 
cvg1<-cv.glmnet(glm_matrix1, y_train$h1n1_vaccine, family = "binomial", alpha=alpha)


#plot log lambda against logloss (prediction error) in order to choose lambda 
plot(cvg1) 
#the larger the lambdas, the more variables are included in the model 

paste0("lambda with optimal prediction error: ",log(cvg1$lambda.min))
#from a lambda a little under exp(-7.07) the prediction error doesn't get smaller
paste0("largest value of lambda such that error is within 1 standard error of the minimum: ",log(cvg1$lambda.1se))
#from a lambda a little over exp(-5.19) the confidence interval of the prediction error is includes the best prediction error 

#build model 
glm_model1<-glmnet(glm_matrix1, y_train$h1n1_vaccine, family="binomial", alpha = 0.9)

#plot log lambda against 
plot(glm_model1, xvar= "lambda") 
#shows how much influence each variable has for each lambda 
#one variable has a lot higher influence than the others 

#make prediction
test_matrix1<- as.matrix(x_test)
pred1<-predict(glm_model1, test_matrix1, type="response", s=cvg1$lambda.1se)
#calculate AUC
auc_glm1<-auc(y_test$h1n1_vaccine, as.vector(pred1))
auc_glm1

##########################
##Linear Model with glmnet
##with one-hot-encoding
##########################

#create training and test set 
x_train <- flu_features[-test_index,-1]
x_test <- flu_features[test_index,-1]

##Linear Model with glmnet
##with one-hot-encoding

#one-hot-encoding with makeX
glm_matrix2<- makeX(x_train)

#model with 10-fold cross-validation
set.seed(1)
cvg2<-cv.glmnet(glm_matrix2, y_train$h1n1_vaccine, family = "binomial", alpha=0.9) 
#plot log lambda against logloss (prediction error) in order to choose lambda 
plot(cvg2) 
#the larger the lambdas, the more variables are included in the model 
log(cvg2$lambda.min)
log(cvg2$lambda.1se)

#build model 
glm_model2<-glmnet(glm_matrix2, y_train$h1n1_vaccine, family="binomial", alpha = 0.9)

#plot log lambda against 
plot(glm_model2, xvar= "lambda") 

#make prediction
test_matrix2<- makeX(x_test)
pred2<-predict(glm_model2, test_matrix2, type="response", s=cvg2$lambda.1se)
#calculate AUC
auc_glm2<-auc(y_test$h1n1_vaccine, as.vector(pred2))
auc_glm2

###############
##Print results
###############

results<- data.frame(Model= c("xgboost", "glmnet", "glmnet with one-hot-encoding"),
                     AUC = c(auc_xgb, auc_glm1, auc_glm2))
results

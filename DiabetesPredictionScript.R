
###########################
# Downloading the dataset #
###########################

# Installing required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

# Diabetes dataset source
urlfile="https://raw.githubusercontent.com/jorditoneu/Diabetes_Project/master/diabetes.csv"

# Reading the Pima Indians Diabetes csv file
diabetes<-read_csv(url(urlfile))

#################################
# Data exploration and cleaning #
#################################

# Structure of the dataset
glimpse (diabetes)

# Printout of the table with the first 10 rows of the dataset
knitr::kable (diabetes[1:10,], table.attr = "style='width:70%;'")

# Creating the vector with variables name
Variable <- names(diabetes)

# Writting the vector with variables description
Description <- c("Number of times pregnant","Plasma glucose concentration a 2 hours in an oral glucose tolerance test","Diastolic blood pressure (mm Hg)","Triceps skin fold thickness (mm)","2-Hour serum insulin (mu U/ml)","Body mass index (weight in kg/(height in m)^2)","Diabetes pedigree function", "Age (years)","class variable. 0 indicates non-diabetes, 1 indicates diabetes")

# Creating the table with variables and descriptions
VarTable<- data_frame(Variable,Description)

# Printout of the table
VarTable %>% 
  knitr::kable()   

# Summary of diabetes dataset
summary(diabetes)

# Checking the number of NA's in the dataset
sum(is.na(diabetes))

# As categorical outcome, we convert the variable type to factor
diabetes$Outcome <- as.factor(diabetes$Outcome)

# Pregnancies histogram
H1 <- diabetes %>%
  ggplot(aes(Pregnancies))+
  geom_histogram(binwidth = 1,fill="darkblue", color="grey") +
  labs (x="Number of pregnancies")

# Glucose histogram
H2 <- diabetes %>%
  ggplot(aes(Glucose))+ 
  geom_histogram(binwidth = 11,fill="darkblue", color="grey")+
  labs (x="Glucose")

# Blood Pressure histogram
H3 <- diabetes %>%
  ggplot(aes(BloodPressure))+ 
  geom_histogram(binwidth = 7,fill="darkblue", color="grey")+
  labs (x="Blood pressure in mm Hg")

# Skin thickness histogram
H4 <- diabetes %>% 
  ggplot(aes(SkinThickness))+
  geom_histogram(binwidth = 5,fill="darkblue", color="grey")+
  labs (x="Skin thickness in mm")

# Insulin histogram
H5 <- diabetes %>%
  ggplot(aes(Insulin))+ 
  geom_histogram(binwidth = 45,fill="darkblue", color="grey")+
  labs (x="2-Hour serum insulin (mu U/ml)")

# BMI histogram
H6 <- diabetes %>%
  ggplot(aes(BMI))+
  geom_histogram(binwidth = 4,fill="darkblue", color="grey")+
  labs (x="BMI - Body Mass Index")

# Diabetes pedigree function histogram
H7 <- diabetes %>% 
  ggplot(aes(DiabetesPedigreeFunction))+
  geom_histogram(binwidth = 0.13,fill="darkblue", color="grey")+
  labs (x="Diabetes pedigree function")

# Age histogram
H8 <- diabetes %>% 
  ggplot(aes(Age))+ 
  geom_histogram(binwidth = 3,fill="darkblue", color="grey")+
  labs (x="Age in years")

# Histograms printout
ggarrange(H1,H2,H3,H4,H5,H6,H7, H8,
          ncol = 2, nrow = 4)

# Function to detect a row with a 0
zeros <- function(x)
{
  return((sum(diabetes[x,1:8]== 0)>0)) 
}

# Computing the proportion of rows with at least one 0
mean(sapply(1:nrow(diabetes), zeros))

# Pregnancies histogram and density plot without 0
# No 0's correction needed
H1 <- diabetes %>%
  ggplot(aes(Pregnancies, y=..density..))+
  geom_histogram(binwidth = 1,fill="darkblue", color="grey")+
  geom_density(col=2)+
  labs (x="Number of pregnancies")

# Glucose histogram and density plot without 0
MedGlucose <- diabetes %>%
  filter (Glucose>0) %>% 
  summarize (median(Glucose))

diabetes$Glucose <-ifelse (round(diabetes$Glucose,0) ==0, as.numeric (round(MedGlucose,0)),diabetes$Glucose)

H2 <- diabetes %>%
  ggplot(aes(Glucose,y=..density..))+
  geom_histogram(binwidth = 11,fill="darkblue", color="grey")  +
  geom_density(col=2)+labs (x="Glucose")

# Blood Pressure histogram and density plot without 0
MedBloodPressure <- diabetes %>% 
  filter (BloodPressure>0) %>% 
  summarize (median(BloodPressure))

diabetes$BloodPressure <-ifelse (diabetes$BloodPressure ==0, as.numeric (round(MedBloodPressure,0)),diabetes$BloodPressure)

H3 <- diabetes %>%
  ggplot(aes(BloodPressure,y=..density..))+
  geom_histogram(binwidth = 7,fill="darkblue", color="grey")  +
  geom_density(col=2)+
  labs (x="Blood pressure in mm Hg")

# Skin thickness histogram and density plot without 0
MedSkin <- diabetes %>%
  filter (SkinThickness>0) %>% 
  summarize (median(SkinThickness))

diabetes$SkinThickness <-ifelse (diabetes$SkinThickness ==0, as.numeric (round(MedSkin,0)),diabetes$SkinThickness )

H4 <- diabetes %>% 
  ggplot(aes(SkinThickness,y=..density..))+
  geom_histogram(binwidth = 5,fill="darkblue", color="grey")  +
  geom_density(col=2)+
  labs (x="Skin thickness in mm")

# Insulin histogram and density plot without 0
MedInsulin <- diabetes %>%
  filter (Insulin>0) %>%
  summarize (median(Insulin))

diabetes$Insulin <-ifelse (diabetes$Insulin ==0, as.numeric (round(MedInsulin,0)),diabetes$Insulin)

H5<-diabetes %>% ggplot(aes(Insulin,y=..density..))+
  geom_histogram(binwidth = 45,fill="darkblue", color="grey")  +
  geom_density(col=2)+
  labs (x="2-Hour serum insulin (mu U/ml)")

# BMI histogram and density plot without 0
MedBMI <- diabetes %>% 
  filter (BMI>0) %>%
  summarize (median(BMI))

diabetes$BMI <-ifelse (diabetes$BMI ==0, as.numeric (round(MedBMI,0)),diabetes$BMI)

H6<-diabetes %>%
  ggplot(aes(BMI,y=..density..))+
  geom_histogram(binwidth = 4,fill="darkblue", color="grey") +
  geom_density(col=2)+
  labs (x="BMI - Body Mass Index")

# Diabetes pedigree function histogram and density plot without 0
# No 0's correction needed
H7 <- diabetes %>%
  ggplot(aes(DiabetesPedigreeFunction,y=..density..))+
  geom_histogram(binwidth = 0.13,fill="darkblue", color="grey")  +
  geom_density(col=2)+
  labs (x="Diabetes pedigree function")

# Age histogram and density plot without 0
# No 0's correction needed
H8 <- diabetes %>% 
  ggplot(aes(Age,y=..density..))+ 
  geom_histogram(binwidth = 3,fill="darkblue", color="grey")+
  geom_density(col=2)+
  labs (x="Age in years")

# Printout of histograms and density plot without 0
ggarrange(H1,H2,H3,H4,H5,H6,H7, H8,
          ncol = 2, nrow = 4)

# Pregnancies density plot
P1 <- diabetes %>% 
  ggplot(aes(Pregnancies, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="Number of pregnancies")

# Pregnancies box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, Pregnancies, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Pregnancies")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Glucose density plot
P1 <- diabetes %>% 
  ggplot(aes(Glucose, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="Glucose")

# Glucose box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, Glucose, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Glucose")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Blood pressure density plot
P1 <- diabetes %>% 
  ggplot(aes(BloodPressure, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="Blood pressure in mm Hg")

# Blood pressure box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, BloodPressure, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Blood pressure")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Skin thickness density plot
P1 <- diabetes %>% 
  ggplot(aes(SkinThickness, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="Skin thickness in mm")

# Skin thickness box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, SkinThickness, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Skin thickness")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Insulin density plot
P1 <- diabetes %>% 
  ggplot(aes(Insulin, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="2-Hour serum insulin (mu U/ml)")

# Insulin box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, Insulin, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Insulin (mu U/ml)")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# BMI density plot
P1 <- diabetes %>% 
  ggplot(aes(BMI, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="BMI - Body Mass Index")

# BMI box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, BMI, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="BMI")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Diabetes Pedigree Function density plot
P1 <- diabetes %>% 
  ggplot(aes(DiabetesPedigreeFunction, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="Diabetes pedigree function")

# Diabetes Pedigree Function box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, DiabetesPedigreeFunction, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Diabetes pedigree")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Age density plot
P1 <- diabetes %>% 
  ggplot(aes(Age, fill = Outcome))+ 
  geom_density(alpha= 0.6) +
  labs (x="Age in years")

# Diabetes Pedigree Function box plot
P2 <- diabetes %>% 
  ggplot(aes(Outcome, Age, fill = Outcome))+ 
  geom_boxplot () +
  labs (y="Age")

# Plots printout
ggarrange(P1, P2,
          ncol = 2, nrow = 1)

# Bar plot to see the Outcome proportion
diabetes %>% 
  ggplot(aes(Outcome, fill=Outcome))+ 
  geom_bar()

# Calculating variables correlation
Correlations <- cor(data.matrix(diabetes))

# Writting reduced descriptions of each variable for a better table printout
Abreviation <- c("Pregnancies","Glucose","Blood P","Skin T","Insulin", "BMI","Pedigree","Age", "Outcome")
#Variable <- c("Pregnancies", "Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age","Outcome")

rownames(Correlations)<- Abreviation
colnames(Correlations)<- Abreviation

# Table printout
kable(round(Correlations,4), col.names = Abreviation)

# Correlation heatmap
heatmap(Correlations, main="Heatmap")

# Converting the Outcome variable to numeric for the corrplot() function
diabetes$Outcome <- as.numeric (diabetes$Outcome)

# Printout of the correlogram
corrplot(cor(diabetes[, 1:9]), type = "lower", method="number")

# Setting the Outcome vriable as a factor again
diabetes$Outcome <- as.factor (diabetes$Outcome)

# Setting the right levels for the Outcome
levels(diabetes$Outcome)<- c(0,1)

# Sorting the variables correlation with Outcome variable
HCorr <-sort(Correlations [9,], decreasing=TRUE)

# Printout of the 4 variables with highest Outcome correlation
HCorr [2:5]

# Computing the average number of pregnancies by Outcome
AvgPregnancies <- diabetes %>%
  group_by(Outcome) %>%
  summarize(AvgPregnancies=mean(Pregnancies))

# Computing the average Age by Outcome
AvgAge <- diabetes %>%
  group_by(Outcome) %>% 
  summarize(AvgAge=mean(Age))

# Plot "Age - Number of Pregnancies" graph relationship
P1 <- diabetes %>% 
  ggplot(aes(Age, Pregnancies))+ 
  geom_point(size = 0.5)+
  geom_smooth(method=loess, formula = y ~ x)+
  geom_vline(data = AvgAge, aes(xintercept = AvgAge,color = Outcome))+
  geom_hline(data = AvgPregnancies, aes(yintercept = AvgPregnancies,color = Outcome))+
  labs (x="Age in years", y="Number of pregnancies")+
  ggtitle("Age - Pregnancies")

# Plot "Age - Number of Pregnancies" stratified by Outcome
P2 <-diabetes %>% 
  ggplot(aes(Age, Pregnancies, color=Outcome))+ 
  geom_point(size = 0.5)+
  labs (x="Age in years", y="Number of pregnancies")+
  ggtitle("Age - Pregnancies stratified")

# Printing out plots
ggarrange(P1,P2,
          ncol = 2, nrow = 1)


# Computing the average BMI value by Output
AvgBMI <- diabetes %>%
  group_by(Outcome) %>% 
  summarize(AvgBMI=mean(BMI))

# Computing the average Skin thickness value by Output                     
AvgSkinT <- diabetes %>% 
  group_by(Outcome) %>% 
  summarize(AvgSkinT=mean(SkinThickness))

# Plot "BMI - Skin Thickness" graph relationship
P1 <- diabetes %>% 
  ggplot(aes(BMI , SkinThickness ))+
  geom_point(size = 0.5)+
  geom_smooth(method=loess, formula = y ~ x)+
  geom_vline(data = AvgBMI, aes(xintercept = AvgBMI,color = Outcome))+
  geom_hline(data = AvgSkinT, aes(yintercept = AvgSkinT,color = Outcome))+
  labs (x="BMI - Body Mass Index", y="Skin thickness in mm")+
  ggtitle("BMI - Skin thickness")

# Plot "BMI - Skin Thickness" stratified by Outcome
P2 <-diabetes %>% 
  ggplot(aes(BMI, SkinThickness, color=Outcome))+ 
  geom_point(size = 0.5)+
  labs (x="BMI - Body Mass Index", y="Skin thickness in mm")+
  ggtitle("BMI - Skin thickness stratified")

# Printing out plots
ggarrange(P1,P2,
          ncol = 2, nrow = 1)

# Computing the average Glucose level by Output
AvgGlucose <- diabetes %>%
  group_by(Outcome) %>% 
  summarize(AvgGlucose=mean(Glucose))

# Plot "Glucose - Output" relationship
P1 <- diabetes %>%
  group_by(Glucose) %>% 
  summarize (avg=mean(Outcome==1)) %>%
  ggplot(aes(Glucose,avg))+geom_point()+
  geom_smooth(method=loess, formula = y ~ x)+
  geom_vline(data = AvgGlucose, aes(xintercept = AvgGlucose,color = Outcome))+
  labs (x="Glucose", y="Diabetic probability")+ 
  ggtitle("Glucose - Outcome")

# Plot "BMI - Skin Thickness" stratified by Outcome
P2 <-diabetes %>% 
  ggplot(aes(Glucose, Outcome, color=Outcome))+ 
  geom_point(size = 0.5)+
  labs (x="Glucose", y="Outcome")+
  ggtitle("BMI - Outcome stratified")

# Printing out plots
ggarrange(P1,P2,
          ncol = 2, nrow = 1)

# Printout of the average Glucose table by Output
AvgGlucose %>% 
  knitr::kable() 

######################
# Data normalization #
######################

# Declaration of function to normalize data
normalize <- function(x) 
{
  return ((x - mean(x)) / sd(x))
}

# Data normalization
diabetes$Pregnancies <-normalize(diabetes$Pregnancies)

diabetes$Glucose <-normalize(diabetes$Glucose)

diabetes$BloodPressure <-normalize(diabetes$BloodPressure)

diabetes$SkinThickness <-normalize(diabetes$SkinThickness)

diabetes$Insulin <-normalize(diabetes$Insulin)

diabetes$BMI <-normalize(diabetes$BMI)

diabetes$DiabetesPedigreeFunction <-normalize(diabetes$DiabetesPedigreeFunction)

diabetes$Age <-normalize(diabetes$Age)

##################
# Data splitting #
##################

# if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1, sample.kind="Rounding")

# The validation set is 10% of the diabetes dataset
test_index <- createDataPartition(y = diabetes$Outcome, times = 1, p = 0.1, list = FALSE)

temp <- diabetes[-test_index,]

validation_set <- diabetes[test_index,]

# if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1, sample.kind="Rounding")

# The train set will be 80% and the test set 20% of the 90% diabetes datatset
test_index <- createDataPartition(y = temp$Outcome, times = 1, p = 0.2, list = FALSE)

train_set <- temp[-test_index,]

test_set <- temp[test_index,]

# Structure of the validation set
str (validation_set)

# Structure of the train set
str (train_set)

# Structure of the test set
str (test_set)

########################
# Modelling approaches #
########################

#############################
# Logistic regression model #
#############################

# Fitting the logistic regression model
fit_glm <- train(Outcome ~ ., method = "glm", 
                 data = train_set)

# Checking the overall accuracy with the logistic regression model
acc_glm <- confusionMatrix(predict(fit_glm, test_set), test_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy 
model_accuracy<- data_frame(Method = "Logistic regression model", Accuracy = acc_glm)

# Creating the results table with the test set
accuracy_results <- data_frame(Method = "Logistic regression model", Accuracy = acc_glm)

# Printout of the model accuracy
model_accuracy %>% 
  knitr::kable()

# Printout of logistic regression model to check the p-value
summary(fit_glm)

#############
# KNN model #
#############

# if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(2, sample.kind="Rounding")

# Fitting the KNN model
fit_knn <- train(Outcome ~ ., method = "knn", tuneGrid = data.frame(k = seq(10, 80, 2)),
                 data = train_set)

# Plotting the model to see the best K value
ggplot(fit_knn, highlight = TRUE)


# Checking the overall accuracy with the KNN model
acc_knn <- confusionMatrix(predict(fit_knn, test_set), test_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "KNN model", Accuracy = acc_knn)

# Adding the model accuracy to the results table
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method="KNN Model",  
                                         Accuracy = acc_knn ))

# Printout of the model accuracy
model_accuracy %>% 
  knitr::kable()

##############
# CART model #
##############

# Fitting the CART model
fit_CART <- train(Outcome ~ ., method = "rpart",tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                  data = train_set)

# Plotting the CART model to see the best CP
ggplot(fit_CART, highlight = TRUE)

# Printout of the best CP value
fit_CART$bestTune

# Plotting the CART tree
plot(fit_CART$finalModel, margin = 0.1)
text(fit_CART$finalModel, cex = 0.75)

# Printout of the variables importance
varImp(fit_CART)

# Checking the overall accuracy with the CART model
acc_CART <- confusionMatrix(predict(fit_CART, test_set), test_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "CART model", Accuracy = acc_CART)

# Adding the model accuracy to the results table
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method="CART Model",  
                                         Accuracy = acc_CART ))

# Printout of the model accuracy
model_accuracy %>% 
  knitr::kable()


#######################
# Random Forest model #
#######################

# if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(2, sample.kind="Rounding")

# Parameters for cross-validation
control <- trainControl(method="cv", number=10, p=0.9)

metric <- "Accuracy"

mtry <- seq(10, 100, 10)

tunegrid <- expand.grid(.mtry=mtry)

# Fitting the Random Forest model
fit_RF <- train(Outcome~., data=train_set, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

# Plotting cross-validation best parameters for accuracy
ggplot(fit_RF, highlight = TRUE)

# Printout of variables importance
varImp(fit_RF)

# Checking the overall accuracy with the Random Forest model
acc_RF <- confusionMatrix(predict(fit_RF, test_set), test_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "Random Forest model", Accuracy = acc_RF)

# Adding the model accuracy to the results table
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method="Random Forest Model",  
                                         Accuracy = acc_RF ))

# Printout of the model accuracy
model_accuracy %>% 
  knitr::kable()

###########
# Results #
###########

# Sorting the models by overall accuracy
accuracy_results <- accuracy_results %>%
  arrange(desc(Accuracy))

# Printout of the overall accuracy models table
accuracy_results %>% 
  knitr::kable()

# Checking the overall accuracy with the logistic regression model
acc_glm <- confusionMatrix(predict(fit_glm, validation_set), validation_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "Logistic regression model", Accuracy = acc_glm)

# Creating the results table with the validation set
accuracy_results <- data_frame(Method = "Logistic regression model", Accuracy = acc_glm)

# Checking the overall accuracy with the KNN model
acc_knn <- confusionMatrix(predict(fit_knn, validation_set), validation_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "KNN model", Accuracy = acc_knn)

# Adding the model accuracy to the results table
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method="KNN Model",  
                                         Accuracy = acc_knn ))

# Checking the overall accuracy with the CART model
acc_CART <- confusionMatrix(predict(fit_CART, validation_set), validation_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "CART model", Accuracy = acc_CART)

# Adding the model accuracy to the results table
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method="CART Model",  
                                         Accuracy = acc_CART ))

# Checking the overall accuracy with the Random Forest model
acc_RF <- confusionMatrix(predict(fit_RF, validation_set), validation_set$Outcome)$overall[["Accuracy"]]

# Saving the model accuracy
model_accuracy<- data_frame(Method = "Random Forest model", Accuracy = acc_RF)

# Adding the model accuracy to the results table
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method="Random Forest Model",  
                                         Accuracy = acc_RF ))

# Sorting the models by overall accuracy
accuracy_results <- accuracy_results %>% arrange(desc(Accuracy))

# Printout of the overall accuracy models table with the validation set
accuracy_results %>% 
  knitr::kable()

##############
# Discussion #
##############

# Checking if predicted results using Logistic Regression model and CART model are the same
identical (predict(fit_glm, validation_set),predict(fit_CART, validation_set))

# Printout of the predicted values with the Logistic Regression model
predict(fit_glm, validation_set)

# Printout of the predicted values with the CART model
predict(fit_CART, validation_set)

# Printout of the Confussion Matrix using the Logistic Regression model
confusionMatrix(predict(fit_glm, validation_set), validation_set$Outcome)

# Printout of the Confussion Matrix using the CART model
confusionMatrix(predict(fit_CART, validation_set), validation_set$Outcome)


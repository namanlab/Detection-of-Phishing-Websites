#INSTALL THE REQUIRED PACKAGES:

#Install tinytex:
if(!"tinytex" %in% installed.packages()) { tinytex::install_tinytex()} #installing tinytex
# List of other packages for the project
packages = c("tidyverse",      
             "RWeka",  
             "Hmisc",
             "caret",
             "tidyr",
             "purrr",
             "party",
             "partykit",
             "gam",
             "MLmetrics",
             "e1071",
             "Rborist",
             "randomForest",
             "LiblineaR",
             "lubridate",
             "knitr",
             "ggcorrplot",
             "gbm",
             "xfun",
             "reactable",
             "kableExtra",
             "lemon"
)
# Install CRAN packages (if not already installed)
inst <- packages %in% installed.packages()
if(length(packages[!inst]) > 0) install.packages(packages[!inst], repos = "http://cran.us.r-project.org")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#LOADING REQUIRED LIBRARIES:

library(tidyverse)
library(RWeka)
library(Hmisc)
library(caret)
library(tidyr)
library(purrr)
library(party)
library(partykit)
library(gam)
library(e1071)
library(Rborist)
library(randomForest)
library(LiblineaR)
library(lubridate)
library(knitr)
library(ggcorrplot)
library(gbm)
library(xfun)
library(reactable)
library(kableExtra)
library(tinytex)
library(MLmetrics)
library(lemon)

#------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#EXTRACTING DATA SET:

#File URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00327/Training%20Dataset.arff"

#Creating a temporary file name
temp <- tempfile()

#Downloading the file.
download.file(url,temp)

#Reading the file into an R object
phishing<-read.arff(temp)

#Removing temporary file
unlink(temp)

#Creating a sub directory 'rdas', and saving the file
dir.create(file.path(getwd(), "rdas"))
save(phishing, file = "rdas/phishing.rda")

#--------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Loading the data set.
load("rdas/phishing.rda")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#WRANGLING AND PREPROCESSING:

#Calculating the pearson correlation coefficient and associated p value for each variable with the Results
correlation_matrix <- rcorr(as.matrix(phishing), type="pearson")
r <- correlation_matrix$r
p <- correlation_matrix$P

#Making data frame of correlation coefficient and p values for Results with other attributes/predictors.
rp <- cbind(r, p) 
rp <- rp[,c(31, 62)] # Selecting only columns for correlation with Result
colnames(rp) <- c("r", "p") #Naming the columns
rp <- as.data.frame(rp) %>% mutate(significant=(p<=0.05), strong_r=(r>=0.2))
attribute <- colnames(phishing)
rp <- cbind(attribute, rp) #Essentially, adding a column to indicate the predictor.
rp <- rp[-c(31),] #Removing Result row from the data frame as we do not want to see the correlation of Result with result itself


#Plotting a scatter plot of the attributes (predictor) with their correlation coefficient with Result. 
#The color is used to indicate if the given attribute has a strong correlation (r>=0.2) with Result.
#The shape is used to indicate if the p value of the correlation coefficient of the attributes are significant (p=<0.05).

plot1<-rp %>% ggplot(aes(x=attribute, y=r, shape=significant, col=strong_r)) + 
  geom_point(size=5) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title="Correlation Plot of Features with Result",
       x="Features",
       y="Correlation Coeffecient",
       col="Strong Correlation?", 
       shape="Significant p-value?")
plot1

#Creating a directory figs and saving the plot.
dir.create(file.path(getwd(), "figs"))
ggsave(filename="figs/significant-attributes.png", plot=plot1, scale = 2)

#We chose only attributes with a strong correlation and significant p value (orange triangles).
to_choose<-(rp %>% 
              mutate(choose=(significant==TRUE & strong_r==TRUE)) %>% 
              filter(choose==TRUE))$attribute
#A final data set with those 8 attributes and Result is created and named 'phish'. 
#This data set will be used for analysis and prediction.
phish<-phishing %>% select(to_choose, Result)  


#saving final data 'phish' in 'rdas', which will be used for analysis.:
save(phish, file = "rdas/phish.rda")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#EXPLORATORY DATA ANALYSIS:

#Making a CORRELATION HEATMAP of selected predictors:
#Creating a correlation matrix and converting it to a data frame:
phish_numeric_corr<-data.frame(apply(phish, 2, function(x) as.numeric(as.character(x)))) %>% select(-Result)
corr <- round(cor(phish_numeric_corr), 1)
p.mat <- cor_pmat(phish_numeric_corr)

#Making the correlation heat map:
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE, p.mat = p.mat, legend.title = "Correlation") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Correlation Heatmap for Selected Features",
       caption="X indicates that the correlation is insignificat (95% C.I.)") +
  scale_color_brewer(palette = "Dark2")

#Saving the heat map:
ggsave("figs/Correlation_Heatmap.png",scale=2)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Calculating Prevalence (proportion of phishing websites in the data set):
pi <- phish %>% summarise(pi=mean(Result==-1)) %>% pull(pi)
pi

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Plot showing total number of phishing and legitimate websites in the data set:
result_count<-phish %>% ggplot(aes(x=Result)) + 
  geom_bar(stat = "count") + 
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Total Number of Phishing and Legitimate Websites in Dataset",
       x="Website Type",
       y="Count") +
  scale_x_discrete(labels=c("Phishing", "Legitimate")) +
  stat_count(geom = "text", colour = "white", size = 5,
             aes(label = ..count..),position=position_stack(vjust=0.5))
result_count

#Saving the plot::
ggsave(filename="figs/result-count.png", plot=result_count, scale = 2)

#---------------------------------------------------------------------------------------------------------------------------

#Plotting all the variables to show their distribution with respect to Result
phish2<-phish %>% mutate(Result=ifelse(Result==-1, "Phishing (-1)", "Legitimate (1)"))
all_variables<-gather(phish2, key="key", "value",-Result) %>% 
  ggplot(aes(x=value, fill=Result)) + 
  facet_wrap(~ key, scales = "free", ncol=3) + 
  geom_bar() + 
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, size = 9)) +
  labs(title="Distribution of Results by Attributes",
       x="Website Type Indicated\n-1: Phishing, 0: Suspicious, 1: Legitimate",
       y="Count",
       fill="Actual Result")
all_variables %>% reposition_legend('center', panel='panel-3-3')

#Saving the plot
ggsave(filename="figs/all-variables.png", plot=all_variables, scale=2)

#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#VARIOUS MACHINE LEARNING ALGORITHMS WILL NOW BE USED:
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------

#LINEAR REGRESSION:

#Converting data frame elements to numeric form for greater flexibility in model:
phish_numeric<-data.frame(apply(phish, 2, function(x) as.numeric(as.character(x))))

#Splitting the data:
set.seed(820, sample.kind = "Rounding")
y<-phish_numeric$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish_numeric[test_index, ]
train_set <- phish_numeric[-test_index, ]
#---------------------------------------------------------------------------------------------
#Fitting the model:
lm_fit <- lm(Result ~ ., data = train_set)
#---------------------------------------------------------------------------------------------
#Computing Accuracy on train set:
p_hat <- predict(lm_fit, train_set)
#Decision rule:
d<-0
y_hat <- ifelse(p_hat >= d, 1, -1) %>% factor()
ytrain<-as.factor(train_set$Result)
train_accuracy_lm<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_lm
#Accuracy:0.9144201
#-----------------------------------------------------------------------------------------
#Manipulating value for decision rule by plotting: (Why d=0?)

#Defining d (the cut off value)
d<-seq(-1, 1, length=100)

#Making the required function which takes input as 'd' and returns the accuracy on the train set.
decision_function<-function(d){
  p_hat <- predict(lm_fit, train_set)
  y_hat <- ifelse(p_hat > d, 1, -1) %>% factor()
  ytrain<-as.factor(train_set$Result)
  confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
}

#Applying the function to different values of d to make a data frame.
df<-data.frame(d=d, accuracy=sapply(d, decision_function)) 

#Plotting the data frame:
ggplot(df, aes(d, accuracy)) +
  geom_line() +
  labs(title="Variation of Accuracy with Value of d") +
  geom_vline(xintercept = 0, col="red")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9))

#Saving the graph:
ggsave(filename="figs/decision-rule-regression.png", scale = 2)
#From graph it is evident that when d is about 0, the accuracy on the training set is maximized.
#------------------------------------------------------------------------------------------------
#Computing Accuracy on test set:
p_hat <- predict(lm_fit, test_set)
#Decision rule:
y_hat <- ifelse(p_hat >= 0, 1, -1) %>% factor()
ytest<-as.factor(test_set$Result)
test_accuracy_lm<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_lm
#Accuracy:0.922
test_f1score_lm<-confusionMatrix(y_hat, ytest)[["byClass"]][["F1"]]
test_f1score_lm
#------------------------------------------------------------------------------------------
#Creating a vector of predictions: (accuracy on test set, F1 Score on test set, and accuracy on train set.)
predictions_lm<-c(train_accuracy_lm, test_accuracy_lm, test_f1score_lm)
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

# LOGISTIC REGRESSION:

#We convert -1 to 0, 0 to 0.5 and keep 1 same to create a new data set phish2.
#This is done because logistic regression requires the categories to be within 0 and 1
phish_numeric<-data.frame(apply(phish, 2, function(x) as.numeric(as.character(x))))
phish_numeric[phish_numeric=="0"]<-0.5
phish_numeric[phish_numeric=="-1"]<-0
phish_numeric<-data.frame(apply(phish_numeric, 2, function(x) as.numeric(as.character(x))))

#Splitting the data set:
set.seed(820, sample.kind = "Rounding")
y<-phish_numeric$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish_numeric[test_index, ]
train_set <- phish_numeric[-test_index, ]
#---------------------------------------------------------------------------------------
#Fitting the model:
glm_fit <- glm(Result ~ ., data=train_set, family = "binomial")
#-------------------------------------------------------------------------------------------
#Computing Accuracy on train set:
p_hat <- predict(glm_fit, train_set, type = "response")
#Decision rule:
d<-0.5
y_hat <- ifelse(p_hat >= d, 1, -1) %>% factor()
ytrain<-as.factor(phish[-test_index, ]$Result)
train_accuracy_glm<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_glm
#Accuracy:0.9173
#--------------------------------------------------------------------------------------------
#Manipulating value for decision rule by plotting: (Why d=0.5?)

#Defining values of d (cut-off values)
d<-seq(0, 1, length=100)

#Making the required function which takes input as 'd' and returns the accuracy on the train set.
decision_function<-function(d){
  p_hat <- predict(glm_fit, train_set, type = "response")
  y_hat <- ifelse(p_hat > d, 1, -1) %>% factor()
  ytrain<-as.factor(phish[-test_index, ]$Result)
  confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
}

#Applying the function to different values of d to make a data frame.
df<-data.frame(d=d, accuracy=sapply(d, decision_function)) 

# Plotting the data frame:
ggplot(df, aes(d, accuracy)) +
  geom_line() +
  labs(title="Variation of Accuracy with Value of d") +
  geom_vline(xintercept = 0.5, col="red")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9))

#Saving the plot:
ggsave(filename="figs/decision-rule-logistic_regression.png", scale = 2)
#From graph it is evident that when d is about 0.5, the accuracy on the training set is maximized.
#--------------------------------------------------------------------------------------------------
#Computing Accuracy on test set:
p_hat <- predict(glm_fit, test_set)
#Decision rule:
y_hat <- ifelse(p_hat >= 0.5, 1, -1) %>% factor()
ytest<-as.factor(phish[test_index, ]$Result)
test_accuracy_glm<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_glm
#Accuracy:0.91986
test_f1score_glm<-F_meas(y_hat, ytest)
test_f1score_glm
#------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_glm<-c(train_accuracy_glm, test_accuracy_glm, test_f1score_glm)
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#K-NEAREST NEIGHBORS (KNN)

#Splitting the data:
set.seed(820, sample.kind = "Rounding")
y<-phish$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish[test_index, ]
train_set <- phish[-test_index, ]
#---------------------------------------------------------------------------------------------------
#Fitting the model (algorithm training)
set.seed(820, sample.kind = "Rounding")
train_knn <- train(Result ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
#-----------------------------------------------------------------------------------------------------
#Plotting accuracy on train set for different values of k
ggplot(train_knn) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Accuracy vs k",
       x="k",
       y="Accuracy")
ggsave("figs/Accuracy-vs-k-knn.png")
#We saw that the parameter that maximized the estimated accuracy was:
train_knn$bestTune
#-------------------------------------------------------------------------------------------------------
#Accuracy on train set:
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_knn, train_set, type = "raw") 
ytrain<-as.factor(train_set$Result)
train_accuracy_knn<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_knn
#Accuracy:0.9439
#-----------------------------------------------------------------------------------------------------------
#Accuracy on test set
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_knn, test_set, type = "raw") 
ytest<-as.factor(test_set$Result)
test_accuracy_knn<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_knn
#Accuracy: 0.93795
test_f1score_knn<-F_meas(y_hat, ytest)
test_f1score_knn
#-------------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_knn<-c(train_accuracy_knn, test_accuracy_knn, test_f1score_knn)
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#SUPPORT VECTOR MACHINES (SVM)

#Splitting the data:
set.seed(820, sample.kind = "Rounding")
y<-phish$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish[test_index, ]
train_set <- phish[-test_index, ]
#---------------------------------------------------------------------------------------------------------
#Training the algorithm:
set.seed(820, sample.kind = "Rounding")
train_svm <- train(Result ~ ., method = "svmLinearWeights2", 
                   data = train_set)
#-------------------------------------------------------------------------------------------------------
#Accuracy on train set:
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_svm, train_set, type = "raw") 
ytrain<-as.factor(train_set$Result)
train_accuracy_svm<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_svm
#Accuracy:0.9296
#-----------------------------------------------------------------------------------------------------------
#Accuracy on test set
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_svm, test_set, type = "raw") 
ytest<-as.factor(test_set$Result)
test_accuracy_svm<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_svm
#Accuracy: 0.9351
test_f1score_svm<-F_meas(y_hat, ytest)
test_f1score_svm
#-------------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_svm<-c(train_accuracy_svm, test_accuracy_svm, test_f1score_svm)
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#GRADIENT BOOSTING MACHINES:

#Splitting the data:
set.seed(820, sample.kind = "Rounding")
y<-phish$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish[test_index, ]
train_set <- phish[-test_index, ]
#---------------------------------------------------------------------------------------------
#Using cross validation and training the data set:
set.seed(820, sample.kind = "Rounding")
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
train_gbm <- train(Result ~ ., data = train_set, method = "gbm", verbose = FALSE, trControl = fitControl)
#-------------------------------------------------------------------------------------------------------
#Accuracy on train set:
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_gbm, train_set, type = "raw") 
ytrain<-as.factor(train_set$Result)
train_accuracy_gbm<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_gbm
#Accuracy:0.9356
#-----------------------------------------------------------------------------------------------------------
#Accuracy on test set
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_gbm, test_set, type = "raw") 
ytest<-as.factor(test_set$Result)
test_accuracy_gbm<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_gbm
#Accuracy: 0.9383
test_f1score_gbm<-F_meas(y_hat, ytest)
test_f1score_gbm
#-------------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_gbm<-c(train_accuracy_gbm, test_accuracy_gbm, test_f1score_gbm)
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

#DECISION TREE:

#Splitting data set:
set.seed(820, sample.kind = "Rounding")
y<-phish$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish[test_index, ]
train_set <- phish[-test_index, ]
#--------------------------------------------------------------------------------------
#Making the Decision-tree:
output.tree<-ctree(Result~., data=train_set)
#-------------------------------------------------------------------------------------------------------
#Accuracy on train set:
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(output.tree, train_set) 
ytrain<-as.factor(train_set$Result)
train_accuracy_decisiontree<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_decisiontree
#Accuracy:0.9365
#-----------------------------------------------------------------------------------------------------------
#Accuracy on test set
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(output.tree, test_set) 
ytest<-as.factor(test_set$Result)
test_accuracy_decisiontree<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_decisiontree
#Accuracy: 0.9354
test_f1score_decisiontree<-F_meas(y_hat, ytest)
test_f1score_decisiontree
#-------------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_decisiontree<-c(train_accuracy_decisiontree, test_accuracy_decisiontree, test_f1score_decisiontree)
#-------------------------------------------------------------------------------------------------------------
#plotting with partykit
library(partykit) 
dir.create(file.path(getwd(), "figures"))
png("figures/decision-tree.png", res=144, pointsize = 0.5,  height=1500, width=4000) 
output.tree<-ctree(Result~., data=train_set)
plot(as.simpleparty(output.tree),
     tp_args = list(FUN = function(info)
       format(info$prediction, nsmall = 1)))
dev.off()
unlink(figures)
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

#RANDOM FOREST:

#Splitting the data:
set.seed(820, sample.kind = "Rounding")
y<-phish$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish[test_index, ]
train_set <- phish[-test_index, ]
#---------------------------------------------------------------------------------------------
#Using the Rborist method to build the algorithm (fitting the model):
set.seed(820, sample.kind = "Rounding")
train_rf <- train(Result ~ .,
                  method = "Rborist",
                  tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                  data = train_set)
#----------------------------------------------------------------------------------------------
#Plotting the accuracy on train set against minimum node size:
ggplot(train_rf)  +
  theme(plot.title = element_text(hjust = 0.5, size=12),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)) +
  labs(title="Accuracy vs Minimal Node Size")
ggsave(file="figs/Accuracy-vs-MinNodeSize-Random-forest.png", scale=2)
#---------------------------------------------------------------------------------------------
#Accuracy on train set:
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_rf, train_set, type = "raw") 
ytrain<-as.factor(train_set$Result)
train_accuracy_rf<-confusionMatrix(y_hat, ytrain)$overall["Accuracy"]
train_accuracy_rf
#Accuracy:0.9401
#-----------------------------------------------------------------------------------------------------------
#Accuracy on test set
set.seed(820, sample.kind = "Rounding")
y_hat<-predict(train_rf, test_set, type = "raw") 
ytest<-as.factor(test_set$Result)
test_accuracy_rf<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_rf
#Accuracy: 0.9381
test_f1score_rf<-F_meas(y_hat, ytest)
test_f1score_rf
#-------------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_rf<-c(train_accuracy_rf, test_accuracy_rf, test_f1score_rf)
#-------------------------------------------------------------------------------------------------------------
#Calculating the variable importance:
varImp(train_rf)
#Plotting the variable importance:
ggplot(varImp(train_rf)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Variable Importance")
ggsave("figs/variable-importance-random-forest.png")
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

#ENSEMBLE:

#Splitting data set:
set.seed(820, sample.kind = "Rounding")
y<-phish$Result
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- phish[test_index, ]
train_set <- phish[-test_index, ]
#-----------------------------------------------------------------------------------------------------------------------
#For ensemble we will choose the 4 algorithms that gives us maximum accuracy on train_set:
#Making a data frame for accuracy on train set for each of the algorithms (using the vectors of predictions we created untill now)
values<-c("train_accuracy", "test_accuracy", "test_f1score")
algorithm_predictions<-data.frame(lm=predictions_lm, 
                                  glm=predictions_glm, 
                                  knn=predictions_knn,
                                  svm=predictions_svm,
                                  gbm=predictions_gbm, 
                                  decisiontree=predictions_decisiontree,
                                  rf=predictions_rf)
algorithm_predictions<-as.data.frame(t(as.matrix(algorithm_predictions)))
colnames(algorithm_predictions)<-values
algorithm<-c("Linear Regression", "Logistic Regression", "k-nearest neigbours", "Support Vector Machines", "Gradient Boosting Machines", "Decision Tree", "Random Forest")
algorithm_predictions<-cbind(algorithm, algorithm_predictions)
rownames(algorithm_predictions) <- NULL

#Making a lollipop plot for the same:
algorithm_predictions %>% mutate(algorithm=reorder(algorithm,train_accuracy)) %>% 
  ggplot(aes(x=algorithm, y=train_accuracy, label=(round(train_accuracy, 4)) )) +
  geom_segment(aes(x=algorithm, xend=algorithm, y=0, yend=train_accuracy)) +
  geom_point() +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7) + 
  geom_text( position = position_nudge(y = 0.1), size=3.5) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)) +
  labs(title="Accuracy on Train Set by Different Algorithms",
       x="Algorithm",
       y="Accuracy on Train Set")

#Saving the plot:
ggsave("figs/AccuracyTrainSet-vs-Algorithm.png", scale=2)

#-----------------------------------------------------------------------------------------------------------------------
#The models we will ensemble: knn, svm, gbm and random forest. (the 4 models with maximum accuracy on train set till now are chosen)
#Decision trees are excluded because they will already be a part of random forests. 
#Random forests are themselves an ensemble model using bagging as ensemble method and decision tree as individual model.
models<-c("rf", "gbm", "knn", "svmLinearWeights2")
set.seed(820, sample.kind = "Rounding")
fits <- lapply(models, function(model){ 
  print(model)
  train(Result ~ ., method = model, data = train_set)
})
#--------------------------------------------------------------------------------------------------------------
#Fitting:
set.seed(820, sample.kind = "Rounding")
mod <- sapply(fits, function(fit){
  predict(fit, test_set)
})
#------------------------------------------------------------------------------------------------------------
#Making final data frame for test_set predicted results by each algorithm:
mod<-as.data.frame(mod, stringsAsFactors =TRUE)
colnames(mod)<-models

#Choosing the value with highest frequency in each of the predicted test_results:
mod_final<-mod %>% mutate(prop_1=rowSums((mod==1)/4)) %>%
  mutate(final_pred=ifelse(prop_1>=0.5,1,-1)) %>% select(-prop_1)

#Computing accuracy on test set:
y_hat<-as.factor(mod_final$final_pred)
ytest<-as.factor(test_set$Result)
test_accuracy_ensemble<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_ensemble
#Accuracy: 0.9398
test_f1score_ensemble<-F_meas(y_hat, ytest)
test_f1score_ensemble
#-------------------------------------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_ensemble<-c(test_accuracy_ensemble, test_f1score_ensemble) #Excludes train accuracy as now it is irrelevant.
#-----------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------

#WEIGHTED ENSEMBLE:

#calculating the Number of observations having predicted values as 2 "-1s" and 2 "1s"
sum_zero<-mod %>% mutate(rs=rowSums(mod==1)) %>% filter(rs==2)
nrow(sum_zero)
#For these values, we were making the assumption that they must be one since one is predicted more often than -1 (more 1s in train set then -1s)
#------------------------------------------------------------------------------------------------------
#To avoid this, We use "weighted ensemble".
#We assign weights to different algorithms used depending upon their accuracy in train set:
set.seed(820, sample.kind = "Rounding")
mod_train <- sapply(fits, function(fit){
  predict(fit, train_set)
})
#Model wise accuracy on train set:
accuracy_model_wise <- colMeans(mod_train == train_set$Result)
df_model<-data.frame(model=models, accuracy=accuracy_model_wise) 
#------------------------------------------------------------------------------------------------------
#Implementation of algorithm on test_set:
#The weights for each algorithm are equal to their accuracy on train_set.
#We multiply the values predicted by each:
mod_weighted<-data.frame(apply(mod, 2, function(x) as.numeric(as.character(x))))
mod_weighted$rf<-(mod_weighted$rf) * ((df_model %>% filter(model=="rf") %>% select(accuracy))[1,1])
mod_weighted$gbm<-(mod_weighted$gbm) * ((df_model %>% filter(model=="gbm") %>% select(accuracy))[1,1])
mod_weighted$knn<-(mod_weighted$knn) * ((df_model %>% filter(model=="knn") %>% select(accuracy))[1,1])
mod_weighted$svmLinearWeights2<-(mod_weighted$svmLinearWeights2) * ((df_model %>% filter(model=="svmLinearWeights2") %>% select(accuracy))[1,1])

#The required data frame with final predictions:
mod_weighted_final<-mod_weighted %>% mutate(rm=rowMeans(mod_weighted)) %>%
  mutate(final_pred=ifelse(rm>=0,1,-1)) %>% select(-rm)
#--------------------------------------------------------------------------------------------------------
#Computing accuracy on test set:
set.seed(820, sample.kind = "Rounding")
y_hat<-as.factor(mod_weighted_final$final_pred)
ytest<-as.factor(test_set$Result)
test_accuracy_weighted_ensemble<-confusionMatrix(y_hat, ytest)$overall["Accuracy"]
test_accuracy_weighted_ensemble
#Accuracy: 0.9405
test_f1score_weighted_ensemble<-F_meas(y_hat, ytest)
test_f1score_weighted_ensemble
#----------------------------------------------------------------------------------------------------------
#Creating a vector of predictions:
predictions_weighted_ensemble<-c(test_accuracy_weighted_ensemble, test_f1score_weighted_ensemble)
#----------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

#FINAL RESULTS (COMPARING THE ALGORITHMS)

#Here we will plot the F1-Scores and the accuracy on test set for all the algorithms we have used, by utilizing the vectors of predictions that we have created.
#We will use the vector of predictions created for every algorithm

#Making the required data set:
values<-c("test_accuracy", "test_f1score")
algorithm_predictions<-data.frame(lm=predictions_lm, 
                                  glm=predictions_glm, 
                                  knn=predictions_knn,
                                  svm=predictions_svm,
                                  gbm=predictions_gbm, 
                                  decisiontree=predictions_decisiontree,
                                  rf=predictions_rf)
algorithm_predictions<-as.data.frame(t(as.matrix(algorithm_predictions)))[,-1] %>% rbind(predictions_ensemble, predictions_weighted_ensemble)
colnames(algorithm_predictions)<-values
algorithm<-c("Linear Regression", "Logistic Regression", "k-nearest neigbours", "Support Vector Machines", "Gradient Boosting Machines", "Decision Tree", "Random Forest", "Ensemble", "Weighted Ensemble")
algorithm_predictions<-cbind(algorithm, algorithm_predictions)
rownames(algorithm_predictions) <- NULL
#----------------------------------------------------------------------------------------------------------
#PLOT OF ACCURACY IN TEST SET FOR EACH ALGORITHM:
algorithm_predictions %>% mutate(algorithm=reorder(algorithm,test_accuracy)) %>% 
  ggplot(aes(x=algorithm, y=test_accuracy, label=(round(test_accuracy, 4)) )) +
  geom_segment(aes(x=algorithm, xend=algorithm, y=0, yend=test_accuracy)) +
  geom_point() +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7) + 
  geom_text( position = position_nudge(y = 0.1), size=3.5) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)) +
  labs(title="Accuracy on Test Set by Different Algorithms",
       x="Algorithm",
       y="Accuracy on Test Set")

#Saving the plot
ggsave("figs/AccuracyTestSet-vs-Algorithm.png", scale=2)
#----------------------------------------------------------------------------------------------------------
#PLOT OF F1-SCORE IN TEST SET FOR EACH ALGORITHM:
algorithm_predictions %>% mutate(algorithm=reorder(algorithm,test_f1score)) %>% 
  ggplot(aes(x=algorithm, y=test_f1score, label=(round(test_f1score, 4)) )) +
  geom_segment(aes(x=algorithm, xend=algorithm, y=0, yend=test_f1score)) +
  geom_point() +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7) + 
  geom_text( position = position_nudge(y = 0.1), size=3.5) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)) +
  labs(title="F1-Score on Test Set by Different Algorithms",
       x="Algorithm",
       y="F1-Score on Test Set")

#Saving the plot
ggsave("figs/F1ScoreTestSet-vs-Algorithm.png", scale=2)

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#END OF SCRIPT







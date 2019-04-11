###################### Prediction Example ###################
library(SqlRender)
library(plyr)
library(caret)
library(pROC)
library(data.table)
library(DatabaseConnector)
library(ggplot2)
library(gridExtra)
library(Aphrodite)
#### IMPORTANT ####
#### Be sure to load either random forest or glmnet for the prediction to work ###
library(randomForest)
library(glmnet)
###
source("CopyOfsettings.R")

jdbcDrivers <<- new.env()   #In case you get a connection error uncomment this line

folder = "/home/jmbanda/OHDSI/Aphrodite-TEMP/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)

###### Fetch data from the cohort you want to predict first in a dataframe that contains person_id and start_date as the only two fields ###
dataPredictions <- getPatientDataFromStartDate(conn, dbms, datesDF$PERSON_ID, datesDF$START_DATE,  as.character('0'), flag, cdmSchema)
fv_all_test<-buildFeatureVector(flag, controlsData)

####################### Need to make this a function later ####################################
flag$labs[1]=0
featureVector<-fv_all_test
feature_vectors <- list()
featuresets=1
if (flag$drugexposures[1]) {
    feature_vectors[[featuresets]]<-featureVector$drugexposures
    featuresets = featuresets+1
}
if (flag$visits[1]) {
    feature_vectors[[featuresets]]<-featureVector$visits
    featuresets = featuresets+1
}
if (flag$observations[1]) {
    feature_vectors[[featuresets]]<-featureVector$observations
    featuresets = featuresets+1
}
if (flag$labs[1]) {
    feature_vectors[[featuresets]]<-featureVector$labs
    featuresets = featuresets+1
}

pp_total = Reduce(function(...) merge(..., by="pid", all=T), feature_vectors)
###############################################################################################


#### LOAD Saved Model
load("~/OHDSI/Aphrodite-TEMP/Sisypus-NoLabs_model_LASSO_SISNoLabs.Rda")
#### LOAD Saved Predictors
load("~/OHDSI/Aphrodite-TEMP/Sisypus-NoLabs_predictors_LASSO_SISNoLabs.Rda")
##Keep pid in the vectors
tempPred<-predictorsNames
#tempPred[length(predictorsNames)+1] <- c("Class_labels")
tempPred[length(predictorsNames)+1] <- c("pid")
fv_all_test_ready <- pp_total[ , names(pp_total) %in% tempPred]
Missing <- setdiff(tempPred, names(fv_all_test_ready))  # Find names of missing columns
fv_all_test_ready[Missing] <- 0                    # Add them, filled with '0's
fv_all_test_ready <- fv_all_test_ready[tempPred]                       # Put columns in desired order
fv_all_test_ready$Class_labels <- NULL

### For LASSO
newx = data.matrix(fv_all_test_ready)
predict(model, newdata=newx, type = "prob")
##### Predict using the model - Random Forest
predict(model, newdata=fv_all_test_ready,  type = "prob")
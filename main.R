library(caret);library(kernlab);library(skimr);
#load the data
devicesData <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv");



#split the data into training and testing sets
inTrain <- createDataPartition(y=devicesData$classe,p=.75,list=FALSE);
training <- devicesData[inTrain,];
testing <- devicesData[-inTrain,];
anyNA(training);
#look into data
summary(devicesData);
skimmed <- skim(training);
#dim(skimmed);

#there is a lot of missing values in these columns find and remove missing column

#replace empty string with NA
training [training == ""] <-NA;
names(which(colSums(is.na(training)) > 0));
training <- training[, -which(colSums(is.na(training)) > 0)];
#remove first and second columns (row number and name is not related)
training <- training[, -c(1,9)];

#change new window variable from yes no to 1 and zero
training$new_window <- ifelse (training$new_window == "no",0,1);

#change char to date
training$cvtd_timestamp <- as.Date(training$cvtd_timestamp);



#create the model using all variables with random forests as we expect
#a b c and d values (more than 2 classes )
#modfit <- train(as.factor( classe) ~ .  ,method="rf",data=training );
modfit <- randomForest( as.factor( classe) ~. , data=training, method="class")
warnings();
modfit;



#use cross validation with the testing set

predictions <- predict(modfit,newdata=testing);
predictions;
testing$classe;

#predict for the other testing data
otherTesting <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv");
predictions2 <- predict(modfit,newdata=otherTesting);
predictions2;




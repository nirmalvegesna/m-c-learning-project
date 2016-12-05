
###Title: "Practical Machine Learning Project"
###output: html_document

### Backgroud: 
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Data:
The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

####Load the libraries

```{r}
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
```

####Get the data

```{r}
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainingset <- read.csv(url(trainUrl),na.strings = c("NA","#DIV/0!",""))
testingset <- read.csv(url(testUrl), na.strings = c("NA","#DIV/0!",""))
set.seed(4567)
```

####delete all col's with NAs
```{r}
training<-trainingset[,colSums(is.na(trainingset)) == 0]
testing <-testingset[,colSums(is.na(testingset)) == 0]
```

#### delete the variables irrelevant to our project
```{r}
training <- training[ ,-c(1:7)]
testing <- testing[ ,-c(1:7)]
```

#### To perfrom cross validation the trainin set has been divided to  2 sets : subtraining(75%) and subtesting(25%)
```{r}
subsamples <- createDataPartition(y = training$classe, p=0.75, list=FALSE)
subTraining <- training[subsamples, ]
subTesting <- training[-subsamples, ]
```

#### Create the model and evaluate using Decision Tree
```{r}
model1 = rpart(classe ~ ., data = subTraining, method="class")
rpart.plot(model1, main = "Classification Tree")
prediction1 = predict(model1, subTesting, type="class")
C1 = confusionMatrix(prediction1, subTesting$classe)
C1
```

#### Create the model and evaluate using Random Forest
```{r}
model2 = randomForest(classe ~. , data=subTraining, method="class")
prediction2 = predict(model2, subTesting)
C2 = confusionMatrix(prediction2, subTesting$classe)
C2
```

#### Choosing Model
Random Forest performed better than Decision Trees. Accuary for Random Forest model was 0.9965 compared to 0.758 for Decision Tree model. The Random Forest is choosen.

#### Testset Prediction
```{r}
prediction_final = predict(model2, testing)
prediction_final
```

Write files for submission
```{r}
pml_write_files = function(x) {
  n = length(x)
  for(i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

pml_write_files(prediction_final)
```













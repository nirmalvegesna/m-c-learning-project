### Title: “Practical Machine Learning Project”

### output: html\_document

### Backgroud:

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement – a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
[http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har)
(see the section on the Weight Lifting Exercise Dataset).

### Data:

The training data for this project are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)

The test data are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

#### Load the libraries

``` {.r}
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` {.r}
library(rpart)
library(rpart.plot)
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

#### Get the data

``` {.r}
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainingset <- read.csv(url(trainUrl),na.strings = c("NA","#DIV/0!",""))
testingset <- read.csv(url(testUrl), na.strings = c("NA","#DIV/0!",""))
set.seed(4567)
```

#### delete all col’s with NAs

``` {.r}
training<-trainingset[,colSums(is.na(trainingset)) == 0]
testing <-testingset[,colSums(is.na(testingset)) == 0]
```

#### delete the variables irrelevant to our project

``` {.r}
training <- training[ ,-c(1:7)]
testing <- testing[ ,-c(1:7)]
```

#### To perfrom cross validation the trainin set has been divided to 2 sets : subtraining(75%) and subtesting(25%)

``` {.r}
subsamples <- createDataPartition(y = training$classe, p=0.75, list=FALSE)
subTraining <- training[subsamples, ]
subTesting <- training[-subsamples, ]
```

#### Create the model and evaluate using Decision Tree

``` {.r}
model1 = rpart(classe ~ ., data = subTraining, method="class")
rpart.plot(model1, main = "Classification Tree")
```


![rplot](https://cloud.githubusercontent.com/assets/23023795/20908217/20a62734-bb08-11e6-854c-80165bb641e1.png)


``` {.r}
prediction1 = predict(model1, subTesting, type="class")
C1 = confusionMatrix(prediction1, subTesting$classe)
C1
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1268  135   10   48   10
    ##          B   54  605  104   75   84
    ##          C   32   93  648   86   77
    ##          D   28   67   62  536   70
    ##          E   13   49   31   59  660
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.758           
    ##                  95% CI : (0.7457, 0.7699)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6933          
    ##  Mcnemar's Test P-Value : 2.533e-14       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9090   0.6375   0.7579   0.6667   0.7325
    ## Specificity            0.9421   0.9198   0.9289   0.9446   0.9620
    ## Pos Pred Value         0.8620   0.6562   0.6923   0.7025   0.8128
    ## Neg Pred Value         0.9630   0.9136   0.9478   0.9353   0.9411
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2586   0.1234   0.1321   0.1093   0.1346
    ## Detection Prevalence   0.3000   0.1880   0.1909   0.1556   0.1656
    ## Balanced Accuracy      0.9256   0.7787   0.8434   0.8057   0.8473

#### Create the model and evaluate using Random Forest

``` {.r}
model2 = randomForest(classe ~. , data=subTraining, method="class")
prediction2 = predict(model2, subTesting)
C2 = confusionMatrix(prediction2, subTesting$classe)
C2
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1395    4    0    0    0
    ##          B    0  944    2    0    0
    ##          C    0    1  853    7    0
    ##          D    0    0    0  796    2
    ##          E    0    0    0    1  899
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9965         
    ##                  95% CI : (0.9945, 0.998)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9956         
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9947   0.9977   0.9900   0.9978
    ## Specificity            0.9989   0.9995   0.9980   0.9995   0.9998
    ## Pos Pred Value         0.9971   0.9979   0.9907   0.9975   0.9989
    ## Neg Pred Value         1.0000   0.9987   0.9995   0.9981   0.9995
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2845   0.1925   0.1739   0.1623   0.1833
    ## Detection Prevalence   0.2853   0.1929   0.1756   0.1627   0.1835
    ## Balanced Accuracy      0.9994   0.9971   0.9978   0.9948   0.9988

#### Choosing Model

Random Forest performed better than Decision Trees. Accuary for Random
Forest model was 0.9965 compared to 0.758 for Decision Tree model. The
Random Forest is choosen.

#### Testset Prediction

``` {.r}
prediction_final = predict(model2, testing)
prediction_final
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E

Write files for submission

``` {.r}
pml_write_files = function(x) {
  n = length(x)
  for(i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

pml_write_files(prediction_final)
```

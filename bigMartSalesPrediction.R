library(caret)
library(neuralnet)
library(dummies)
library(forecast)
library(scales)
library(reshape) 
#for exhaustive search functionality
library(leaps)
#install.packages("mice")
library(mice)
#install.packages("VIM")
library(VIM)
library(ggplot2)
library(caret)
library(corrplot)
#for exhaustive search functionality
library(leaps)
#for chi-squared
library(MASS)
library(forecast)
library(scales)
library(FNN)
library(adabag)
library(rpart)
library(randomForest)
library(rpart.plot)
library(gbm)


#function to create partitions for any dataset 
create.partition <- function(dataset){
  set.seed(1) 
  partition.list<-list()
  
  #partitioning into training (50%) and validation (30%) ,test(20%)
  train.rows <- sample(rownames(dataset), dim(dataset)[1]*0.5)
  valid.rows <- sample(setdiff(rownames(dataset), train.rows), dim(dataset)[1]*0.3)
  test.rows <- setdiff(rownames(dataset), union(train.rows, valid.rows))
  
  #Assigning the  above calculated rows to the respective variables
  train.data <- dataset[train.rows, ]
  valid.data <- dataset[valid.rows, ]
  test.data  <- dataset[test.rows, ]
  
  
  #Adding the above partitions into a list and returning to the function call
  partition.list$valid.data<-valid.data
  partition.list$train.data<-train.data
  partition.list$test.data<-test.data
  
  return (partition.list)
}

#function to convert categorical variables to factors
create.factors<-function(colnames,dataset){
  dataset[colnames] <- lapply(dataset[colnames], factor) 
  return(dataset)
  
}

#function to merge different Item Ids to their respective categories
merge.categories<-function(dataset){
  temp<-dataset$Item_Identifier
  for (i in 1:length(temp))
  {
    value<-temp[i]
    if(startsWith(value,"FD")){
      temp[i]<-"Food"
    }
    else if(startsWith(value,"DR")){
      temp[i]<-"Dairy&Drinks"
    }
    else
      temp[i]<-"Health,Household&Others"
  }
  dataset$Item_category<-temp
  return(dataset)
  
}

#function to assign Grocery store as Small Size as this relationship is quiet evident in the dataset
rewrite.outletsize<-function(dataset){
 
  temp.ot<-dataset$Outlet_Type
  temp.os<-dataset$Outlet_Size
 
  
  for (i in 1:length(temp.ot)){
    if(is.na(temp.os[i])){
      value<-temp.ot[i]
      if(value =="Grocery Store"){
      temp.os[i]="Small"
      }
    }
  }
  dataset$Outlet_Size<-temp.os
  return(dataset)
}

#Imputing missing values of Item visibility
impute.itemVisibility<-function(dataset){
  
  #predicting Visibility on the basis of Item Weight,Item Type and outlet characteristics
  imputed.item.data<- mice(dataset[,c(2,4,5,9,10,11)], m=5, maxit = 50, method = 'pmm', seed = 500)
  dataset[,c(2,4,5,9,10,11)] <- complete(imputed.item.data,1)
  return(dataset)
  
}

#Imputing missing  values of Item Weight
impute.itemweight<-function(){
  
  #Getting Item Id for which weights are missing
  item.id.list<-table(missing.weight.dataset$Item_Identifier)
  item.id.list<-dimnames(item.id.list)[[1]]
  
  #Getting the subset of  dataset for which weights are missing
  bigMart.temp.dataset<-bigMart.dataset[-attributes(missing.weight.dataset)$row.names,]
  
  #Get the weight of specific Item Id from above subset and assign it to the respective Item Id having missing weights
  for(i in 1:length(item.id.list)){
    temp_id<-item.id.list[i]
    temp<-bigMart.temp.dataset[which(bigMart.temp.dataset$Item_Identifier ==temp_id),]
    if(length(unique(temp$Item_Weight)) == 1){
      imputed.weight<-temp$Item_Weight[1]
    }
    else{
      print("Unequal weights")
      imputed.weight<-mean(temp$Item_Weight)
    }
    temp1<-missing.weight.dataset[which(missing.weight.dataset$Item_Identifier ==temp_id),]
    temp1$Item_Weight <-imputed.weight 
    missing.weight.dataset[which(missing.weight.dataset$Item_Identifier ==temp_id),]<-temp1
    
  }
  return(missing.weight.dataset)
}

#Imputing missing values of Outlet Size
impute.outletttributes<-function(dataset){
  
  #Before predicting Outlet Size;lets assign grocery store outlet as small
  dataset<-rewrite.outletsize(dataset)
  
  #Using outletId,Establishment Year,Outlet size and location Type
  imputed.outlet.data<- mice(dataset[,c(7:11)], m=5, maxit = 20, seed = 500)
  #print(imputed.outlet.data)
  imputed.outlet.data$imp$Outlet_Size
  dataset[,c(7:11)] <- complete(imputed.outlet.data,1)
  return(dataset)
  
}

#Denormalizing the sales values-required for ANN model
denormalize.sales<- function(norm.dataset,min.sales,range.sales){
    for( i in 1:length(norm.dataset)){
      norm.dataset[i]=(norm.dataset[i]*range.sales)+min.sales
      
    }
  return(norm.dataset)
}


#Reading the csv file

bigMart.dataset <- read.csv("C:\\Users\\jhash\\Desktop\\university\\University Studies Docs\\BAN620\\project work\\Big Mart\\bigMart.csv", header = TRUE)

#......................................................................Data pre-processing/cleaning starts...................................................

#For Outlet Size and Item Visibility-Treating empty as NA
bigMart.dataset$Outlet_Size[bigMart.dataset$Outlet_Size == ""] <- NA
bigMart.dataset$Item_Visibility[bigMart.dataset$Item_Visibility == 0] <- NA

#Checking no of NA items in each variable
length(which(is.na(bigMart.dataset$Item_Visibility)))
length(which(is.na(bigMart.dataset$Outlet_Size)))
length(which(is.na(bigMart.dataset$Item_Weight)))

#Extracting dataset for which outlet size is missing
missing.osize.dataset<-bigMart.dataset[which(is.na(bigMart.dataset$Outlet_Size)),]
#head(bigMart.dataset[row.names(missing.osize.dataset),])

#Extracting dataset for which weight size is missing
missing.weight.dataset<-bigMart.dataset[which(is.na(bigMart.dataset$Item_Weight)),]
#head(bigMart.dataset[row.names(missing.weight.dataset),])


#It shows the no of NA values for each variable and also the combination
md.pattern(bigMart.dataset)

mp <- aggr(bigMart.dataset, col=c('green','red'),
           numbers=TRUE, sortVars=TRUE,
           labels=names(bigMart.dataset), cex.axis=.7,
           gap=3, ylab=c("Missing data","Pattern"))

#..........................................Imputing weight starts.........................................................................

missing.weight.dataset<-impute.itemweight()
bigMart.dataset[row.names(missing.weight.dataset),]<-missing.weight.dataset
#head(bigMart.dataset[attributes(missing.weight.dataset)$row.names,])


#...............................................Imputing weight ends.........................................................................


#............................................Imputing Outlet Size starts.........................................................................

#Combining Item_Fat Content
bigMart.dataset$Item_Fat_Content[bigMart.dataset$Item_Fat_Content %in% c("LF","low fat")]<-"Low Fat"
bigMart.dataset$Item_Fat_Content[bigMart.dataset$Item_Fat_Content %in% c("reg")]<-"Regular"

#Merging Item Identifiers
bigMart.dataset<-merge.categories(bigMart.dataset)

#Creating factors for below columns
factor.columns<-c("Outlet_Size","Outlet_Identifier",
                  "Outlet_Establishment_Year","Outlet_Location_Type",
                  "Outlet_Type","Item_Fat_Content","Item_Type","Item_category")
bigMart.dataset<-create.factors(factor.columns,bigMart.dataset)

#Imputing outlet Size
bigMart.dataset<-impute.outletttributes(bigMart.dataset)


#............................................Imputing Outlet size ends........................................


#............................................Imputing Item Visibility starts........................................

bigMart.dataset<-impute.itemVisibility(bigMart.dataset)

#............................................Imputing Item Visibility ends........................................
bigMart.copy.dataset<-bigMart.dataset

#head(bigMart.copy.dataset)

#Creating Dummies 
dummy.columns<-c("Outlet_Size","Outlet_Location_Type","Outlet_Type","Item_category","Item_Fat_Content")
bigMart.copy.dataset <- dummy.data.frame(bigMart.dataset,names =dummy.columns, sep = ".")

#Removing spaces in Column Names
colnames(bigMart.copy.dataset) <- gsub(" ", "", colnames(bigMart.copy.dataset))
colnames(bigMart.copy.dataset)<-make.names(colnames(bigMart.copy.dataset), unique = TRUE)


#......................................................................Data pre-processing/cleaning ends...................................................

#Checking correlation between predictors--
numerical.cols<- c("Item_Weight","Item_Visibility", "Item_MRP","Item_Outlet_Sales")
bigMart.numeric.dataset <- bigMart.copy.dataset[, numerical.cols]
cor.mat <- round(cor(bigMart.numeric.dataset),2)
corrplot(cor.mat)
#The numerical predictors doesn't seem to be correlated with each other.

#Partitioning Data

partition.list<-create.partition(bigMart.copy.dataset)
train.dataset<-partition.list$train.data
valid.dataset<-partition.list$valid.data
test.dataset<-partition.list$test.data

#4>Selecting Predictor columns

predictors.columns<-c("Item_Weight","Item_Fat_Content.Regular","Item_Visibility",
                      "Item_MRP","Item_category.Food","Item_category.Dairy.Drinks",
                      "Outlet_Size.High","Outlet_Size.Medium","Outlet_Location_Type.Tier1",
                      "Outlet_Location_Type.Tier2","Outlet_Type.SupermarketType1",
                      "Outlet_Type.SupermarketType2","Outlet_Type.SupermarketType3")
length(predictors.columns)



#.................................................Multivariable Regression Model........................................................

#Running Exhaustive Search algorithm for better selection of predictors

models.list <- regsubsets(Item_Outlet_Sales ~ ., data =train.dataset[,c(predictors.columns,"Item_Outlet_Sales")], nbest = 1, nvmax = dim(train.dataset)[2],
                          method = "exhaustive")
models.list.summary<- summary(models.list)
print(models.list.summary$which)
# showing Ad.Rsquare values for each model
print(models.list.summary$adjr2)

#MRP,Outlet Size,Category,Outlet Type,Outlet Location Type-Weight ,Fat ,Item Category and Visibility are dropped



predictor.subset<-c("Item_MRP","Outlet_Size.High","Outlet_Size.Medium",
                     "Outlet_Location_Type.Tier1",
                    "Outlet_Location_Type.Tier2","Outlet_Type.SupermarketType1",
                    "Outlet_Type.SupermarketType2","Outlet_Type.SupermarketType3")


#Taking log values of response variable as it is highly skewed
train.dataset$Item_Outlet_Sales<-log(train.dataset$Item_Outlet_Sales)
valid.dataset$Item_Outlet_Sales<-log(valid.dataset$Item_Outlet_Sales)

bigMart.regressor <- lm(Item_Outlet_Sales~ .,data=train.dataset[,c(predictor.subset,"Item_Outlet_Sales")])
summary(bigMart.regressor)

#Predicting price values over the validation data
bigMart.pred <- predict(bigMart.regressor, newdata = valid.dataset[,c(predictor.subset,"Item_Outlet_Sales")])
reg.valid.res <- data.frame(exp(valid.dataset$Item_Outlet_Sales), exp(bigMart.pred), residuals = 
                                 exp(valid.dataset$Item_Outlet_Sales) - exp(bigMart.pred))
#Calculating accuracy over the validation data
cat("Accuracy Metrics of MultiVariable Regression model  is :")
accuracy(bigMart.pred,exp(valid.dataset$Item_Outlet_Sales))
#RMSE 2681
plot(bigMart.regressor, which = 3)


#...........................................................Regression ends.....................................................



#............................................................KNN starts.....................................................


#Adding all the dummy variables unlike Regression
predictor.knnsubset<-append(predictor.subset,c("Outlet_Size.Small","Outlet_Location_Type.Tier3",
                                            "Outlet_Type.GroceryStore"))
#Standardize only MRP and weight
numerical.columns<-c(2,7)

#column name of iTem outlet sales
response.column<-c(20)
response.variable<-"Item_Outlet_Sales"

#Taking Inverse of log transformations as KNN doesn't require it
train.dataset$Item_Outlet_Sales<-exp(train.dataset$Item_Outlet_Sales)
valid.dataset$Item_Outlet_Sales<-exp(valid.dataset$Item_Outlet_Sales)

#Standardize Data
preprocessParams.knn =preProcess(train.dataset[,numerical.columns],method=c("center","scale"))
tstd.dataset<-train.dataset
vstd.dataset<-valid.dataset
tstd.dataset[,numerical.columns] <- predict(preprocessParams.knn,train.dataset[,numerical.columns])
vstd.dataset[,numerical.columns] <- predict(preprocessParams.knn,valid.dataset[,numerical.columns])

#Checking optimal value of k
bigMart.accuracy.df <- data.frame(k = seq(1, 25, 1), RMSE = rep(0, 25)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy

# compute knn for different k on validation.
for(i in 1:25) {
  #Use knn function with k=i and predict for valid dataset
  knn.fit <-knn.reg(train =tstd.dataset[,predictor.knnsubset],test=vstd.dataset[,predictor.knnsubset],
                    y=tstd.dataset[,response.column],k=i)
  bigMart.accuracy.df[i, 2] <- accuracy(knn.fit$pred,vstd.dataset[,response.column])[2]
}
plot(bigMart.accuracy.df)

#Above graph shows after k=11 RMSE is not decreasing much.
#So optimal value of k=11
#Creating KNN model 
knn.fit <-knn.reg(train =tstd.dataset[,predictor.knnsubset],test=vstd.dataset[,predictor.knnsubset],
              y=tstd.dataset[,response.column],k=11)

#Checking the performance of above KNN model
cat("Accuracy Metrics of KNN model  is :")
accuracy(knn.fit$pred,vstd.dataset[,response.column])

model2.valid.res <- data.frame(valid.dataset$Item_Outlet_Sales, knn.fit$pred, residuals = 
                                 valid.dataset$Item_Outlet_Sales- knn.fit$pred)
summary(model2.valid.res)

#RMSE is 984

#...........................................................................KNN ends.......................................................



#...........................................................................ANN starts................................................
 #We can use same predictor List as that of regression

train.norm.dataset<-train.dataset[,c(predictor.subset,"Item_Outlet_Sales")]
valid.norm.dataset<-valid.dataset[,c(predictor.subset,"Item_Outlet_Sales")]

#Normalizing the data--We have to use training data values to normalize validation data

#Scaling the data 
preprocessParams.ann =preProcess(train.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")],method=c("range"))

train.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")] <- predict(preprocessParams.ann,train.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")])
valid.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")] <- predict(preprocessParams.ann,valid.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")])

#Training the ANN model using training data- 2 hidden layer with 2 nodes

bigMart.nnmodel <- neuralnet(Item_Outlet_Sales ~ .,data =train.norm.dataset,hidden = c(3,2),linear.output = F)

#Plotting the above model
plot(bigMart.nnmodel,rep="best")

#Predicting Sales for validation data using above model

bigMart.nnmodel.pred <- compute(bigMart.nnmodel,valid.norm.dataset[,predictor.subset])
valid.predstrength<-as.data.frame(bigMart.nnmodel.pred$net.result)

#Descaliing predicted values using training data min and max Sales values
min.sales<-min(train.dataset$Item_Outlet_Sales)
range.sales<-max(train.dataset$Item_Outlet_Sales) - min.sales

pred.sales<-denormalize.sales(valid.predstrength$V1,min.sales,range.sales)
valid.sales<-denormalize.sales(valid.norm.dataset$Item_Outlet_Sales,min.sales,range.sales)

#Computing the RMSE
cat("Accuracy Metrics of ANN  model  is :")
accuracy(pred.sales,valid.sales)

plot(valid.sales,pred.sales,main="Expected vs Predicted values")

#RMSE 936


#............................................................................ANN ends.................................................


#...............................................................................Regression Tress starts...................................

#Here also we need to pass all the dummies like KNN
predictor.rtsubset<-predictor.knnsubset
bigmart.default.tree <- rpart(Item_Outlet_Sales ~., data = train.dataset[,c(predictor.rtsubset,"Item_Outlet_Sales")], method = "anova") #regression tree

prp(bigmart.default.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 
default.varImp<-varImp(bigmart.default.tree,scale=FALSE) # important variables
print(default.varImp)

# Default tree prediction
pred.default <- predict(bigmart.default.tree, newdata = valid.dataset[,predictor.rtsubset]) # predicting using valid data
valid.default.res <- data.frame(valid.dataset$Item_Outlet_Sales, pred.default, residuals = valid.dataset$Item_Outlet_Sales - pred.default) #residuals
#head(valid.default.res)

#Calculating RMSE for default tree on valid dataset
cat("Accuracy Metrics of Default Tree   is :")
accuracy(pred.default, valid.dataset$Item_Outlet_Sales) #966 RMSE

#RMSE-966
# ................................................................Regression Trees ends..................................................................


# ................................................................Random forest starts..................................................................
bigmart.rf <- randomForest(Item_Outlet_Sales ~ ., data = train.dataset[,c(predictor.rtsubset,"Item_Outlet_Sales")], ntree = 500, 
                           mtry = 4, nodesize = 5, importance = TRUE)  

#PLotting Variable and Partial Importance graphs
varImpPlot(bigmart.rf, type = 1) #item mrp, outlet type, outlet age top three variables
partialPlot(bigmart.rf, train.dataset[,c(predictor.rtsubset,"Item_Outlet_Sales")],
                                       Item_MRP,Item_Outlet_Sales,ylim=c(500,5000))

#Prediction using above model
pred.rf <- predict(bigmart.rf, newdata = valid.dataset[,predictor.rtsubset]) # predicting using valid data
valid.rf.res <- data.frame(valid.dataset$Item_Outlet_Sales, pred.rf, residuals = valid.dataset$Item_Outlet_Sales - pred.rf) #residuals
#head(valid.rf.res)

#Calculating RMSE of random forest on valid dataset
cat("Accuracy Metrics of Random Forest   is :")
accuracy(pred.rf, valid.dataset$Item_Outlet_Sales) #RMSE 978, increased from 966-default tree to 981 compared to default tree 

# ................................................................Random forest ends..................................................................


# ................................................................Boosted Trees starts..................................................................

boost.bigmart = gbm(Item_Outlet_Sales ~ ., data = train.dataset[,c(predictor.rtsubset,"Item_Outlet_Sales")], distribution = "gaussian",
                    n.trees = 10000, shrinkage = 0.01, interaction.depth = 4, cv.folds = 5)

# Prediction using boosted tree
pred.boost = predict(boost.bigmart, newdata = valid.dataset[,predictor.rtsubset]) #this one mentions usage of 704 trees to predict
valid.boost.res <- data.frame(valid.dataset$Item_Outlet_Sales, pred.boost, residuals = valid.dataset$Item_Outlet_Sales - pred.boost) #residuals
#head(valid.boost.res)

#RMSE Calculation
cat("Accuracy Metrics of Boosted Tree   is :")
accuracy(pred.boost, valid.dataset$Item_Outlet_Sales) #RMSE -942 

#.....................................................................Boosted Trees ends.................................................


#....................................................................Model Selection  starts...................................................

#ANN is providing the least RMSE-938.

#Checking the performance of ANN over test dataset

test.norm.dataset<-test.dataset[,c(predictor.subset,"Item_Outlet_Sales")]

#Normalizing the  test data--We have to use training data values to normalize test data
test.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")] <- predict(preprocessParams.ann,test.norm.dataset[,c("Item_MRP","Item_Outlet_Sales")])

#Training the ANN model using training data- 2 hidden layer with 2 nodes

bigMart.finalmodel <- neuralnet(Item_Outlet_Sales ~ .,data =train.norm.dataset,hidden = c(3,2),linear.output = F)

#Plotting the above model
plot(bigMart.finalmodel,rep="best")

#Predicting Sales for test data using above model

bigMart.finalmodel.pred <- compute(bigMart.finalmodel,test.norm.dataset[,predictor.subset])
test.predstrength<-as.data.frame(bigMart.finalmodel.pred$net.result)

#Descaling predicted values using min max sale values of training data

pred.sales<-denormalize.sales(test.predstrength$V1,min.sales,range.sales)
test.sales<-denormalize.sales(test.norm.dataset$Item_Outlet_Sales,min.sales,range.sales)

#Computing the RMSE
cat("Accuracy Metrics of ANN model on test data is :")
accuracy(pred.sales,test.sales)

#RMSE -917-the lowest
#....................................................................Model Selection ends...................................................
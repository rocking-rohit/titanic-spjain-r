# Step 1: Load + Clean Data

titanic = read.csv(file="titanic.csv", stringsAsFactors = TRUE)

Train = titanic_data[1:667,]
Test = titanic_data[668:889,]



str(Train)
summary(Train)

# fill in missing values for Age
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)

# Step 2: Create DF of independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Embarked","Cabin")
Train = Train[,!(names(Train) %in% nonvars)]
str(Train)

# Step 3: Check for MultiCollinearity
#Train$Sex = as.numeric(Train$Sex)
#Test$Sex = as.numeric(Test$Sex)
#cor(Train)

# Step 4: Build a Logistic Regression Model
TitanicLog1 = glm(Survived~., data = Train, family = binomial)
summary(TitanicLog1)

# Step 5: Revise Model
TitanicLog2 = glm(Survived ~ . - Parch, data = Train, family = binomial)
summary(TitanicLog2)

TitanicLog3 = glm(Survived ~ . - Parch - Fare, data = Train, family = binomial)
summary(TitanicLog3)

# Step 6: Test Accuracy of Model on Training Data

# always predict 0 (didn't survive)
baseAcur = 549 / (549 + 342)

predictTrain = predict(TitanicLog3, type = "response")
table(Train$Survived, predictTrain >= 0.5)

accuracy = (244 + 458) / nrow(Train)
sensitivity = 244 / (244 + 98)
specificity = 458 / (458 + 91)

cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)

# Step 7: Use Model to predict survivability for Test Data
predictTest = predict(TitanicLog3, type = "response", newdata = Test)

# no preference over error t = 0.5
Test$Survived = as.numeric(predictTest >= 0.5)
table(Test$Survived)

Predictions = data.frame(Test[c("PassengerId","Survived")])
write.csv(file = "TitanicPred", x = Predictions)
setwd("~/Desktop/GMP/R")

#library(tidyverse)
library(ggplot2)

getAllCols = function(df){
  cols = names(df[1,])
  namesVector = c()
  for(i in cols){
    namesVector = append(namesVector, i)
  }
  return(namesVector)
}

dropNa = function  (df){
  allColumns = getAllCols(df[1,])
  
  removeCols = c()
  
  
  for(i in 1:nrow(df)){
    for(j in allColumns){
      if(is.na(df[i,j])){
        removeCols = append(removeCols, i)
        break
      }
    }
  }
  
  df = df[-(removeCols), ]
  return(df)
}


replaceAgeByMedian = function(df){
  
  median = median(as.numeric(df$Age))
  
  df$Age = sub("^$", median, df$Age)
  
  #levels(as.factor(df$Age))
  
  removeCols = c()
  
  for(i in 1:nrow(df)){
      if(is.na(as.numeric(df[i,"Age"]))){
        removeCols = append(removeCols, i)
        break
      }
  }
  
  df = df[-(removeCols), ]
  
  df$Age = as.numeric(df$Age)
  
  return(df)
}


oneHotEncodingOfGender = function(df){
  
  # Replacing Empty Strings with 1, then women is 0 and male is 1
  
  df$Sex = ifelse(df$Sex == "", "male", ifelse(
    df$Sex == "male", "male","female"
  ))
  
  df$passSex = ifelse(df$Sex == "", 1, ifelse(
    df$Sex == "male", 1,0
  ))
    
  return(df)
}


findMode = function(vect){
  ux = unique(vect)
  tabl = tabulate(match(vect, ux))
  i1 = tabl == max(tabl)
  toString(ux[i1])
  
  return(ux[i1])
}




#levels(titanic$Sex)


titanic = read.csv(file="titanic.csv", stringsAsFactors = TRUE)

#View(titanic)

titanic = dropNa(titanic)

titanic = replaceAgeByMedian(titanic)

titanic = oneHotEncodingOfGender(titanic)

levels(as.factor(titanic$Pclass))



View(titanic)

levels(titanic$Embarked)

table(titanic$Embarked)

x=findMode(titanic$Embarked)


titanic$Embarked = sub("^$", x, titanic$Embarked)

titanic$familySize = as.numeric(as.character(titanic$SibSp)) + as.numeric(as.character(titanic$Parch))+1

titanic$SurvivedFac = ifelse(titanic$Survived=="1","yes","no")

#total Cabins which are empty

#Sex Pie
t = table(titanic$Sex)

data = data.frame(
  group=names(t),
  value=as.vector(t)
)

ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#Embarked Pie

emb = table(titanic$Embarked)

data = data.frame(
  group=names(emb),
  value=as.vector(emb)
)

ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#Sex and Survived Bar Plot
males = nrow(titanic[titanic$Sex == "male",])
survivedMales = nrow(titanic[titanic$Sex == "male" & titanic$Survived == "1", ])
deadMales = males - survivedMales




females = nrow(titanic[titanic$Sex == "female",])
survivedFemales = nrow(titanic[titanic$Sex == "female" & titanic$Survived == "1", ])
deadFemales = females - survivedFemales


Sex = c(rep("Males",2), rep("Females",2))
condition = rep(c( "Survived", "Dead"))
count = c(survivedMales, deadMales, survivedFemales, deadFemales)
data = data.frame(Sex,condition,count)

ggplot(data, aes(fill=condition, y=count, x=Sex)) + 
  geom_bar(position="dodge", stat="identity")


#Embarked and Survived Bar Chart
#Southampton, Cherbourg, and Queenstown

smptn = nrow(titanic[titanic$Embarked == "S",])
survivedSmptn = nrow(titanic[titanic$Embarked == "S" & titanic$Survived == "1", ])
deadSmptn = smptn - survivedSmptn

cbg = nrow(titanic[titanic$Embarked == "C",])
survivedCbg = nrow(titanic[titanic$Embarked == "C" & titanic$Survived == "1", ])
deadCbg = cbg - survivedCbg

qtn = nrow(titanic[titanic$Embarked == "Q",])
survivedQtn = nrow(titanic[titanic$Embarked == "Q" & titanic$Survived == "1", ])
deadQtn = qtn - survivedQtn



Embarked = c(rep("Southampton",2), rep("Cherbourg",2), rep("QueensTown",2))
condition = rep(c( "Survived", "Dead"))
count = c(survivedSmptn, deadSmptn, survivedCbg, deadCbg, survivedQtn, deadQtn)
data = data.frame(Embarked,condition,count)

ggplot(data, aes(fill=condition, y=count, x=Embarked)) + 
  geom_bar(position="dodge", stat="identity")


#Survivability based on pClass

pClass1 = nrow(titanic[titanic$Pclass == "1",])
survivedPclass1 = nrow(titanic[titanic$Pclass == "1" & titanic$Survived == "1", ])
deadPclass1 = pClass1 - survivedPclass1
pClass1Males = nrow(titanic[titanic$Pclass == "1" & titanic$Sex == "male",])
pClass1Females = pClass1- pClass1Males

pClass2 = nrow(titanic[titanic$Pclass == "2",])
survivedPclass2 = nrow(titanic[titanic$Pclass == "2" & titanic$Survived == "1", ])
deadPclass2 = pClass2 - survivedPclass2
pClass2Males = nrow(titanic[titanic$Pclass == "2" & titanic$Sex == "male",])
pClass2Females = pClass2- pClass2Males

pClass3 = nrow(titanic[titanic$Pclass == "3",])
survivedPclass3 = nrow(titanic[titanic$Pclass == "3" & titanic$Survived == "1", ])
deadPclass3 = pClass3 - survivedPclass3
pClass3Males = nrow(titanic[titanic$Pclass == "3" & titanic$Sex == "male",])
pClass3Females = pClass3- pClass3Males

PClass = c(rep("pClass1",2), rep("pClass2",2), rep("pClass3",2))
condition = rep(c( "Survived", "Dead"))
count = c(survivedPclass1, deadPclass1, survivedPclass2, deadPclass2, survivedPclass3, deadPclass3)
data = data.frame(PClass,condition,count)

ggplot(data, aes(fill=condition, y=count, x=PClass)) + 
  geom_bar(position="stack", stat="identity")

#Sex ratio based on pClass
PClassSex = c(rep("pClass1",2), rep("pClass2",2), rep("pClass3",2))
condition = rep(c( "Male", "Female"))
count = c(pClass1Males, pClass1Females, pClass2Males, pClass2Females, pClass3Males, pClass3Females)
data = data.frame(PClassSex,condition,count)

ggplot(data, aes(fill=condition, y=count, x=PClass)) + 
  geom_bar(position="dodge", stat="identity")


#Ages of Passengers Histogram
ggplot(data = titanic, aes(x = Age, stat="count")) +
  geom_histogram(color = 'turquoise4', bins=70)


# pclass & Embarked & Fare

ggplot(data = titanic, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + 
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format()) + 
  theme_bw()


#Relation bw Embarked, Fare and Survived
ggplot(data = titanic, aes(x = Embarked, y = Fare, fill = factor(Survived))) + 
  geom_boxplot() + 
  geom_hline(aes(yintercept = 32), 
             colour = "red", linetype = "dashed", lwd = 2) +
  scale_y_continuous(labels = scales::dollar_format()) + 
  theme_bw()


View(titanic)


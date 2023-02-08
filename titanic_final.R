setwd("~/Desktop/GMP/R")

#library(tidyverse)
library(ggplot2)
library(dplyr)
library(visdat)
library(corrplot)


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
  
  removeRows = c()
  
  
  for(i in 1:nrow(df)){
    for(j in allColumns){
      if(is.na(df[i,j])){
        removeRows = append(removeRows, i)
        break
      }
    }
  }
  
  df = df[-(removeRows), ]
  return(df)
}


#Median is unaffected by the outliers thats why we use it
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

# We replace embarked by Mode as there are only 2 missing values in the column and it follows no pattern
findMode = function(vect){
  ux = unique(vect)
  tabl = tabulate(match(vect, ux))
  i1 = tabl == max(tabl)
  toString(ux[i1])
  
  return(ux[i1])
}

#We drop cabin as 70% of the values in cabin are missing and the data will be skewed if we use it as many cabins

cleanData = function(titanic){
  
  titanic = dropNa(titanic)
  
  titanic = replaceAgeByMedian(titanic)
  
  titanic = oneHotEncodingOfGender(titanic)
  
  x=findMode(titanic$Embarked)
  
  titanic$Embarked = sub("^$", x, titanic$Embarked)
  
  return(titanic)
  
}

sexPieChart = function(titanic){
  
  t = table(titanic$Sex)
  
  data = data.frame(
    group=names(t),
    value=as.vector(t)
  )
  
  plot(ggplot(data, aes(x="", y=value, fill=group)) +
         geom_text(aes( label = group), color = "white", size=6) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0))
   
  return(TRUE)
  
}

embarkedPie = function(titanic){
  emb = table(titanic$Embarked)
  
  data = data.frame(
    group=names(emb),
    value=as.vector(emb)
  )
  
  plot(ggplot(data, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0))
  
  return(TRUE)
}

sexAndSurvived = function(titanic){
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
  
  plot(ggplot(data, aes(fill=condition, y=count, x=Sex)) + 
    geom_bar(position="dodge", stat="identity"))
  
  return(TRUE)
}


embarkedAndSurvived = function(titanic){
  
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
  
  plot(ggplot(data, aes(fill=condition, y=count, x=Embarked)) + 
    geom_bar(position="dodge", stat="identity"))
  
  return(TRUE)
}

survivableAndPClass = function(titanic){
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
  
  plot(ggplot(data, aes(fill=condition, y=count, x=PClass)) + 
    geom_bar(position="stack", stat="identity"))
  
  return(TRUE)
}



pClassEmbarkedFareBoxPlot = function(titanic){
  plot(ggplot(data = titanic, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + 
    geom_boxplot() +
    scale_y_continuous(labels = scales::dollar_format()) + 
    theme_bw())
  
  return(TRUE)
}

embarkedFareAndSurvivedBoxPlot = function(){

  plot(ggplot(data = titanic, aes(x = Embarked, y = Fare, fill = factor(Survived))) + 
    geom_boxplot() + 
    geom_hline(aes(yintercept = 40), 
               colour = "red", linetype = "dashed", lwd = 2) +
    scale_y_continuous(labels = scales::dollar_format()) + 
    theme_bw())
  
  return(TRUE)
}

titanic = read.csv(file="titanic.csv", stringsAsFactors = TRUE)

titanic = cleanData(titanic)

print(mean(titanic$Fare))

summary(titanic)

table(titanic$Embarked)

View(titanic)

names(titanic)

titanic[,1:4]

titanic_backup = titanic

titanic_backup$Pclass = as.numeric(titanic_backup$Pclass)
#titanic_backup$SexN = 0
#titanic_backup$SexN[titanic_backup$Sex == "male"] = 1

titanic_backup$FamilySize = 1+ as.numeric(titanic_backup$Parch)+ as.numeric(titanic_backup$SibSp)

#Female = 1, Male = 0

titanic_numeric = titanic_backup[, sapply(titanic_backup, is.numeric)]

M = cor(titanic_numeric, use="pairwise.complete.obs")
corrplot(M, cl.pos="b", tl.pos="d", tl.col = "black", tl.cex=0.85, type="upper", method="color")

ggplot(titanic, aes(x=Embarked)) +
  
  geom_density(aes(group=Survived, fill=Pclass), alpha = 0.5) 




#print(titanic[,"Age"])

titanic = read.csv(file="titanic.csv", stringsAsFactors = TRUE)



titanic = cleanData(titanic)

summary(titanic)



males = nrow(titanic[titanic$Sex == "male",])
survivedMales = nrow(titanic[titanic$Sex == "male" & titanic$Survived == "1", ])
deadMales = males - survivedMales

print(survivedMales)


females = nrow(titanic[titanic$Sex == "female",])
survivedFemales = nrow(titanic[titanic$Sex == "female" & titanic$Survived == "1", ])
deadFemales = females - survivedFemales

print("=========")

print(survivedFemales/deadFemales)



  
View(titanic)

summary(titanic)

sexPieChart(titanic)

embarkedPie(titanic)

sexAndSurvived(titanic)

embarkedAndSurvived(titanic)

survivableAndPClass(titanic)

pClassEmbarkedFareBoxPlot(titanic)






titanic %>%
  
  filter(Embarked %in% c("S","C","Q")) %>%
  
  ggplot() +
  
  geom_bar(aes(Embarked, fill = Pclass), position = "dodge") +
  
  facet_grid(~ Survived)


titanic %>%
  
  filter(Embarked %in% c("S","C","Q")) %>%
  
  ggplot(mapping = aes(Age, Fare, color = Survived, shape = Sex)) +
  
  geom_point() +
  
  scale_y_log10() +
  
  facet_grid(Pclass ~ Embarked)

ggplot(titanic, aes(x=Age)) +
  
  geom_density(aes(fill = Survived, color=Survived), alpha = 0.5) +
  
  facet_wrap(~Sex)


titanic$isSurvived = ifelse(titanic$Survived , "Survived", "Dead")

mosaicplot(table(titanic$isSurvived, titanic$Sex, titanic$Pclass),
           
           main = "Survival by Pclass and Sex", shade = TRUE)


titanic %>%
  
  ggplot(aes(Parch, SibSp, color = Survived)) +
  
  geom_count()


titanic %>%
  
  ggplot() +
  
  geom_bar(aes(Parch, fill = Sex), position = "dodge") +
  
  scale_y_log10()


ggplot(titanic) +
  geom_density(mapping = aes(x=Age, fill=Sex), alpha=0.5)



#Age Group survived
ggplot(titanic) +
  geom_density(aes(x=Age, group=Pclass, fill=Pclass), alpha=1)

View(titanic)
set.seed(500)

# Create random normally distributed values
x <- rnorm(1200)

# QQplot of normally distributed values
qqnorm(x)

# Add qqline to plot
qqline(x, col = "darkgreen")


library(ggplot2)

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
print(value)
data <- data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")


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

sexRatioAndPclass = function(titanic){
  
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
  
  PClassSex = c(rep("pClass1",2), rep("pClass2",2), rep("pClass3",2))
  condition = rep(c( "Male", "Female"))
  count = c(pClass1Males, pClass1Females, pClass2Males, pClass2Females, pClass3Males, pClass3Females)
  data = data.frame(PClassSex,condition,count)
  
  plot(ggplot(data, aes(fill=condition, y=count, x=PClass)) + 
         geom_bar(position="dodge", stat="identity"))
  
  return(TRUE)
}
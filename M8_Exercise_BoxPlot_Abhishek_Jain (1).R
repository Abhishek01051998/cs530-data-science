#installing library  and loading  dataset 
library(datasets)
data("iris")
summary(iris)
names(iris)
#the sepal length of each of these three different species of iris.
boxplot(Sepal.Length~Species ,data = iris ,xlab = "Species",ylab = "Sepal Length" , 
main="Sepal length of different species of iris Dataset")
#the sepal length of each of these three different species of iris.
boxplot(Sepal.Width~Species ,data = iris ,xlab = "Species",ylab = "Sepal Width" , 
main="Sepal width of different species of iris Dataset")
#the petal length of each of these three different species of iris
boxplot(Petal.Length~Species ,data = iris ,xlab = "Species",ylab = "Petal Length" ,
 main="Petal length of different species of iris Dataset")
#the petal width of each of these three different species of iris
boxplot(Petal.Width~Species ,data = iris ,xlab = "Species",ylab = "Petal Width" , 
main="Petal width of different species of iris Dataset")

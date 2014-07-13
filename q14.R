library(datasets)
data(iris)
a<-subset(iris,iris$Species=="virginica",select=c("Sepal.Length"))
apply(a,2,mean)
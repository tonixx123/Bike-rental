library(ggplot2)
library(dplyr)
pl <- ggplot(bike,aes(x=datetime,y=count)) + geom_point(aes(color=temp),alpha=0.2) + scale_color_gradient(high = 'orange',low = 'light blue')

num.cols <- sapply(bike,is.numeric)
cor.data <- cor(bike[,num.cols])

pl1 <- ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season)))
time <- function(x){
  format(x,'%H')
}

bike$hour <- sapply(bike$datetime,time)
pl3 <-  ggplot(filter(bike,workingday==1),aes(x=hour,y=count)) + geom_point(aes(color=temp),alpha=0.5) + scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl4 <- ggplot(filter(bike,workingday==0),aes(x=hour,y=count)) + geom_point(aes(color=temp),alpha=0.5,size=5) + scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) 



temp.model <- lm(count ~ temp , bike)
summary(temp.model)

temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

model <- lm(count ~. - casual - registered - datetime - atemp , bike)

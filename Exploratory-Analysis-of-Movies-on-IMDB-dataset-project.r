# Exploratory-Analysis-of-Movies-on-IMDB-dataset-project
rm(list = ls())
install.packages("tidyverse")
library(tidyverse) #helps wrangle data
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)

#LOADING DATA 
movies <- read.csv("movies.csv")

summary(movies)
glimpse(movies)

install.packages("skimr")
library(skimr)

skim(movies)

movies$budget <- movies$budget/1000000
movies$gross <- movies$gross/1000000

#Distribution of Important Variables

ggplot(movies,aes(budget,fill=company))+geom_histogram()+xlab ( "budget (in million $)")

ggplot(movies,aes(runtime))+geom_histogram(fill='blue')+xlab ( "runtime (in minutes)")

ggplot(movies,aes(year))+geom_histogram(fill='blue')+xlab ( "year")


ggplot(movies,aes(x=budget,y=score)) + geom_point() + geom_smooth(se=FALSE)

ggplot(movies[which(!is.na(movies$gross)),],aes(x=score,y=gross,rm.na=T))+ geom_point(aes(colour= genre)) + geom_smooth(method = lm)

movies.corr <- na.omit(movies)

cor.movies <- cor(movies.corr[,c('budget','year','gross','runtime','score')])
ggcorrplot(cor.movies,hc.order = TRUE,lab=TRUE)


ggplot(movies.corr,aes(x=genre,y=gross))+ geom_boxplot(fill="blue")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

movies.cost <- movies[order(-movies$budget),]%>% head(n=20)
ggplot(movies.cost,aes(x=reorder(name,budget),y=budget)) + geom_point(stat="identity",size=2, alpha=0.5,color="blue")+ coord_flip()

movies.rev <- movies[order(-movies$gross),] %>% head(n=20)
ggplot(movies.rev, aes(x=reorder(name,gross), y=gross)) +geom_bar(stat="identity", width=.5,fill="tomato3") + labs(title="Top 20 grossing movies of all time",y="revenue(in million $)",x="movie title")+coord_flip()

movies.upd <- movies.corr %>% mutate(roi=gross/budget) %>%  filter(budget>10)
toproi <- movies.upd[order(-movies.upd$roi),] %>% head(10)

ggplot(toproi, aes(x=reorder(name,roi), y=roi)) + 
  geom_bar(width=.6, fill="blue",stat="identity")+coord_flip()

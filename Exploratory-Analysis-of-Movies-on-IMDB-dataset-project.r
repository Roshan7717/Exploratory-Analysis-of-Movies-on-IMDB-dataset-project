# installing libraries 
rm(list = ls())
install.packages("tidyverse")
library(tidyverse) #helps wrangle data
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("skimr")
library(skimr)

#LOADING DATA 
movies <- read.csv("movies.csv")

summary(movies)
glimpse(movies)
skim(movies)

#WRANGLE DATA
movies$budget <- movies$budget/1000000
movies$gross <- movies$gross/1000000

#Relationship between Important Variables

#Budget vs revenue
ggplot(movies,aes(x=budget,y=gross)) + geom_point() + geom_smooth(se=FALSE)+ labs(title='Revenue vs. budget',x='budget(in million $)',y='revenue(in million $)')

#gross revenue vs score 
ggplot(movies[which(!is.na(movies$gross)),],aes(x=score,y=gross,rm.na=T))+ geom_point() + geom_smooth(se=FALSE)

# Omit missing values of revenue
movies.corr <- na.omit(movies)

#Correlation matrix
cor.movies <- cor(movies.corr[,c('budget','year','gross','runtime','score')])
ggcorrplot(cor.movies,hc.order = TRUE,lab=TRUE) + 

#Revenue by Genre
ggplot(movies.corr,aes(x=genre,y=gross))+ geom_boxplot(fill="blue")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ labs(y='revenue(in million $)',title='Revenue by Genre') 

#Top 20 costliest movies
movies.cost <- movies[order(-movies$budget),]%>% head(n=20)
ggplot(movies.cost,aes(x=reorder(name,budget),y=budget)) + geom_point(stat="identity",size=2, alpha=0.5,color="blue")+ coord_flip()

#Top 20 higest revenue 
movies.rev <- movies[order(-movies$gross),] %>% head(n=20)
ggplot(movies.rev, aes(x=reorder(name,gross), y=gross)) +geom_bar(stat="identity", width=.5,fill="tomato3") + labs(title="Top 20 grossing movies of all time",y="revenue(in million $)",x="movie title")+coord_flip()

#Top 10 ROI
movies.upd <- movies.corr %>% mutate(roi=gross/budget) %>%  filter(budget>10)
movies.upd <- movies.upd[order(-movies.upd$roi),] %>% head(10)
ggplot(movies.upd, aes(x=reorder(name,roi), y=roi)) + 
  geom_bar(width=.6, fill="blue",stat="identity")+coord_flip() + 
  labs(y="Return on Investment",x="Movie title") + ggtitle("Top 10 movies based on return on investment(Budget>1 million $") 

#Top earning production houses
avg_revenue_by_company <- movies.corr %>% group_by(company) %>% summarise(avg_revenue =mean(gross)) %>% arrange(-avg_revenue) %>% head(10)
ggplot(avg_revenue_by_company ,aes(x=reorder(company,avg_revenue),y=avg_revenue))+ geom_bar(width=.6, fill="blue",stat="identity")+labs(title="Top 10 production houses by revenue",y="revenue(in million $)",x="production house")+coord_flip()

#Top 10 actor by revenue
revenue_by_star <- movies.corr %>% group_by(star) %>% summarise(revenue =sum(gross)) %>% arrange(-revenue) %>% head(10)
ggplot(revenue_by_star ,aes(x=reorder(star,revenue),y=revenue))+ geom_bar(width=.6, fill="blue",stat="identity")+labs(title="Top 10 actor by revenue",y="revenue(in million $)",x="actor name")+coord_flip()

#top 10 director by avg imdb score 
avg_imdb_score_of_director <- movies.corr %>% group_by(director) %>% filter(votes> 1000000) %>% summarise(imdB_score =mean(score)) %>% arrange(-imdB_score) %>% head(10) 
ggplot(avg_imdb_score_of_director,aes(x=reorder(director,imdB_score),y=imdB_score))+ geom_point(stat="identity",size=2, alpha=0.5,color="blue")+labs(title="Top 10 director by revenue",y="revenue(in million $)",x="Director")+coord_flip()

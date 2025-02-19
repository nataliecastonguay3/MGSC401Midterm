## Test file for midterm project
library(ggplot2)

movies = read.csv('IMDB_data_Winter_2025.csv')
attach(movies)
View(movies)

summary(movie_budget)
hist(movie_budget)
boxplot(movie_budget)
plot = ggplot(movies,aes(y=imdb_score,x=movie_budget))
plot + geom_point()

summary(release_day)
hist(release_day)
boxplot(release_day)
plot = ggplot(movies,aes(y=imdb_score,x=release_day))
plot + geom_point()

summary(release_year)
hist(release_year)
boxplot(release_year)
plot = ggplot(movies,aes(y=imdb_score,x=release_year))
plot + geom_point()

summary(duration)
hist(duration)
boxplot(duration)
plot = ggplot(movies,aes(y=imdb_score,x=duration))
plot + geom_point()

summary(nb_news_articles)
hist(nb_news_articles)
boxplot(nb_news_articles)
plot = ggplot(movies,aes(y=imdb_score,x=nb_news_articles))
plot + geom_point()


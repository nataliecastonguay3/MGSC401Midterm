## Test file for midterm project
library(ggplot2)

movies = read.csv('IMDB_data_Winter_2025.csv')
attach(movies)
View(movies)

qualitatives = c("release_month", "language", "country", "maturity_rating", 
                "distributor", "director", "colour_film", "cinematographer", 
                 "production_company")

for (var in qualitatives) {
  movies[[var]] <- as.factor(movies[[var]])
}

quantvar = movie_budget 
summary(quantvar)
hist(quantvar)
boxplot(quantvar)
plot = ggplot(movies,aes(y=imdb_score,x=quantvar))
plot + geom_point()

qualvar = colour_film
ggplot(movies, aes(x = qualvar)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))
ggplot(movies, aes(x = qualvar, y = imdb_score)) + geom_boxplot() + theme(axis.text.x = element_text(angle=90))




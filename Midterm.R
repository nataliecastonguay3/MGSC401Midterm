### MGSC401: Midterm ###

### Clear the memory
rm(list=ls())

### Load the data
imdb = read.csv("~/Business analytics/Multivariate statistics/IMDB_data_Winter_2025.csv")

### Load necessary packages
library(ggplot2)
library(car)

### Clean the data
qualvars = c("release_month", "language", "country", "maturity_rating", "distributor", 
             "director", "colour_film", "cinematographer", "production_company")
for (var in qualvars){
  imdb[[var]] = as.factor(imdb[[var]])
}
imdb$release_month_num = match(imdb$release_month, month.abb)
attach(imdb)

### Explore the data
names(imdb)
View(imdb)
quantvars = c("imdb_score", "movie_budget", "release_day", "release_year", "release_month_num", "duration", 
              "aspect_ratio", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter",
              "nb_faces", "movie_meter_IMDBpro")
for (var in quantvars){
  hist(get(var), main = var)
  boxplot(get(var), main = var)
}

fit = vector("list", length(quantvars)-1)
ncv_tests = rep(NA, length(quantvars)-1)
outlier_tests = vector("list", length(quantvars)-1)
corr_tests = rep(NA, length(quantvars)-1)
for (i in 2:length(quantvars)){
  fit[[i-1]] = lm(imdb_score~get(quantvars[i]))
  names(fit[[i-1]]$coefficients) = c("Constant", quantvars[i])
  ncv_tests[i-1] = ncvTest(fit[[i-1]])$p
  outlier_tests[[i-1]] = outlierTest(fit[[i-1]])
  corr_tests[i-1] = cor(imdb_score, get(quantvars[i]))
  print(ggplot(imdb, aes(y=imdb_score, x=get(quantvars[i])))+xlab(quantvars[i])+geom_point())
  print(residualPlots(fit[[i-1]], main=quantvars[i]))
}
ncv_results = cbind(quantvars[2:13], ncv_tests)
corr_results = cbind(quantvars[2:13], corr_tests)


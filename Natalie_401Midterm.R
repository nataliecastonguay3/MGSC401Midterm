library(car)
library(caTools)
library(splines)
library(boot)

movies = read.csv("C:/Users/ngcas/Downloads/IMDB_data_Winter_2025 (1).csv")
attach(movies)
View(movies)

# assigning the categorical variables as factors
movies$release_month = as.factor(movies$release_month)
movies$language = as.factor(movies$language)
movies$country = as.factor(movies$country)
movies$maturity_rating = as.factor(movies$maturity_rating)
movies$distributor = as.factor(movies$distributor)
movies$director = as.factor(movies$director)
movies$colour_film = as.factor(movies$colour_film)
movies$cinematographer = as.factor(movies$cinematographer)
movies$production_company = as.factor(movies$production_company)

quantlabels = c("movie_budget", "release_day", "release_year", "duration",
                "aspect_ratio", "nb_news_articles", "actor1_star_meter", 
                "actor2_star_meter", "actor3_star_meter", "movie_meter_IMDBpro",
                "nb_faces","action","adventure","scifi","thriller","musical",
                "romance","western","sport","horror","drama","war","animation","crime")
qualabels = c("release_month","language","country","maturity_rating","colour_film","distributor","director","cinematographer","production_company")

for (name in quantlabels){
  reg = lm(imdb_score~movies[[name]])
  print(name)
  print(residualPlots(reg))
}

linear = c("movie_budget", "release_day","actor1_star_meter", 
           "actor2_star_meter", "action","adventure","scifi","thriller","musical",
           "romance","western","sport","horror","drama","war","animation","crime")
nonlinear = c("release_year","duration","aspect_ratio","nb_news_articles","actor3_star_meter",
              "movie_meter_IMDBpro","nb_faces")

for (var in linear){
  reg = lm(imdb_score~movies[[var]])
  print(var)
  print(summary(reg))
}

for (var in nonlinear){
  print(var)
  result = rep(NA,5)
  for (i in 1:5){
  reg = lm(imdb_score~poly(movies[[var]],i))
  result[i] = summary(reg)$adj.r.squared}
  
  num = which.max(result)
  print(which.max(result))
  print(max(result))
  status = lm((imdb_score~poly(movies[[var]],num)))
  print(summary(status))}

for (var in nonlinear){
  reg1 = lm(imdb_score~movies[[var]])
  reg2 = lm(imdb_score~poly(movies[[var]],2))
  reg3 = lm(imdb_score~poly(movies[[var]],3))
  reg4 = lm(imdb_score~poly(movies[[var]],4))
  reg5 = lm(imdb_score~poly(movies[[var]],5))
  print(var)
  print(anova(reg1,reg2,reg3,reg4,reg5))
}

mse = rep(NA,5)
for (i in 1:5){
  fit = glm(imdb_score~poly(release_year,i),data=movies)
  mse[i] = cv.glm(movies,fit)$delta[1]
}
mse

for (var in qualabels){
  reg = lm(imdb_score~movies[[var]])
  print(var)
  print(summary(reg))
}

# 0.06893
rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
reg= lm(imdb_score~bs(duration,knots=c(75,120), degree=i))
test = glm(imdb_score~bs(duration,knots=c(75,120), degree=i))
rresult[i] = summary(reg)$adj.r.squared
mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
k1 = quantile(movie_meter_IMDBpro,0.20)
k2 = quantile(movie_meter_IMDBpro,0.40)
k3 = quantile(movie_meter_IMDBpro,0.60)
k4 = quantile(movie_meter_IMDBpro,0.80)
reg= lm(imdb_score~bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=i))
test = glm(imdb_score~bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=i))
rresult[i] = summary(reg)$adj.r.squared
mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
  reg= lm(imdb_score~bs(release_year,knots=c(1965,1995), degree=i))
  test = glm(imdb_score~bs(release_year,knots=c(1965,1995), degree=i))
  rresult[i] = summary(reg)$adj.r.squared
  mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
  k1 = quantile(release_year,0.20)
  k2 = quantile(release_year,0.40)
  k3 = quantile(release_year,0.60)
  k4 = quantile(release_year,0.80)
  reg= lm(imdb_score~bs(release_year,knots=c(k1,k2,k3,k4), degree=i))
  test = glm(imdb_score~bs(release_year,knots=c(k1,k2,k3,k4), degree=i))
  rresult[i] = summary(reg)$adj.r.squared
  mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
  reg= lm(imdb_score~bs(nb_faces,knots=c(1965,1995), degree=i))
  test = glm(imdb_score~bs(nb_faces,knots=c(1965,1995), degree=i))
  rresult[i] = summary(reg)$adj.r.squared
  mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
  k1 = quantile(nb_faces,0.20)
  k2 = quantile(nb_faces,0.40)
  k3 = quantile(nb_faces,0.60)
  k4 = quantile(nb_faces,0.80)
  reg= lm(imdb_score~bs(nb_faces,knots=c(k1,k2,k3,k4), degree=i))
  test = glm(imdb_score~bs(nb_faces,knots=c(k1,k2,k3,k4), degree=i))
  rresult[i] = summary(reg)$adj.r.squared
  mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

plot(aspect_ratio,imdb_score)

rresult = rep(NA,5)
mseresult = rep(NA,5)
for (i in 1:5){
  k1 = quantile(actor3_star_meter,0.25)
  k2 = quantile(actor3_star_meter,0.50)
  k3 = quantile(actor3_star_meter,0.75)
  reg= lm(imdb_score~bs(actor3_star_meter,knots=c(k1,k2,k3), degree=i))
  test = glm(imdb_score~bs(actor3_star_meter,knots=c(k1,k2,k3), degree=i))
  rresult[i] = summary(reg)$adj.r.squared
  mseresult[i] = cv.glm(movies,test)$delta[1]
}
rresult
mseresult

sort(table(director),decreasing=TRUE)

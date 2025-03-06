### MGSC401: Midterm ###

### Clear the memory
rm(list=ls())

### Load the data
imdb = read.csv("~/Business analytics/Multivariate statistics/IMDB_data_Winter_2025.csv")

### Load necessary packages
library(ggplot2)
library(car)
library(splines)
require(psych)
require(lmtest)
require(plm)
require(caTools)
library(boot)

### Clean the data
imdb$release_month_num = match(imdb$release_month, month.abb)
imdb$english = ifelse(imdb$language == "English", 1, 0)
imdb$USA = ifelse(imdb$country == "USA", 1, 0)
imdb$color = ifelse(imdb$colour_film == "Color", 1, 0)
imdb_original = imdb
imdb = imdb[-c(989,1581),]
attach(imdb)

distributor_counts = table(distributor)
imdb$distributor_new = ifelse(distributor %in% names(distributor_counts[distributor_counts > 8]), 
                              distributor, "Other")
maturity_counts = table(maturity_rating)
imdb$maturity_new = ifelse(maturity_rating %in% names(maturity_counts[maturity_counts > 5]), 
                           maturity_rating, "Other")
director_counts = table(director)
imdb$director_new = ifelse(director %in% names(director_counts[director_counts > 9]), 
                           director, "Other")
cinematographer_counts = table(cinematographer)
imdb$cinematographer_new = ifelse(cinematographer %in% names(cinematographer_counts[cinematographer_counts > 8]), 
                           cinematographer, "Other")
production_counts = table(production_company)
imdb$production_new = ifelse(production_company %in% names(production_counts[production_counts > 8]), 
                             production_company, "Other")
actor1_counts = table(actor1)
imdb$actor1_new = ifelse(actor1 %in% names(actor1_counts[actor1_counts > 8]), 
                         actor1, "Other")
actor2_counts = table(actor2)
imdb$actor2_new = ifelse(actor2 %in% names(actor2_counts[actor2_counts > 8]), 
                         actor2, "Other")
actor3_counts = table(actor3)
imdb$actor3_new = ifelse(actor3 %in% names(actor3_counts[actor3_counts > 5]), 
                         actor3, "Other")
country_counts = table(country)
imdb$country_new = ifelse(country %in% names(country_counts[country_counts > 8]), 
                         country, "Other")
language_counts = table(language)
imdb$language_new = ifelse(language %in% names(language_counts[language_counts > 8]), 
                          language, "Other")
attach(imdb)

for (i in 47:56){
  imdb[,i] = as.factor(imdb[,i])
}
attach(imdb)

### Testing models
#for (d in 1:8){
  #imdb = imdb[-c(395,191,1804,1590),]
  model_formulas = list(
    imdb_score~poly(duration,5)+poly(release_year,2)+poly(movie_meter_IMDBpro,5)
    +poly(movie_budget,4)+poly(release_month_num,6)+poly(actor2_star_meter,3)+poly(nb_faces,4)
    +western+drama+action+crime+animation+romance+USA+director_new+actor3_new
  )
  
  MSE_LOOCV = rep(NA, length(model_formulas))
  
  for (i in 1:length(model_formulas)){
    fit = glm(model_formulas[[i]], data=imdb)
    MSE_LOOCV[i] = cv.glm(imdb, fit)$delta[1]
  }
  print(MSE_LOOCV)
  
  model = lm(model_formulas[[1]])
  print(summary(model))
  #outlierTest(model)
  #ncvTest(model)
#}

### Testing models
#imdb = imdb[-c(191,316,395,599,1590,1804),]
model_formulas = list(
  imdb_score~poly(duration,5)+poly(release_year,3)+poly(movie_meter_IMDBpro,5)
  +movie_budget+poly(release_month_num,2)+nb_faces+western+drama+action+crime
  +animation+romance+horror+thriller+musical+USA+color+actor3_new+director_new+maturity_new
)

MSE_LOOCV = rep(NA, length(model_formulas))

for (i in 1:length(model_formulas)){
  fit = glm(model_formulas[[i]], data=imdb)
  MSE_LOOCV[i] = cv.glm(imdb, fit)$delta[1]
}
print(MSE_LOOCV)

model = lm(model_formulas[[1]])
print(summary(model))
outlierTest(model)
ncvTest(model)
model_adjusted = coeftest(model, vcov=vcovHC(model, type="HC1"))

### Testing models
model_formulas = list(
  imdb_score~poly(duration,5)+poly(release_year,3)+poly(movie_meter_IMDBpro,5)
  +movie_budget+poly(release_month_num,2)+nb_faces+western+drama+action+crime
  +animation+romance+horror+thriller+musical+USA+color+actor3_new+director_new+maturity_new
)

MSE_LOOCV = rep(NA, length(model_formulas))

for (i in 1:length(model_formulas)){
  fit = glm(model_formulas[[i]], data=imdb)
  MSE_LOOCV[i] = cv.glm(imdb, fit)$delta[1]
}
print(MSE_LOOCV)

model = lm(model_formulas[[1]])
print(summary(model))
outlierTest(model)

### Testing models
#imdb = imdb[-c(191,316,395,1435,1590,1804),]
k1_duration = 85
k2_duration = 130
knots_duration = c(k1_duration,k2_duration)
k1_year = 1985
knots_year = c(k1_year)
k1_meter = 8000
k2_meter = 20000
knots_meter = c(k1_meter,k2_meter)
model_formulas = list(
  imdb_score~bs(duration, knots=knots_duration, degree=1)+bs(release_year, knots=knots_year, degree=1)
  +bs(movie_meter_IMDBpro, knots=knots_meter, degree=5)+movie_budget
  +poly(release_month_num,2)+nb_faces+western+drama+action+crime
  +animation+romance+horror+thriller+musical+USA+color+actor3_new+director_new+maturity_new
)
  
MSE_LOOCV = rep(NA, length(model_formulas))
  
for (i in 1:length(model_formulas)){
  fit = glm(model_formulas[[i]], data=imdb)
  MSE_LOOCV[i] = cv.glm(imdb, fit)$delta[1]
}

print(MSE_LOOCV)

model = lm(model_formulas[[1]])
print(summary(model))
outlierTest(model)

### Testing models
#imdb = imdb[-c(191,316,395,1435,1590,1804),]
k1_duration = 85
k2_duration = 130
knots_duration = c(k1_duration,k2_duration)
k1_year = 1985
knots_year = c(k1_year)
k1_meter = 8000
k2_meter = 20000
knots_meter = c(k1_meter,k2_meter)
model_formulas = list(
  imdb_score~bs(duration, knots=knots_duration, degree=1)+bs(release_year, knots=knots_year, degree=1)
  +bs(movie_meter_IMDBpro, knots=knots_meter, degree=5)+movie_budget
  +poly(release_month_num,2)+nb_faces+western+drama+action+crime
  +animation+romance+horror+thriller+musical+USA+color+actor3_new+director_new+maturity_new
)

MSE_LOOCV = rep(NA, length(model_formulas))

for (i in 1:length(model_formulas)){
  fit = glm(model_formulas[[i]], data=imdb)
  MSE_LOOCV[i] = cv.glm(imdb, fit)$delta[1]
}

print(MSE_LOOCV)

model = lm(model_formulas[[1]])
print(summary(model))
outlierTest(model)
vif(model)


library(car)
library(caTools)
library(splines)
library(boot)
library(lmtest)
library(plm)

movies = read.csv("C:/Users/ngcas/Downloads/IMDB_data_Winter_2025 (1).csv")
attach(movies)
country_counts = table(country)
movies$country_new = ifelse(country %in% names(country_counts[country_counts > 2]), 
                                country, "Other")
distributor_counts = table(distributor)
movies$distributor_new = ifelse(distributor %in% names(distributor_counts[distributor_counts > 5]), 
                              distributor, "Other")
maturity_counts = table(maturity_rating)
movies$maturity_new = ifelse(maturity_rating %in% names(maturity_counts[maturity_counts > 1]), 
                           maturity_rating, "Other")
director_counts = table(director)
movies$director_new = ifelse(director %in% names(director_counts[director_counts > 4]), 
                           director, "Other")
cinematographer_counts = table(cinematographer)
movies$cinematographer_new = ifelse(cinematographer %in% names(cinematographer_counts[cinematographer_counts > 1]), 
                                  cinematographer, "Other")
production_counts = table(production_company)
movies$production_new = ifelse(production_company %in% names(production_counts[production_counts > 2]), 
                             production_company, "Other")
language_counts = table(language)
movies$language_new = ifelse(language %in% names(language_counts[language_counts > 1]), 
                               language, "Other")
movies$distributor_new = as.factor(movies$distributor_new)
movies$maturity_new = as.factor(movies$maturity_new)
movies$director_new = as.factor(movies$director_new)
movies$cinematographer_new = as.factor(movies$cinematographer_new)
movies$production_new = as.factor(movies$production_new)
movies$country_new = as.factor(movies$country_new)
movies$language_new = as.factor(movies$language_new)
attach(movies)

k1 = quantile(movie_meter_IMDBpro,0.20)
k2 = quantile(movie_meter_IMDBpro,0.40)
k3 = quantile(movie_meter_IMDBpro,0.60)
k4 = quantile(movie_meter_IMDBpro,0.80)

# MODEL 1
model1 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4))
cv.glm(movies,model1)$delta[1]
model1 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4))
outlierTest(model1)
movies_model1 = movies[-c(395,191,1806),]
model1_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4),data=movies_model1)
cv.glm(movies_model1,model1_outliers)$delta[1]

# MODEL 2
model2 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5))
cv.glm(movies,model2)$delta[1]
model2 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5))
outlierTest(model2)
movies_model2 = movies[-c(191,395,1581,1806),]
model2_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5),data=movies_model2)
cv.glm(movies_model2,model2_outliers)$delta[1]

# MODEL 3
model3 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama)
cv.glm(movies,model3)$delta[1]
model3 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama)
outlierTest(model3)
movies_model3 = movies[-c(1806,191,1581,1436,395),]
model3_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama,data=movies_model3)
cv.glm(movies_model3,model3_outliers)$delta[1]

# MODEL 4
model4 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3))
cv.glm(movies,model4)$delta[1]
model4 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3))
outlierTest(model4)
movies_model4 = movies[-c(1806,191,1581,395,1436),]
model4_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3),data=movies_model4)
cv.glm(movies_model4,model4_outliers)$delta[1]

# MODEL 5
model5 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month)
cv.glm(movies,model5)$delta[1]
model5 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month)
outlierTest(model5)
movies_model5 = movies[-c(1806,191,1581,395,1436),]
model5_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month,data=movies_model5)
cv.glm(movies_model5,model5_outliers)$delta[1]

# MODEL 6
model6 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror)
cv.glm(movies,model6)$delta[1]
model6 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror)
outlierTest(model6)
movies_model6 = movies[-c(1806,191,1581,395,1436,1255),]
model6_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror,data=movies_model6)
cv.glm(movies_model6,model6_outliers)$delta[1]

# MODEL 7
model7 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action)
cv.glm(movies,model7)$delta[1]
model7 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action)
outlierTest(model7)
movies_model7 = movies[-c(1806,1581,191,395,1255,989,1436),]
model7_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action,data=movies_model7)
cv.glm(movies_model7,model7_outliers)$delta[1]

# MODEL 8
model8 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+language_m)
cv.glm(movies,model8)$delta[1]
model8 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+language_m)
outlierTest(model8)
movies_model8 = movies[-c(1806,1581,191,395,1255,989,1436),]
model8_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+language_m,data=movies_model8)
cv.glm(movies_model8,model8_outliers)$delta[1]

# MODEL 9
model9 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war)
cv.glm(movies,model9)$delta[1]
model9 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war)
outlierTest(model9)
movies_model9 = movies[-c(1806,1581,191,395,1255,989,1436),]
model9_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war,data=movies_model9)
cv.glm(movies_model9,model9_outliers)$delta[1]

# MODEL 10
model10 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+scifi)
cv.glm(movies,model10)$delta[1]
model10 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+scifi)
outlierTest(model10)
movies_model10 = movies[-c(1806,1581,191,395,1255,989,1436),]
model10_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war,data=movies_model10)
cv.glm(movies_model10,model10_outliers)$delta[1]

# MODEL 11
model11 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+poly(nb_faces,2))
cv.glm(movies,model11)$delta[1]
model11 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+poly(nb_faces,2))
outlierTest(model11)
movies_model11 = movies[-c(1806,1581,191,395,989,1436),]
model11_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+poly(nb_faces,2),data=movies_model11)
cv.glm(movies_model11,model11_outliers)$delta[1]

# MODEL 12
model12 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+thriller)
cv.glm(movies,model12)$delta[1]
model12 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+thriller)
outlierTest(model12)
movies_model12 = movies[-c(1806,1581,191,395,1255,989,1436),]
model12_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+thriller,data=movies_model12)
cv.glm(movies_model12,model12_outliers)$delta[1]

# MODEL 13
model13 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new)
cv.glm(movies,model13)$delta[1]
model13 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new)
outlierTest(model13)
movies_model13 = movies[-c(1581,1806,191,989,1436,316),]
model13_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new,data=movies_model13)
cv.glm(movies_model13,model13_outliers)$delta[1]

# MODEL 14
model14 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new+country_new)
cv.glm(movies,model14)$delta[1]
model14 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new+country_new)
summary(model14)
outlierTest(model14)
movies_model14 = movies[-c(1581,1806,989,191,1436,316),]
model14_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new+country_new,data=movies_model14)
cv.glm(movies_model14,model14_outliers)$delta[1]

# MODEL 15
model15 = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new+country_new+language_new)
cv.glm(movies,model15)$delta[1]
model15 = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new+country_new+director_new)
outlierTest(model15)
movies_model15 = movies[-c(1581,1806,989,191,1436,316),]
model15_outliers = glm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war+production_new+country_new+director_new,data=movies_model15)
cv.glm(movies_model14,model14_outliers)$delta[1]

best_model = lm(imdb_score~bs(duration,knots=c(75,120), degree=4)+bs(movie_meter_IMDBpro,knots=c(k1,k2,k3,k4), degree=5)+drama+poly(release_year,3)+release_month+horror+action+war,data=movies_model9)
cv.glm(movies_model9,best_model)$delta[1]
summary(best_model)
vif(best_model)
ncvTest(best_model)
coeftest(best_model, vcov=vcovHC(best_model, type="HC1"))

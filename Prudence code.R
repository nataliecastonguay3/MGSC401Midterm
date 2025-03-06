install.packages("ggplot2")
install.packages("psych")
require(psych)
library(ggplot2)
library(psych)
install.packages("car")
library(car)
install.packages("lmtest")
install.packages("plm")
require(lmtest)
require(plm)
library(boot)
library(splines)
require(caTools)
require(splines)
require(methods)



movies = read.csv("~/GitHub/MGSC401Midterm/IMDB_data_Winter_2025.csv")
attach(movies)
View(movies)
names(movies)
dictionary = read.csv("C:/Users/Prudence/OneDrive/Bureau/data_dictionary_IMDB_Winter_2025.csv")
View(dictionary)

boxplot(imdb_score)

plot(movie_budget,imdb_score)
plot(duration,imdb_score)
plot(nb_faces,imdb_score)
plot(aspect_ratio,imdb_score)
plot(release_year,imdb_score)
plot(release_day,imdb_score)
plot(nb_news_articles,imdb_score)
plot(actor1_star_meter,imdb_score)
plot(actor2_star_meter,imdb_score)
plot(actor3_star_meter,imdb_score)
plot(movie_meter_IMDBpro,imdb_score)



hist(movie_budget)
hist(imdb_score)
hist(duration)
hist(release_day)
hist(aspect_ratio)
summary(imdb_score)
boxplot(movie_budget)

quantvar = movie_budget 
summary(quantvar)
hist(quantvar)
boxplot(quantvar)
plot = ggplot(movies,aes(y=imdb_score,x=quantvar))
plot + geom_point()


ncol(movies)

qualitatives = c("release_month", "language", "country", "maturity_rating", 
                 "distributor", "director", "colour_film", "cinematographer", 
                 "production_company")

qualvar = colour_film
ggplot(movies, aes(x = qualvar)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))
ggplot(movies, aes(x = qualvar, y = imdb_score)) + geom_boxplot() + theme(axis.text.x = element_text(angle=90))

quantvars = movies[,c(4,5,6,8,9,13,15,18,20,22,25,40)]
corr_matrix = cor(quantvars)
round(corr_matrix,3) #doesn't seem to be collinearity
#Those with high correlation with imdb_score: duration (0.411), release_year (-0.195),
#nb_news_articles(0.225)
#duration seems to be correlated with movie budget (0.188)
#release_year seems to be correlated with movie budget(0.166),duration(-0.223) and aspect ratio (0.241)

reg9=lm(imdb_score~movie_budget+release_day+release_year+duration+aspect_ratio+nb_news_articles+actor1_star_meter+
          +actor2_star_meter+actor3_star_meter+nb_faces+movie_meter_IMDBpro)                    
vif(reg9)


reg6=lm(imdb_score~duration+release_year+nb_news_articles)
residualPlot(reg6, quadratic=FALSE)
ncvTest(reg6)
summary(reg6)
#p = 0.0034321 < 0.05 , signs of heteroskedasticity
coeftest(reg6, vcov=vcovHC(reg6,type="HC1")) 

reg7= lm(imdb_score~duration+release_year+nb_news_articles) 
qqPlot(reg7, envelope=list(style="none"))

#numerically
outlierTest(reg7)



reg1 = lm(imdb_score~movie_budget)
summary(reg1)




movies$genres=as.factor(movies$genres)
attach(movies)
levels(genres)

table(director)

regg=lm(imdb_score ~ duration+release_year+nb_news_articles + adventure + scifi + thriller + musical + romance + western + sport + horror + drama + war + animation + crime, data = movies)
summary(regg)

vif(regg)


qualitatives = c("release_month", "language", "country", "maturity_rating", 
                 "distributor", "director", "colour_film", "cinematographer", 
                 "production_company")


unique(movies$maturity_rating)
movies$maturity_rating=as.factor(movies$maturity_rating)
attach(movies)
levels(maturity_rating)

mreg3=lm(imdb_score ~ duration+release_year+nb_news_articles + adventure + thriller + musical + romance + horror + drama + animation
         +maturity_rating, data = movies)
summary(mreg3)


attach(movies)
levels(colour_film)
mreg4=lm(imdb_score ~ duration+release_year+nb_news_articles + adventure + thriller + musical + romance + horror + drama + animation
         +maturity_rating+colour_film, data = movies)
summary(mreg4)


y = "imdb_score"

quants = c("movie_budget", "release_day", "release_year", "duration", 
           "aspect_ratio", "nb_news_articles", "actor1_star_meter", 
           "actor2_star_meter", "actor3_star_meter", "nb_faces", 
           "movie_meter_IMDBpro")

reg_models = list()

#See which variables are significant or not 
for (i in 1:length(quants)) {
  formula_str = paste(y, "~", quants[i]) 
  reg_models[[i]] = lm(as.formula(formula_str), data = movies)  }

for (i in 1:length(quants)) {
  print(summary(reg_models[[i]]))} 

#plotting all the quantitative variables vs imdb_score
for (i in 1:length(quants)) {
  plot(movies[[quants[i]]], movies$imdb_score, 
       xlab = quants[i], ylab = "IMDB Score")  
  title(paste("IMDB Score vs", quants[i]))  
}

plot=ggplot(movies,aes(y=imdb_score,x=duration))  #creates plot environment
scatter = geom_point()  ##add scatterplot
line_d2=geom_smooth(method="lm",formula=y~poly(x,5))
plot + scatter + line_d2 

#Checking for heteroskedasticity
for (i in 1:length(quants)) {
  reg = lm(imdb_score ~ movies[[quants[i]]], data = movies)
  residualPlot(reg, quadratic = FALSE)
  title(paste("Heteroskedasticity IMDB_Score vs", quants[i]))
  print(quants[i]) 
  print(ncvTest(reg))  
}
##p-value of test below 0.05 --> heteroskedasticity


#See which polynomial degree better fits for each variable
for (i in 1:length(quants)) {
  reg_1 = lm(imdb_score ~ movies[[quants[i]]], data = movies)
  reg_2 = lm(imdb_score ~ poly(movies[[quants[i]]], 2), data = movies)
  reg_3 = lm(imdb_score ~ poly(movies[[quants[i]]], 3), data = movies)
  reg_4 = lm(imdb_score ~ poly(movies[[quants[i]]], 4), data = movies)
  reg_5 = lm(imdb_score ~ poly(movies[[quants[i]]], 5), data = movies)
  print(quants[i])
  print(anova(reg_1, reg_2, reg_3, reg_4, reg_5))
}

#For duration
z = 4  
reg_1 = lm(imdb_score ~ movies[[quants[z]]], data = movies)
reg_2 = lm(imdb_score ~ poly(movies[[quants[z]]], 2), data = movies)
reg_3 = lm(imdb_score ~ poly(movies[[quants[z]]], 3), data = movies)
reg_4 = lm(imdb_score ~ poly(movies[[quants[z]]], 4), data = movies)
reg_5 = lm(imdb_score ~ poly(movies[[quants[z]]], 5), data = movies)

print(quants[z])
print(anova(reg_1, reg_2, reg_4, reg_5))

#Check to see which models are linear vs which models are non-linear
#LOOCV Test

#movie budget
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score ~ poly(movie_budget, i), data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
mse
which.min(mse) 

#Release year
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score ~ poly(release_year, i), data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
mse
which.min(mse)#d=3

#Duration
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score ~ poly(duration, i), data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
mse
which.min(mse) 

#nb_news_articles
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score ~ poly(nb_news_articles, i), data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
mse
which.min(mse) 

#nb_faces
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score ~ poly(nb_faces, i), data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
mse
which.min(mse)

#movie_meter_IMDBpro
mse = rep(NA, 5)
for (i in 1:5){
  fit = glm(imdb_score ~ poly(movie_meter_IMDBpro, i), data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
mse
which.min(mse)


#splines
#4 knots
for (quant_var in quants) {

  k1 = quantile(movies[[quant_var]], 0.2)
  k2 = quantile(movies[[quant_var]], 0.4)
  k3 = quantile(movies[[quant_var]], 0.6)
  k4 = quantile(movies[[quant_var]], 0.8)
  
  for (deg in 1:5) {
    
plot = ggplot(movies, aes_string(x = quant_var, y = "imdb_score")) +
    geom_point(col = "grey") +
    geom_smooth(method = "lm", formula = y ~ bs(x, knots = c(k1, k2, k3, k4), degree = deg), se = FALSE, col = "blue") +
    geom_vline(xintercept = c(k1, k2, k3, k4), linetype = "dotted", col = "red") +
    ggtitle(paste("Spline Fit for", quant_var, "Degree", deg)) +
    xlab(quant_var) + 
    ylab("IMDB Score")
    print(plot)
    reg = lm(imdb_score ~ bs(movies[[quant_var]], knots = c(k1, k2, k3, k4), degree = deg), data = movies)
    print(summary(reg))
  }
}


#if graphs not working try this and rerun code
dev.off()  
graphics.off()


#10 knots
for (quant_var in quants) {
  
  k1 = quantile(movies[[quant_var]], 0.33)
  k2 = quantile(movies[[quant_var]], 0.66)
  for (deg in 1:5) {
    
    plot = ggplot(movies, aes_string(x = quant_var, y = "imdb_score")) +
      geom_point(col = "grey") +
      geom_smooth(method = "lm", formula = y ~ bs(x, knots = c(k1, k2), degree = deg), se = FALSE, col = "blue") +
      geom_vline(xintercept = c(k1, k2), linetype = "dotted", col = "red") +
      ggtitle(paste("Spline Fit for", quant_var, "Degree", deg)) +
      xlab(quant_var) + 
      ylab("IMDB Score")
    print(plot)
    reg = lm(imdb_score ~ bs(movies[[quant_var]], knots = c(k1, k2), degree = deg), data = movies)
    print(summary(reg))
  }
}

#10 knots
for (quant_var in quants) {
  var_range = range(movies[[quant_var]])
  knots = seq(var_range[1], var_range[2], length.out = 12)[2:11]  # Generate 12 points, but exclude the first and last
  
  for (deg in 1:5) {
    
    plot = ggplot(movies, aes_string(x = quant_var, y = "imdb_score")) +
      geom_point(col = "grey") +
      geom_smooth(method = "lm", formula = y ~ bs(x, knots = knots, degree = deg), se = FALSE, col = "blue") +
      geom_vline(xintercept = knots, linetype = "dotted", col = "red") +
      ggtitle(paste("Spline Fit for", quant_var, "Degree", deg)) +
      xlab(quant_var) + 
      ylab("IMDB Score")
    print(plot)
    reg = lm(imdb_score ~ bs(movies[[quant_var]], knots = knots, degree = deg), data = movies)
    print(summary(reg))
  }
}

#100 knots
for (quant_var in quants) {
  var_range <- range(movies[[quant_var]])
  knots <- seq(var_range[1], var_range[2], length.out = 101)[2:100]
  
  for (deg in 1:5) {
    
    plot = ggplot(movies, aes_string(x = quant_var, y = "imdb_score")) +
      geom_point(col = "grey") +
      geom_smooth(method = "lm", formula = y ~ bs(x, knots = knots, degree = deg), se = FALSE, col = "blue") +
      geom_vline(xintercept = knots, linetype = "dotted", col = "red") +
      ggtitle(paste("Spline Fit for", quant_var, "Degree", deg)) +
      xlab(quant_var) + 
      ylab("IMDB Score")
    print(plot)
    
    # Fit the model
    reg = lm(imdb_score ~ bs(movies[[quant_var]], knots = knots, degree = deg), data = movies)
    print(summary(reg))
  }
}

#10 knots , until degree = 10
for (quant_var in quants) {
  var_range = range(movies[[quant_var]])
  knots = seq(var_range[1], var_range[2], length.out = 12)[2:11]  # Generate 12 points, but exclude the first and last
  
  for (deg in 1:10) {
    
    plot = ggplot(movies, aes_string(x = quant_var, y = "imdb_score")) +
      geom_point(col = "grey") +
      geom_smooth(method = "lm", formula = y ~ bs(x, knots = knots, degree = deg), se = FALSE, col = "blue") +
      geom_vline(xintercept = knots, linetype = "dotted", col = "red") +
      ggtitle(paste("Spline Fit for", quant_var, "Degree", deg)) +
      xlab(quant_var) + 
      ylab("IMDB Score")
    print(plot)
    
    # Fit the model
    reg = lm(imdb_score ~ bs(movies[[quant_var]], knots = knots, degree = deg), data = movies)
    print(summary(reg))
  }
}

#Qualitatives
qualitatives1 = c("release_month", "language", "country","maturity_rating", "actor1","actor2","actor3")
qualitatives2 = c("director", "colour_film", "cinematographer","distributor","production_company")

for (qual_var in qualitatives1) {
  movies[[qual_var]] = as.factor(movies[[qual_var]])
  reg = lm(imdb_score ~ movies[[qual_var]], data = movies)
  print(qual_var)
  print(summary(reg))
}

for (qual_var in qualitatives2) {
  movies[[qual_var]] = as.factor(movies[[qual_var]])
  reg = lm(imdb_score ~ movies[[qual_var]], data = movies)
  print(qual_var)
  print(summary(reg))
}


levels(genres)

genre_columns <- c("action", "adventure", "scifi", "thriller", "musical", 
                   "romance", "western", "sport", "horror", "drama", "war", 
                   "animation", "crime")
for (genre in genre_columns) {
  reg = lm(movies$imdb_score ~ movies[[genre]])
  print( genre)
  print(summary(reg))
}


#LOOCV Test for qualitative variables
################################################Problematic###########################################################
qualitatives = c("release_month", "language", "country","maturity_rating","director", "colour_film", "cinematographer","distributor","production_company")
for (qual_var in qualitatives) {
  movies[[qual_var]] = as.factor(movies[[qual_var]])
}
mse = rep(NA, length(qualitatives))
for (i in 1:length(qualitatives)) {
  qual_var = qualitatives[i]
  formula = as.formula(paste("imdb_score ~", qual_var))
    fit = glm(formula, data = movies)
    mse[i] = cv.glm(movies, fit)$delta[1]
  }


#works but missing director & production_company
qualitatives7 = c("maturity_rating", "colour_film")
for (qual_var in qualitatives7) {
  movies[[qual_var]] = as.factor(movies[[qual_var]])
}
mse = rep(NA, length(qualitatives7))
for (i in 1:length(qualitatives7)) {
  qual_var = qualitatives7[i]
  formula = as.formula(paste("imdb_score ~", qual_var))
  fit = glm(formula, data = movies)
  mse[i] = cv.glm(movies, fit)$delta[1]
}
print(mse[i])



################Models

model0 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
 poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
  action + scifi + thriller + horror + drama + war + crime + maturity_rating + 
  director + colour_film + production_company)
summary(model0)

model1 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5))

model2 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
 poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
action + scifi + thriller + horror + drama + war + crime + maturity_rating + colour_film )

model9 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
              action + scifi + thriller + horror + drama + war + crime + maturity_rating + colour_film+director )
summary(model9)

model3 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
              action + scifi + thriller + horror + drama + war + crime)

model4 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
 poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
action + scifi + thriller + horror + drama + war + crime + maturity_rating + 
director + colour_film)
summary(model4)

model5 = lm(imdb_score ~ poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5))
summary(model5)

model6 = lm(imdb_score ~ poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
              action + scifi + thriller + horror + drama + war + crime + maturity_rating + colour_film )
summary(model6)

model7 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
              action + scifi + thriller + horror + drama  + crime 
            +maturity_rating+colour_film)
summary(model7)

model8 = lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
              war + action + scifi + thriller + horror  + drama
            +maturity_rating+colour_film) 
summary(model8)

####director, cinematographer  to check if they're in the dataset
#to see

fit = glm(model9, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse

fit = glm(model8, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse =  0.7681775 took out crime

fit = glm(model7, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse =  0.7646458 took out war 

#taking too much time to run
fit = glm(model0, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse  
#mse = 



fit = glm(model1, data = movies)
mse1 = cv.glm(movies, fit)$delta[1]
mse1  
#mse = 0.8568346

fit = glm(model2, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse = 0.7667852

fit = glm(model3, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse = 0.7793448

#taking too much time to run
fit = glm(model4, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse = 


fit = glm(model5, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse = 0.9132517

fit = glm(model6, data = movies)
mse = cv.glm(movies, fit)$delta[1]
mse
#mse = 0.7856584


####K-fold
#K=10
for (i in 1:30) {
  fit <- glm(model2, data = movies)
  mse_values[i] <- cv.glm(movies, fit, K=10)$delta[1]
}

average_mse = mean(mse_values)
average_mse # 0.8694685

for (i in 1:50) {
  fit <- glm(model2, data = movies)
  mse_values[i] <- cv.glm(movies, fit, K=10)$delta[1]
}

average_mse = mean(mse_values)
average_mse # 0.8797294


for (i in 1:50) {
  fit <- glm(model2, data = movies)
  mse_values[i] <- cv.glm(movies, fit, K=5)$delta[1]
}
average_mse = mean(mse_values)
average_mse #0.9086433

for (i in 1:50) {
  fit <- glm(model2, data = movies)
  mse_values[i] <- cv.glm(movies, fit, K=100)$delta[1]
}
average_mse = mean(mse_values)
average_mse #0.8636971


#Validation Set Test
for (i in 1:30) {
sample = sample.split(movies$imdb_score,SplitRatio=0.5)
train = subset(movies, sample == TRUE)
test = subset(movies,sample ==FALSE)
fit = lm(model2, data=train)    
test$pred=predict(fit,test)
test$res=(test$imdb_score-test$pred) #error = actual - predicted
test$res_sq = test$res^2  #squared error
mse[i] = mean(test$res_sq)
}
mse
mean(mse)
#4728.909

for (i in 1:30) {
sample = sample.split(movies$imdb_score,SplitRatio=0.5)
train = subset(movies, sample == TRUE)
test = subset(movies,sample ==FALSE)
fit = lm(model7, data=train)    
test$pred=predict(fit,test)
test$res=(test$imdb_score-test$pred) #error = actual - predicted
test$res_sq = test$res^2  #squared error
mse[i] = mean(test$res_sq)
}
mse
mean(mse) #867.8715


### Clear the memory
rm(list=ls())

### Load the data
imdb = read.csv("~/GitHub/MGSC401Midterm/IMDB_data_Winter_2025.csv")
View(imdb)

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
View(imdb)

distributor_counts = table(distributor)
imdb$distributor_new = ifelse(distributor %in% names(distributor_counts[distributor_counts > 1]), 
                              distributor, "Other")
maturity_counts = table(maturity_rating)
imdb$maturity_new = ifelse(maturity_rating %in% names(maturity_counts[maturity_counts > 1]), 
                           maturity_rating, "Other")
director_counts = table(director)
imdb$director_new = ifelse(director %in% names(director_counts[director_counts > 1]), 
                           director, "Other")
cinematographer_counts = table(cinematographer)
imdb$cinematographer_new = ifelse(cinematographer %in% names(cinematographer_counts[cinematographer_counts > 1]), 
                                  cinematographer, "Other")
production_counts = table(production_company)
imdb$production_new = ifelse(production_company %in% names(production_counts[production_counts > 5]), 
                             production_company, "Other")
actor1_counts = table(actor1)
imdb$actor1_new = ifelse(actor1 %in% names(actor1_counts[actor1_counts > 1]), 
                         actor1, "Other")
actor2_counts = table(actor2)
imdb$actor2_new = ifelse(actor2 %in% names(actor2_counts[actor2_counts > 1]), 
                         actor2, "Other")
actor3_counts = table(actor3)
imdb$actor3_new = ifelse(actor3 %in% names(actor3_counts[actor3_counts > 1]), 
                         actor3, "Other")
attach(imdb)


################Models part2
model_1 = lm(imdb_score ~ duration) 
fit = glm(model_1, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.9915897

model_2=lm(imdb_score ~ duration + poly(movie_meter_IMDBpro, 5)) 
fit = glm(model_2, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.9365571

model_3=lm(imdb_score ~ duration + poly(movie_meter_IMDBpro, 5) + drama) 
fit = glm(model_3, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.854959

model_4=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama) 
fit = glm(model_4, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8269758

model_5=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num) 
fit = glm(model_5, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8278632

model_6=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror) 
fit = glm(model_6, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8223176

model_7=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action) 
fit = glm(model_7, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8085888

model_8=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+english) 
fit = glm(model_8, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8125034

model_9=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war) 
fit = glm(model_9, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.807741

model_10=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+scifi) 
fit = glm(model_10, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse # 0.8085317
outlierTest(model_10)
imdb3=imdb[-c(1804,395,191,1254),]
model_10=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+scifi) 
fit = glm(model_10, data = imdb3)
mse = cv.glm(imdb3, fit)$delta[1]
mse   #0,7755566

model_11=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+nb_faces) 
fit = glm(model_11, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8013706

outlierTest(model_11)
imdb2=imdb[-c(1804,395,191),]
model_11=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+nb_faces) 
fit = glm(model_11, data = imdb2)
mse = cv.glm(imdb2, fit)$delta[1]
mse   #0.7750851         
      

model_12=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+thriller) 
fit = glm(model_12, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8080903

model_13=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+distributor_new) 
fit = glm(model_13, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8619958

model_14=lm(imdb_score ~ duration +poly(release_year, 3)+ poly(movie_meter_IMDBpro, 5) + drama+release_month_num+horror+action+war+distributor_new+color) 
fit = glm(model_14, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #0.8574713

model_15=lm(imdb_score ~ movie_budget + poly(release_year, 3) + poly(duration, 5) + 
              poly(nb_faces, 2) + poly(movie_meter_IMDBpro, 5) + 
              action + scifi + thriller + horror + drama + war + crime + maturity_new + 
              director_new + color + cinematographer_new)
fit = glm(model_15, data = imdb)
mse = cv.glm(imdb, fit)$delta[1]
mse #1.105508






































































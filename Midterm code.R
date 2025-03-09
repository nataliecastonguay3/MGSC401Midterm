##### Midterm #####

### Magali Schlupp - 261102966
### Prudence Tchamdja - 261119632
### Natalie Castonguay - 260963636
### Lauren Salo - 261179713

### Clear the memory
rm(list=ls())

### Load the data
movies = read.csv("~/Business analytics/Multivariate statistics/IMDB_data_Winter_2025.csv")
test_data = read.csv("~/Business analytics/Multivariate statistics/test_data_IMDB_Winter_2025.csv")

### Load necessary packages
library(ggplot2)
library(car)
library(splines)
require(psych)
require(lmtest)
require(plm)
require(caTools)
library(boot)
require(methods)
require(stargazer)

### Arrange the data
movies$release_month_num = match(movies$release_month, month.abb)
movies$english = ifelse(movies$language == "English", 1, 0)
movies$USA = ifelse(movies$country == "USA", 1, 0)
movies$color = ifelse(movies$colour_film == "Color", 1, 0)
attach(movies)

### Let's create new columns using the categorical variables where every category with more than a certain
# number of observations stay the same and all categories below this threshold become "Other". We did this
# because too many categories had only 1 observation, which prevented us from testing them using the LOOCV
# method as categories in the testing data would not be present in the training data. We began with a threshold
# of 5 observations so that the code would not take too long to run and then experimented with the threshold
# for the variables included in our final model.
distributor_counts = table(distributor)
movies$distributor_new = ifelse(distributor %in% names(distributor_counts[distributor_counts > 5]), 
                              distributor, "Other")
maturity_counts = table(maturity_rating)
movies$maturity_new = ifelse(maturity_rating %in% names(maturity_counts[maturity_counts > 5]), 
                           maturity_rating, "Other")
director_counts = table(director)
movies$director_new = ifelse(director %in% names(director_counts[director_counts > 9]), 
                           director, "Other")
cinematographer_counts = table(cinematographer)
movies$cinematographer_new = ifelse(cinematographer %in% names(cinematographer_counts[cinematographer_counts > 5]), 
                                  cinematographer, "Other")
production_counts = table(production_company)
movies$production_new = ifelse(production_company %in% names(production_counts[production_counts > 5]), 
                             production_company, "Other")
actor1_counts = table(actor1)
movies$actor1_new = ifelse(actor1 %in% names(actor1_counts[actor1_counts > 5]), 
                         actor1, "Other")
actor2_counts = table(actor2)
movies$actor2_new = ifelse(actor2 %in% names(actor2_counts[actor2_counts > 5]), 
                         actor2, "Other")
actor3_counts = table(actor3)
movies$actor3_new = ifelse(actor3 %in% names(actor3_counts[actor3_counts > 5]), 
                         actor3, "Other")
country_counts = table(country)
movies$country_new = ifelse(country %in% names(country_counts[country_counts > 5]), 
                          country, "Other")
language_counts = table(language)
movies$language_new = ifelse(language %in% names(language_counts[language_counts > 5]), 
                           language, "Other")

### Put every new categorical variable as a factor
for (i in 47:56){
  movies[,i] = as.factor(movies[,i])
}
movies$maturity_rating = as.factor(movies$maturity_rating)

### Save the original dataset so that we can modify the main one as much as we need
movies_original = movies
attach(movies)

### Explore the data
names(movies)
View(movies)
quantvars = c("movie_budget", "release_day", "release_year", "release_month_num", "duration", 
              "aspect_ratio", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter",
              "nb_faces", "movie_meter_IMDBpro")
qualvars = c("distributor_new", "maturity_new", "director_new", "cinematographer_new", 
             "production_new", "actor1_new", "actor2_new", "actor3_new", "country_new", 
             "language_new")
quant_qualvars = c("USA", "english", "color")
genres = c("action", "adventure", "scifi", "thriller", "musical", "romance", "western", 
           "sport", "horror", "drama", "war", "animation", "crime")

### Visuals
hist(imdb_score)
boxplot(imdb_score)
ggplot(movies, aes(y=imdb_score, x=imdb_score))+geom_point()+geom_smooth(method="lm", formula=y~x) 

for (var in quantvars){
  hist(get(var), main = var)
  boxplot(get(var), main = var)
  print(ggplot(movies, aes(y=imdb_score, x=get(var)))+geom_point()+geom_smooth(method="lm", formula=y~x)+xlab(var)) 
}

for (var in qualvars){
  print(ggplot(movies, aes(x=get(var)))+xlab(var)+geom_bar())
  boxplot(get(var), main = var)
  plot(get(var), imdb_score, xlab = var)
  print(ggplot(movies, aes(y=imdb_score, x=get(var)))+geom_point()+xlab(var)) 
}

for (var in genres){
  hist(get(var), main = var)
  boxplot(get(var), main = var)
  plot(get(var), imdb_score, xlab = var)
}

for (var in quant_qualvars){
  hist(get(var), main = var)
  boxplot(get(var), main = var)
  plot(get(var), imdb_score, xlab = var)
}

### Quantitative variables
# Simple linear regressions and 4 model issues
reg = vector("list", length(quantvars))
significance = rep(NA, length(quantvars))
linear = rep(NA, length(quantvars))
heteroskedasticity = rep(NA, length(quantvars))
outliers = rep(NA, length(quantvars))
for (i in 1:length(quantvars)){
  reg[[i]] = lm(imdb_score~get(quantvars[i]))
  names(reg[[i]]$coefficients) = c("constant", quantvars[i])
  print(summary(reg[[i]]))
  if ((summary(reg[[i]])$coefficients[2,4]) <= 0.05){ 
    significance[i] = "Significant"
  } else {
    significance[i] = "Not significant"
  }
  residual_test = residualPlots(reg[[i]], main=quantvars[i])
  if (residual_test[1,2] <= 0.1){ 
    linear[i] = "Not linear"
  } else {
    linear[i] = "Linear"
  }
  hetero = ncvTest(reg[[i]])
  if (hetero$p < 0.05){ 
    heteroskedasticity[i] = "Yes"
  } else {
    heteroskedasticity[i] = "No"
  }
  outlier = outlierTest(reg[[i]])
  if (outlier$signif == TRUE){
    outliers[i] = paste(names(outlier$rstudent), collapse=", ")
  }
}
recap_quant = cbind(quantvars, significance, linear, heteroskedasticity, outliers)

linear = c("movie_budget", "release_day", "actor1_star_meter", "actor2_star_meter")
nonlinear = c("release_year", "release_month_num", "duration", "aspect_ratio", "nb_news_articles",
              "actor3_star_meter", "movie_meter_IMDBpro", "nb_faces")

# Correlation
corr_matrix = cor(movies[,c(4,5,6,8,9,13,15,18,20,22,25,40,43)])
round(corr_matrix, 2)
pairs.panels(corr_matrix)
corr_tests = rep(NA, length(quantvars))
for (i in 1:length(quantvars)){
  corr_tests[i] = cor(imdb_score, get(quantvars[i]))
}
corr_results = cbind(quantvars, corr_tests)

# Multiple linear regression
reg_quant = lm(movies$imdb_score~movie_budget+release_day
               +release_year+release_month_num+duration+aspect_ratio+nb_news_articles
               +actor1_star_meter+actor2_star_meter+actor3_star_meter
               +nb_faces+movie_meter_IMDBpro)
summary(reg_quant)
vif(reg_quant)

# Polynomial regressions
anova = vector("list", length(nonlinear))
for (i in 1:length(nonlinear)){
  reg = vector("list", 5)
  for (j in 1:5){
    reg[[j]] = lm(imdb_score~poly(get(nonlinear[i]),j))
  }
  anova[[i]] = anova(reg[[1]],reg[[2]],reg[[3]],reg[[4]],reg[[5]])
  print(anova[[i]])
}

anova_table = as.data.frame(matrix(NA, nrow=length(nonlinear), ncol=5))
colnames(anova_table) = c("d=1", "d=2", "d=3", "d=4", "d=5")
for (j in 1:length(nonlinear)){
  for (i in 1:5){
    anova_table[j,i] = anova[[j]][i,"Pr(>F)"]
  }
}
best_degree = c(3, 2, 5, 2, 5, 4, 5, 1) # by looking at anova test and seeing if values are around or below 0.05
best_degree_2 = rep(NA, length(nonlinear))
anova_table = cbind(nonlinear, anova_table, best_degree, best_degree_2)

for (var in nonlinear){
  result = rep(NA,5)
  for (i in 1:5){
    reg = lm(imdb_score~poly(movies[[var]],i))
    result[i] = summary(reg)$adj.r.squared
  }
  num = which.max(result)
  index = which(var == nonlinear)
  best_degree_2[index] = num
  anova_table[index,8] = num
}

# Splines
# Using quantiles
spline_r2 = list()
spline_mse = list()
for (i in 1:length(nonlinear)){
  spline_r2_matrix = matrix(NA, nrow=5, ncol=5)
  spline_mse_matrix = matrix(NA, nrow=5, ncol=5)
  for (k in 1:5){
    knots = rep(NA, k)
    knots = quantile(get(nonlinear[i]),(1:k)/(k+1))
    for (j in 1:5){
      eq_spline = geom_smooth(method="lm", formula=y~bs(x,knots=knots, degree=j))
      knots_lines = geom_vline(xintercept=knots, linetype="dotted")
      plot = ggplot(movies, aes(y=imdb_score, x=get(nonlinear[i])))
      scatter = geom_point()
      print(plot+scatter+eq_spline+knots_lines+xlab(nonlinear[i]))
      spline = lm(imdb_score~bs(get(nonlinear[i]), knots=knots, degree=j))
      test = glm(imdb_score~bs(get(nonlinear[i]), knots=knots, degree=j))
      spline_r2_matrix[k,j] = summary(spline)$adj.r.squared
      spline_mse_matrix[k,j] = cv.glm(movies, test)$delta[1]
    }
  }
  spline_r2[[i]] = spline_r2_matrix
  spline_mse[[i]] = spline_mse_matrix
}

# Visually
k1_year = 1985
knots_year = c(k1_year)
k1_month = 4.5
k2_month = 7.5
knots_month = c(k1_month,k2_month)
k1_duration = 85
k2_duration = 130
knots_duration = c(k1_duration,k2_duration)
k1_aspect = 2
knots_aspect = c(k1_aspect)
k1_articles = 1500
knots_articles = c(k1_articles)
k1_actor3 = 2000
k2_actor3 = 1800000
knots_actor3 = c(k1_actor3,k2_actor3)
k1_meter = 8000
k2_meter = 20000
knots_meter = c(k1_meter,k2_meter)
k1_faces = 6
knots_faces = c(k1_faces)
knots = list(knots_year, knots_month, knots_duration, knots_aspect,  knots_articles, 
             knots_actor3, knots_meter, knots_faces)
spline_r2_table = as.data.frame(matrix(NA, nrow=length(nonlinear), ncol=5))
colnames(spline_r2_table) = c("d=1", "d=2", "d=3", "d=4", "d=5")
spline_mse_table = as.data.frame(matrix(NA, nrow=length(nonlinear), ncol=5))
colnames(spline_mse_table) = c("d=1", "d=2", "d=3", "d=4", "d=5")
for (i in 1:length(nonlinear)){
  for (j in 1:5){
    eq_spline = geom_smooth(method="lm", formula=y~bs(x,knots=knots[[i]], degree=j))
    knots_lines = geom_vline(xintercept=knots[[i]], linetype="dotted")
    plot = ggplot(movies, aes(y=imdb_score, x=get(nonlinear[i])))
    scatter = geom_point()
    print(plot+scatter+eq_spline+knots_lines+xlab(nonlinear[i]))
    spline = lm(imdb_score~bs(get(nonlinear[i]), knots=knots[[i]], degree=j))
    test = glm(imdb_score~bs(get(nonlinear[i]), knots=knots[[i]], degree=j))
    spline_r2_table[i,j] = summary(spline)$adj.r.squared
    spline_mse_table[i,j] = cv.glm(movies, test)$delta[1]
  }
}
spline_r2_table = cbind(nonlinear, spline_r2_table)
spline_mse_table = cbind(nonlinear, spline_mse_table)

# Polynomial VS splines
poly_mse_table = as.data.frame(matrix(NA, nrow=length(nonlinear), ncol=2))
colnames(poly_mse_table) = c("d=best_degree", "d=best_degree_2")
for (i in 1:length(nonlinear)){
  poly = glm(imdb_score~poly(get(nonlinear[i]), degree=best_degree[i]))
  poly_2 = glm(imdb_score~poly(get(nonlinear[i]), degree=best_degree_2[i]))
  poly_mse_table[i,1] = cv.glm(movies, poly)$delta[1]
  poly_mse_table[i,2] = cv.glm(movies, poly_2)$delta[1]
}
poly_mse_table = cbind(nonlinear, best_degree, best_degree_2, poly_mse_table)

### Qualitative variables
# Simple linear regressions and 4 model issues
reg_qual = vector("list", length(qualvars))
significance_qual = rep(NA, length(qualvars))
anova_qual = rep(NA, length(qualvars)) 
heteroskedasticity_qual = rep(NA, length(qualvars))
outliers_qual = rep(NA, length(qualvars))
for (i in 1:length(qualvars)){
  reg_qual[[i]] = lm(imdb_score~get(qualvars[i]))
  names(reg_qual[[i]]$coefficients) = c("constant", qualvars[i])
  print(summary(reg_qual[[i]]))
  p_value = pf(summary(reg_qual[[i]])$fstatistic[1], summary(reg_qual[[i]])$fstatistic[2], 
               summary(reg_qual[[i]])$fstatistic[3], lower.tail = FALSE)
  if (p_value <= 0.05){ 
    significance_qual[i] = "Significant"
  } else {
    significance_qual[i] = "Not significant"
  }
  anova_test = anova(reg_qual[[i]])
  anova_qual[i] = anova_test["Pr(>F)"][1,1]
  hetero = ncvTest(reg_qual[[i]])
  if (hetero$p < 0.05){ 
    heteroskedasticity_qual[i] = "Yes"
  } else {
    heteroskedasticity_qual[i] = "No"
  }
  outlier = outlierTest(reg_qual[[i]])
  if (outlier$signif == TRUE){
    outliers_qual[i] = paste(names(outlier$rstudent), collapse=", ")
  }
}
recap_qual = cbind(qualvars, significance_qual, anova_qual, heteroskedasticity_qual, outliers_qual)

# Multiple linear regression
reg_qual = lm(imdb_score~distributor_new+maturity_new+director_new+cinematographer_new
              +production_new+actor1_new+actor2_new+actor3_new+country_new+language_new)
summary(reg_qual)
vif(reg_qual)

### Quant-qual variables
# Simple linear regressions and 4 model issues
reg_quant_qual = vector("list", length(quant_qualvars))
significance_quant_qual = rep(NA, length(quant_qualvars))
linear_quant_qual = rep(NA, length(quant_qualvars))
heteroskedasticity_quant_qual = rep(NA, length(quant_qualvars))
outliers_quant_qual = rep(NA, length(quant_qualvars))
for (i in 1:length(quant_qualvars)){
  reg_quant_qual[[i]] = lm(imdb_score~get(quant_qualvars[i]))
  names(reg_quant_qual[[i]]$coefficients) = c("constant", quant_qualvars[i])
  print(summary(reg_quant_qual[[i]]))
  if ((summary(reg_quant_qual[[i]])$coefficients[2,4]) <= 0.05){ 
    significance_quant_qual[i] = "Significant"
  } else {
    significance_quant_qual[i] = "Not significant"
  }
  residual_test = residualPlots(reg_quant_qual[[i]], main=quant_qualvars[i])
  if (residual_test[1,2] <= 0.1){ 
    linear_quant_qual[i] = "Not linear"
  } else {
    linear_quant_qual[i] = "Linear"
  }
  hetero = ncvTest(reg_quant_qual[[i]])
  if (hetero$p < 0.05){ 
    heteroskedasticity_quant_qual[i] = "Yes"
  } else {
    heteroskedasticity_quant_qual[i] = "No"
  }
  outlier = outlierTest(reg_quant_qual[[i]])
  if (outlier$signif == TRUE){
    outliers_quant_qual[i] = paste(names(outlier$rstudent), collapse=", ")
  }
}
recap_quant_qual = cbind(quant_qualvars, significance_quant_qual, linear_quant_qual, heteroskedasticity_quant_qual, outliers_quant_qual)

# Correlation
corr_matrix_quant_qual = cor(movies[,c(44:46)])
round(corr_matrix_quant_qual, 2)
pairs.panels(corr_matrix_quant_qual)
corr_tests_quant_qual = rep(NA, length(quant_qualvars))
for (i in 1:length(quant_qualvars)){
  corr_tests_quant_qual[i] = cor(imdb_score, get(quant_qualvars[i]))
}
corr_results_quant_qual = cbind(quant_qualvars, corr_tests_quant_qual)

# Multiple linear regression
reg_quant_qual = lm(imdb_score~english+USA+color)
summary(reg_quant_qual)
vif(reg_quant_qual)

# Multiple linear regression all categorical variables
reg_qual_2 = lm(imdb_score~distributor_new+maturity_new+director_new+cinematographer_new
              +production_new+actor1_new+actor2_new+actor3_new+english+USA+color)
summary(reg_qual_2)
vif(reg_qual_2)

### Genres
# Simple linear regressions and 4 model issues
reg_genre = vector("list", length(genres))
significance_genre = rep(NA, length(genres))
linear_genre = rep(NA, length(genres))
heteroskedasticity_genre = rep(NA, length(genres))
outliers_genre = rep(NA, length(genres))
for (i in 1:length(genres)){
  reg_genre[[i]] = lm(imdb_score~get(genres[i]))
  names(reg_genre[[i]]$coefficients) = c("constant", genres[i])
  print(summary(reg_genre[[i]]))
  if ((summary(reg_genre[[i]])$coefficients[2,4]) <= 0.05){ 
    significance_genre[i] = "Significant"
  } else {
    significance_genre[i] = "Not significant"
  }
  residual_test = residualPlots(reg_genre[[i]], main=genres[i])
  if (residual_test[1,2] <= 0.1){ 
    linear_genre[i] = "Not linear"
  } else {
    linear_genre[i] = "Linear"
  }
  hetero = ncvTest(reg_genre[[i]])
  if (hetero$p < 0.05){ 
    heteroskedasticity_genre[i] = "Yes"
  } else {
    heteroskedasticity_genre[i] = "No"
  }
  outlier = outlierTest(reg_genre[[i]])
  if (outlier$signif == TRUE){
    outliers_genre[i] = paste(names(outlier$rstudent), collapse=", ")
  }
}
recap_genre = cbind(genres, significance_genre, linear_genre, heteroskedasticity_genre, outliers_genre)

# Correlation
corr_matrix_genre = cor(movies[,c(27:39)])
round(corr_matrix_genre, 2)
pairs.panels(corr_matrix_genre)
corr_tests_genre = rep(NA, length(genres))
for (i in 1:length(genres)){
  corr_tests_genre[i] = cor(imdb_score, get(genres[i]))
}
corr_results_genre = cbind(genres, corr_tests_genre)

# Multiple linear regression
reg_genre = lm(imdb_score~action+adventure+scifi+thriller+musical+romance+western+sport+horror
               +drama+war+animation+crime)
summary(reg_genre)
vif(reg_genre)

### Full multiple linear regression
mreg = lm(imdb_score~movie_budget+release_day
          +release_year+release_month_num+duration+aspect_ratio+nb_news_articles
          +actor1_star_meter+actor2_star_meter+actor3_star_meter+nb_faces+movie_meter_IMDBpro
          +distributor_new+maturity_new+director_new+cinematographer_new+production_new+actor1_new
          +actor2_new+actor3_new+country_new+language_new+color+action+adventure+scifi+thriller
          +musical+romance+western+sport+horror+drama+war+animation+crime)
summary(mreg)
vif(mreg)

mreg_2 = lm(imdb_score~movie_budget+release_day
          +release_year+release_month_num+duration+aspect_ratio+nb_news_articles
          +actor1_star_meter+actor2_star_meter+actor3_star_meter+nb_faces+movie_meter_IMDBpro
          +distributor_new+maturity_new+director_new+cinematographer_new+production_new+actor1_new
          +actor2_new+actor3_new+USA+english+color+action+adventure+scifi+thriller
          +musical+romance+western+sport+horror+drama+war+animation+crime)
summary(mreg_2)
vif(mreg_2)

### Remove outliers that appear a lot
movies = movies[-c(989,1581),]
attach(movies)

### Testing models
# Significant variables
k1_movie_meter = quantile(movie_meter_IMDBpro, 1/6)
k2_movie_meter = quantile(movie_meter_IMDBpro, 2/6)
k3_movie_meter = quantile(movie_meter_IMDBpro, 3/6)
k4_movie_meter = quantile(movie_meter_IMDBpro, 4/6)
k5_movie_meter = quantile(movie_meter_IMDBpro, 5/6)
knots_movie_meter = c(k1_movie_meter, k2_movie_meter, k3_movie_meter, k4_movie_meter, k5_movie_meter)
k1_nb_faces = quantile(nb_faces, 1/4)
k2_nb_faces = quantile(nb_faces, 2/4)
k3_nb_faces = quantile(nb_faces, 3/4)
knots_nb_faces = c(k1_nb_faces, k2_nb_faces, k3_nb_faces)
k1_month_num = quantile(release_month_num, 1/3)
k2_month_num = quantile(release_month_num, 2/3)
knots_month_num = c(k1_month_num, k2_month_num)

variables = c(
  "bs(duration, knots=knots_duration, degree=1)", "drama", "bs(nb_news_articles, knots=knots_articles, degree=1)",
  "poly(release_year, 3)", "horror", "color", "action", "war", "english", "USA", "scifi", 
  "bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5)", "bs(nb_faces, knots=knots_nb_faces, degree=2)",
  "thriller", "movie_budget", "adventure", "western", "bs(release_month_num, knots=knots_month_num, degree=1)",
  "crime", "sport", "director_new", "cinematographer_new", "maturity_new", "production_new", "distributor_new", "actor1_new",
  "actor2_new"
)

model_formula = "imdb_score ~ bs(duration, knots=knots_duration, degree=1)"

fit = glm(as.formula(model_formula), data=movies)
MSE_LOOCV = cv.glm(movies, fit)$delta[1]

for (i in 2:length(variables)){
  new_formula = paste(model_formula, "+", variables[i]) 
  fit_test = glm(as.formula(new_formula), data=movies)
  MSE_LOOCV_test = cv.glm(movies, fit_test)$delta[1]
  
  if (MSE_LOOCV_test < MSE_LOOCV){
    MSE_LOOCV = MSE_LOOCV_test
    model_formula = new_formula 
  }
}

# Not significant variables
k1_ratio = quantile(aspect_ratio, 1/4)
k2_ratio = quantile(aspect_ratio, 2/4)
k3_ratio = quantile(aspect_ratio, 3/4)
knots_ratio = c(k1_ratio, k2_ratio, k3_ratio)
variables_nonsignif = c(
  "actor2_star_meter", "actor1_star_meter", "musical", "release_day", "animation", "romance",
  "bs(aspect_ratio, knots=knots_ratio, degree=1)", "bs(actor3_star_meter, knots=knots_actor3, degree=3)",
  "actor3_new"
)

for (i in 1:length(variables_nonsignif)){
  new_formula = paste(model_formula, "+", variables_nonsignif[i]) 
  fit_test = glm(as.formula(new_formula), data=movies)
  MSE_LOOCV_test = cv.glm(movies, fit_test)$delta[1]
  
  if (MSE_LOOCV_test < MSE_LOOCV){
    MSE_LOOCV = MSE_LOOCV_test
    model_formula = new_formula 
  }
}

model_formula

test_1 = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
            + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
            + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
            + bs(nb_faces, knots=knots_nb_faces, degree=2) + movie_budget + western 
            + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
            + maturity_new + musical + animation + romance + actor3_new)

vif(test_1)

test_2 = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
            + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
            + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
            + bs(nb_faces, knots=knots_nb_faces, degree=1) + movie_budget + western 
            + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
            + maturity_new + musical + animation + romance + actor3_new)

vif(test_2)

test_3 = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
            + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
            + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
            + movie_budget + western 
            + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
            + maturity_new + musical + animation + romance + actor3_new)

vif(test_3)

fit_test_3 = glm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                 + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                 + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                 + movie_budget + western 
                 + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
                 + maturity_new + musical + animation + romance + actor3_new, data=movies)
MSE_LOOCV_test_3 = cv.glm(movies, fit_test_3)$delta[1]

test_4 = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
            + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
            + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
            + nb_faces + movie_budget + western 
            + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
            + maturity_new + musical + animation + romance + actor3_new)

vif(test_4)

fit_test_4 = glm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                 + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                 + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                 + nb_faces + movie_budget + western 
                 + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
                 + maturity_new + musical + animation + romance + actor3_new, data=movies)
MSE_LOOCV_test_4 = cv.glm(movies, fit_test_4)$delta[1]

test_5 = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
            + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
            + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
            + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
            + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
            + maturity_new + musical + animation + romance + actor3_new)

vif(test_5)

fit_test_5 = glm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                 + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                 + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                 + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
                 + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
                 + maturity_new + musical + animation + romance + actor3_new, data=movies)
MSE_LOOCV_test_5 = cv.glm(movies, fit_test_5)$delta[1]

# Check that correcting the model did not impact the usefulness of predictors
variables_test_6 = c(
  "bs(duration, knots=knots_duration, degree=1)", "drama", "bs(nb_news_articles, knots=knots_articles, degree=1)",
  "poly(release_year, 3)", "horror", "color", "action", "war", "english", "USA", 
  "bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5)", "bs(nb_faces, knots=knots_faces, degree=2)",
  "movie_budget", "western", "bs(release_month_num, knots=knots_month_num, degree=1)",
  "crime", "sport", "director_new", "maturity_new", "musical", "animation", "romance", "actor3_new"
)

model_formula_test_6 = "imdb_score ~ bs(duration, knots=knots_duration, degree=1)"

fit_model_test_6 = glm(as.formula(model_formula_test_6), data=movies)
MSE_LOOCV_model_test_6 = cv.glm(movies, fit_model_test_6)$delta[1]

for (i in 2:length(variables_test_6)){
  new_formula = paste(model_formula_test_6, "+", variables_test_6[i]) 
  fit_test = glm(as.formula(new_formula), data=movies)
  MSE_LOOCV_test = cv.glm(movies, fit_test)$delta[1]
  
  if (MSE_LOOCV_test < MSE_LOOCV_model_test_6){
    MSE_LOOCV_model_test_6 = MSE_LOOCV_test
    model_formula_test_6 = new_formula 
  }
}

not_variables = c("scifi", "thriller", "adventure", "cinematographer_new", "production_new", 
                  "distributor_new", "actor1_new", "actor2_new", "actor2_star_meter", "actor1_star_meter", 
                  "release_day", "bs(aspect_ratio, knots=knots_ratio, degree=1)", 
                  "bs(actor3_star_meter, knots=knots_actor3, degree=3)"
)

for (i in 1:length(not_variables)){
  new_formula = paste(model_formula_test_6, "+", not_variables[i]) 
  fit_test = glm(as.formula(new_formula), data=movies)
  MSE_LOOCV_test = cv.glm(movies, fit_test)$delta[1]
  
  if (MSE_LOOCV_test < MSE_LOOCV_model_test_6){
    MSE_LOOCV_model_test_6 = MSE_LOOCV_test
    model_formula_test_6 = new_formula 
  }
}

test_6 = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
            + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
            + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
            + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
            + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
            + maturity_new + musical + animation + romance + actor3_new)
vif(test_6)

# Testing interaction terms (it takes forever to run, and adding the interaction terms did improve 
# the MSE of the model slightly but created problems with the vif test and we needed a lot of them for a rather
# small so we decided not to include them to avoid collinearity and overfitting)
model_predictors = c("duration", "drama", "nb_news_articles", "release_year", "horror", "color", "action",
                     "war", "english", "USA", "movie_meter_IMDBpro", "nb_faces", "movie_budget", "western",
                     "release_month_num", "crime", "sport", "director_new", "maturity_new", "musical", "animation",
                     "romance", "actor3_new")

for (i in 1:length(model_predictors)){
  for (j in 1:length(model_predictors)){
    if (model_predictors[i] != model_predictors[j]){
      new_formula = paste(model_formula_test_6, "+", model_predictors[i], "*", model_predictors[j]) 
      fit_test = glm(as.formula(new_formula), data=movies)
      MSE_LOOCV_test = cv.glm(movies, fit_test)$delta[1]
      
      if (MSE_LOOCV_test < MSE_LOOCV_model_test_6){
        MSE_LOOCV_model_test_6 = MSE_LOOCV_test
        model_formula_test_6 = new_formula 
      }
    }
  }
}

model = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                 + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                 + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                 + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
                 + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
                 + maturity_new + musical + animation + romance + actor3_new)

outlierTest(model)
vif(model)
ncvTest(model)
model_adjusted = coeftest(model, vcov=vcovHC(model, type="HC1"))

fit_model = glm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                + action + war + english + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
                + bs(release_month_num, knots=knots_month_num, degree=1) + crime + sport + director_new 
                + maturity_new + musical + animation + romance + actor3_new , data=movies)
MSE_LOOCV_model = cv.glm(movies, fit_model)$delta[1]

# Final model
final_model = lm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                + action + war + language_new + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
                + poly(release_month_num,2) + crime + sport + director_new 
                + maturity_new + musical + animation + romance + actor3_new)

outlierTest(final_model)
movies = movies[-c(1804,395,191,316,1122,599,1590,1435),]
vif(final_model)
ncvTest(final_model)
stargazer(final_model, type="html")
final_model_adjusted = coeftest(final_model, vcov=vcovHC(final_model, type="HC1"))

fit_final_model = glm(imdb_score ~ bs(duration, knots=knots_duration, degree=1) + drama 
                     + bs(nb_news_articles, knots=knots_articles, degree=1) + poly(release_year, 3) + horror + color 
                     + action + war + language_new + USA + bs(movie_meter_IMDBpro, knots=knots_movie_meter, degree=5) 
                     + bs(nb_faces, knots=knots_faces, degree=2) + movie_budget + western 
                     + poly(release_month_num,2) + crime + sport + director_new 
                     + maturity_new + musical + animation + romance + actor3_new , data=movies)
MSE_LOOCV_final_model = cv.glm(movies, fit_final_model)$delta[1]

### Predictions
test_data$release_month_num = match(test_data$release_month, month.abb)
test_data$english = ifelse(test_data$language == "English", 1, 0)
test_data$USA = ifelse(test_data$country == "USA", 1, 0)
test_data$color = ifelse(test_data$colour_film == "Color", 1, 0)
attach(test_data)

distributor_counts = table(distributor)
test_data$distributor_new = ifelse(distributor %in% names(distributor_counts[distributor_counts > 5]), 
                                   distributor, "Other")
maturity_counts = table(maturity_rating)
test_data$maturity_new = ifelse(maturity_rating %in% names(maturity_counts[maturity_counts > 5]), 
                                maturity_rating, "Other")
director_counts = table(director)
test_data$director_new = ifelse(director %in% names(director_counts[director_counts > 9]), 
                                director, "Other")
cinematographer_counts = table(cinematographer)
test_data$cinematographer_new = ifelse(cinematographer %in% names(cinematographer_counts[cinematographer_counts > 5]), 
                                       cinematographer, "Other")
production_counts = table(production_company)
test_data$production_new = ifelse(production_company %in% names(production_counts[production_counts > 5]), 
                                  production_company, "Other")
actor1_counts = table(actor1)
test_data$actor1_new = ifelse(actor1 %in% names(actor1_counts[actor1_counts > 5]), 
                              actor1, "Other")
actor2_counts = table(actor2)
test_data$actor2_new = ifelse(actor2 %in% names(actor2_counts[actor2_counts > 5]), 
                              actor2, "Other")
actor3_counts = table(actor3)
test_data$actor3_new = ifelse(actor3 %in% names(actor3_counts[actor3_counts > 5]), 
                              actor3, "Other")
country_counts = table(country)
test_data$country_new = ifelse(country %in% names(country_counts[country_counts > 5]), 
                               country, "Other")
language_counts = table(language)
test_data$language_new = ifelse(language %in% names(language_counts[language_counts > 5]), 
                                language, "Other")

### Put every new categorical variable as a factor
for (i in 47:56){
  test_data[,i] = as.factor(test_data[,i])
}
test_data$maturity_rating = as.factor(test_data$maturity_rating)
attach(test_data)
predictions = predict(final_model, test_data)
predictions = cbind(test_data[,1], predictions)



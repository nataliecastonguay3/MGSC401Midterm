### Hey Magali! Sorry, I don't know how useful my code will be! 
### I'm sure everyone else's analysis is far better than mine, so 
### feel free to include as little or as much of it as you like in the 
### final code. Thanks again for amalgamating everyone's work! :)
  
movies = read_excel("C:/Users/salol/Downloads/IMDB_data_modified.xlsx")
attach(movies) 

### Step 2&3: Explore Variables Individually & Relationships
  
# Quantitative Variables 
library(ggplot2)
require(methods)

# movie_budget
plot=ggplot(movies, aes(y=imdb_score, x=movie_budget)) 
scatter = geom_point(color="blue") 
line = geom_smooth(method="lm", formula=y~x) 
plot + scatter + line
hist(movie_budget)
boxplot(movie_budget)
reg_budget = lm(movies$imdb_score~movie_budget)
summary(reg_budget) 
outlierTest(reg_budget)

# release_day
plot=ggplot(movies, aes(y=imdb_score, x=release_day)) 
scatter= geom_point(color="green") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(release_day)
boxplot(release_day)
reg_day = lm(movies$imdb_score~release_day)
summary(reg_day)
outlierTest(reg_day)

# release_year
plot=ggplot(movies, aes(y=imdb_score, x=release_year)) 
scatter= geom_point(color="red") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(release_year) 
boxplot(release_year) 
reg_year = lm(movies$imdb_score~release_year)
summary(reg_year)
outlierTest(reg_year)

# duration
plot=ggplot(movies, aes(y=imdb_score, x=duration)) 
scatter= geom_point(color="pink") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(duration)
boxplot(duration) 
reg_duration = lm(movies$imdb_score~movies$duration)
summary(reg_duration)
outlierTest(reg_duration)

# aspect_ratio
plot=ggplot(movies, aes(y=imdb_score, x=aspect_ratio)) 
scatter= geom_point(color="lightblue") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(aspect_ratio)
boxplot(aspect_ratio)
reg_aspect = lm(movies$imdb_score~aspect_ratio)
summary(reg_aspect)
outlierTest(reg_aspect)

# nb_news_articles
plot=ggplot(movies, aes(y=imdb_score, x=nb_news_articles)) 
scatter= geom_point(color="purple") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(nb_news_articles)
boxplot(nb_news_articles)
reg_news = lm(movies$imdb_score~nb_news_articles)
summary(reg_news)
outlierTest(reg_news)

# actor1_star_meter
plot=ggplot(movies, aes(y=imdb_score, x=actor1_star_meter)) 
scatter= geom_point(color="lightgreen") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(actor1_star_meter)
boxplot(actor1_star_meter) 
reg_actor1 = lm(movies$imdb_score~actor1_star_meter)
summary(reg_actor1)
outlierTest(reg_actor1)

# actor2_star_meter
plot=ggplot(movies, aes(y=imdb_score, x=actor2_star_meter)) 
scatter= geom_point(color="yellow") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(actor2_star_meter)
boxplot(actor2_star_meter)
reg_actor2 = lm(movies$imdb_score~actor2_star_meter)
summary(reg_actor2)
outlierTest(reg_actor2)

# actor3_star_meter
plot=ggplot(movies, aes(y=imdb_score, x=actor3_star_meter)) 
scatter= geom_point(color="orange") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(actor3_star_meter)
boxplot(actor3_star_meter)
reg_actor3 = lm(movies$imdb_score~actor3_star_meter)
summary(reg_actor3)
outlierTest(reg_actor3)

# nb_faces
plot=ggplot(movies, aes(y=imdb_score, x=nb_faces)) 
scatter= geom_point(color="darkgreen") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(nb_faces)
boxplot(nb_faces) 
reg_faces = lm(movies$imdb_score~nb_faces)
summary(reg_faces)
outlierTest(reg_faces)

# movie_meter_IMDBpro
plot=ggplot(movies, aes(y=imdb_score, x=movie_meter_IMDBpro)) 
scatter= geom_point(color="darkblue") 
line= geom_smooth(method="lm", formula=y~x) 
plot+scatter+line
hist(movie_meter_IMDBpro)
boxplot(movie_meter_IMDBpro) 
reg_IMDBpro = lm(movies$imdb_score~movie_meter_IMDBpro)
summary(reg_IMDBpro)
outlierTest(reg_IMDBpro)

# Quantitative - Multiple Linear Regression

reg_quant = lm(movies$imdb_score~movie_budget+release_day
               +release_year+movies$duration+aspect_ratio+nb_news_articles
               +actor1_star_meter+actor2_star_meter+actor3_star_meter
               +nb_faces+movie_meter_IMDBpro)
summary(reg_quant)

# Heteroskedasticity
residualPlot(reg_quant)
ncvTest(reg_quant) # p 0.0034094 < 0.05 --> heteroskedasticity

require(lmtest)
require(plm)
coeftest(reg_quant,vcov=vcovHC(reg_quant, type="HC1"))

# Outliers 
outlierTest(reg_quant) 

reg_quant_adjusted = lm(movies$imdb_score~movie_budget
                        +release_year+duration+nb_news_articles
                        +nb_faces+movie_meter_IMDBpro)
summary(reg_quant_adjusted)

# Heteroskedasticity
residualPlot(reg_quant_adjusted)
ncvTest(reg_quant_adjusted) # p 0.003551 < 0.05 --> heteroskedasticity

require(lmtest)
require(plm)
coeftest(reg_quant_adjusted,vcov=vcovHC(reg_quant_adjusted, type="HC1"))

# Outliers 
outlierTest(reg_quant_adjusted) 

# Correlation Matrix 
require(psych)
quantvars = movies[, c(4,5,8,9,15,25,40)]
corr_matrix=cor(quantvars)
round(corr_matrix,3)
pairs.panels(corr_matrix) # no values greater than 0.8, no collinearity 
vif(reg_quant_adjusted) # no values greater than 3, no collinearity

# Qualitative Variables

# Production Company
movies$production_company = as.factor(movies$production_company)
levels(movies$production_company)
table(movies$production_company)
reg2 = lm(imdb_score~production_company)
summary(reg2) 
residualPlot(reg2)

plot=ggplot(movies, aes(y=imdb_score, x=production_company))
scatter= geom_point(color="cyan")
plot+scatter

# Director
movies$director = as.factor(movies$director)
levels(movies$director)
table(movies$director)
reg3 = lm(imdb_score~director)
summary(reg3) 
residualPlot(reg3)
 
plot=ggplot(movies, aes(y=imdb_score, x=director))
scatter= geom_point(color="lightgreen")
plot+scatter

# Maturity Rating
movies$maturity_rating = as.factor(movies$maturity_rating)
levels(movies$maturity_rating)
table(movies$maturity_rating)
reg4 = lm(imdb_score~maturity_rating)
summary(reg4)
residualPlot(reg4)

plot=ggplot(movies, aes(y=imdb_score, x=maturity_rating))
scatter= geom_point(color="darkorange")
plot+scatter

# Genres
reg_action = lm(imdb_score~action)
summary(reg_action)

reg_adventure = lm(imdb_score~adventure)
summary(reg_adventure)

reg_sf = lm(imdb_score~scifi)
summary(reg_sf)

reg_thriller = lm(imdb_score~thriller)
summary(reg_thriller)

reg_musical = lm(imdb_score~musical)
summary(reg_musical)

reg_romance = lm(imdb_score~romance)
summary(reg_romance)

reg_western = lm(imdb_score~western)
summary(reg_western)

reg_sport = lm(imdb_score~sport)
summary(reg_sport)
  
reg_horror = lm(imdb_score~horror)
summary(reg_horror)

reg_drama = lm(imdb_score~drama)
summary(reg_drama)

reg_war = lm(imdb_score~war)
summary(reg_war)

reg_animation = lm(imdb_score~animation)
summary(reg_animation)

reg_crime = lm(imdb_score~crime)
summary(reg_crime)

# Release Month
as.integer(factor(movies$release_month, levels = month.abb))
table(movies$release_month)
reg6 = lm(imdb_score~release_month)
summary(reg6) 
residualPlot(reg6)

plot=ggplot(movies, aes(y=imdb_score, x=release_month))
scatter= geom_point(color="brown")
plot+scatter

# Language 
movies$language = as.factor(movies$language)
levels(movies$language)
table(movies$language)
reg7 = lm(imdb_score~language)
summary(reg7) 
residualPlot(reg7)

plot=ggplot(movies, aes(y=imdb_score, x=language))
scatter= geom_point(color="turquoise")
plot+scatter
 
# Country
movies$country = as.factor(movies$country)
levels(movies$country)
table(movies$country)
reg8 = lm(imdb_score~country)
summary(reg8)
residualPlot(reg8)

plot=ggplot(movies, aes(y=imdb_score, x=country))
scatter= geom_point(color="orchid")
plot+scatter

# Distributor
movies$distributor = as.factor(movies$distributor)
levels(movies$distributor)
table(movies$distributor)
reg9 = lm(imdb_score~distributor)
summary(reg9) 
residualPlot(reg9)

plot=ggplot(movies, aes(y=imdb_score, x=distributor))
scatter= geom_point(color="aquamarine")
plot+scatter

# actor1 
movies$actor1 = as.factor(movies$actor1)
levels(movies$actor1)
table(movies$actor1)
reg10 = lm(imdb_score~actor1)
summary(reg10) 
residualPlot(reg10)

plot=ggplot(movies, aes(y=imdb_score, x=actor1))
scatter= geom_point(color="darkmagenta")
plot+scatter

# actor2
movies$actor2 = as.factor(movies$actor2)
levels(movies$actor2)
table(movies$actor2)
reg11 = lm(imdb_score~actor2)
summary(reg11) 
residualPlot(reg11)

plot=ggplot(movies, aes(y=imdb_score, x=actor2))
scatter= geom_point(color="coral")
plot+scatter

# actor3
movies$actor3 = as.factor(movies$actor3)
levels(movies$actor3)
table(movies$actor3)
reg12 = lm(imdb_score~actor3)
summary(reg12)
residualPlot(reg12)

plot=ggplot(movies, aes(y=imdb_score, x=actor3))
scatter= geom_point(color="cadetblue")
plot+scatter

# colour_film
movies$colour_film = as.factor(movies$colour_film)
levels(movies$colour_film)
table(movies$colour_film)
reg13 = lm(imdb_score~colour_film)
summary(reg13) 
residualPlot(reg13)

plot=ggplot(movies, aes(y=imdb_score, x=colour_film))
scatter= geom_point(color="cornflowerblue")
plot+scatter

# cinematographer
movies$cinematographer = as.factor(movies$cinematographer)
levels(movies$cinematographer)
table(movies$cinematographer)
reg14 = lm(imdb_score~cinematographer)
summary(reg14)
residualPlot(reg14)

plot=ggplot(movies, aes(y=imdb_score, x=cinematographer))
scatter= geom_point(color="darkseagreen") 
plot+scatter 

# Qualitative - Multiple Linear Regression

reg_qualit = lm(movies$imdb_score~production_company
                +director+maturity_rating+release_month
                +country+distributor+actor1+actor2+colour_film
                +cinematographer+action+scifi+thriller+horror+drama+war)
summary(reg_qualit) # 0.9628

# Heteroskedasticity
library("car")
residualPlots(reg_qualit)
residualPlot(reg_qualit)
ncvTest(reg_qualit) 

require(lmtest)
require(plm)
coeftest(reg_qualit,vcov=vcovHC(reg_qualit, type="HC1"))

# Outliers 
outlierTest(reg_qualit) 

### Step 4. Test Non-Linearity & Fit

# Polynomial Degree for each Quantitative Variable

# Movie Budget
reg_budget1 = lm(imdb_score~movie_budget) # best
reg_budget2 = lm(imdb_score~poly(movie_budget,2))
reg_budget3 = lm(imdb_score~poly(movie_budget,3))
reg_budget4 = lm(imdb_score~poly(movie_budget,4))
reg_budget5 = lm(imdb_score~poly(movie_budget,5))
anova(reg_budget1, reg_budget2, reg_budget3, reg_budget4, reg_budget5)

# Release Year
reg_year1 = lm(imdb_score~release_year)
reg_year2 = lm(imdb_score~poly(release_year,2))
reg_year3 = lm(imdb_score~poly(release_year,3)) # best
reg_year4 = lm(imdb_score~poly(release_year,4))
reg_year5 = lm(imdb_score~poly(release_year,5))
anova(reg_year1, reg_year2, reg_year3, reg_year4, reg_year5)

# Nb Faces
reg_faces1 = lm(imdb_score~nb_faces) # best
reg_faces2 = lm(imdb_score~poly(nb_faces,2))
reg_faces3 = lm(imdb_score~poly(nb_faces,3))
reg_faces4 = lm(imdb_score~poly(nb_faces,4))
reg_faces5 = lm(imdb_score~poly(nb_faces,5))
anova(reg_faces1, reg_faces2, reg_faces3, reg_faces4, reg_faces5)

# Duration
reg_duration1 = lm(imdb_score~duration)
reg_duration2 = lm(imdb_score~poly(duration,2))
reg_duration3 = lm(imdb_score~poly(duration,3))
reg_duration4 = lm(imdb_score~poly(duration,4))
reg_duration5 = lm(imdb_score~poly(duration,5)) # best
anova(reg_duration1, reg_duration2, reg_duration3, reg_duration4, reg_duration5)

# Nb News Articles
reg_news1 = lm(imdb_score~nb_news_articles)
reg_news2 = lm(imdb_score~poly(nb_news_articles,2))
reg_news3 = lm(imdb_score~poly(nb_news_articles,3))
reg_news4 = lm(imdb_score~poly(nb_news_articles,4))
reg_news5 = lm(imdb_score~poly(nb_news_articles,5)) # best 
anova(reg_news1, reg_news2, reg_news3, reg_news4, reg_news5)

# IMDB Pro
reg_IMDBpro1 = lm(imdb_score~movie_meter_IMDBpro)
reg_IMDBpro2 = lm(imdb_score~poly(movie_meter_IMDBpro,2))
reg_IMDBpro3 = lm(imdb_score~poly(movie_meter_IMDBpro,3))
reg_IMDBpro4 = lm(imdb_score~poly(movie_meter_IMDBpro,4))
reg_IMDBpro5 = lm(imdb_score~poly(movie_meter_IMDBpro,5)) # best
anova(reg_IMDBpro1, reg_IMDBpro2, reg_IMDBpro3, reg_IMDBpro4, reg_IMDBpro5)

### Building a Regression Model

# Quantitative Model - Significant Variables
reg_quant_adjusted = lm(movies$imdb_score~movie_budget
                        +release_year+movies$duration+nb_news_articles
                        +nb_faces+movie_meter_IMDBpro)
summary(reg_quant_adjusted) # 0.2455

reg_quant_poly = lm(movies$imdb_score~movie_budget+poly(release_year,3)
              +poly(movies$duration,5)+poly(nb_news_articles,5)+nb_faces
              +poly(movie_meter_IMDBpro,5))
summary(reg_quant_poly) # 0.3682

anova(reg_quant_adjusted, reg_quant_poly) # reg_quant_poly is better

# Adding Qualitative Variables

# Production
reg_quant_production = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                          +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                          +poly(movie_meter_IMDBpro,5)+production_company)
summary(reg_quant_production) # Adjusted R-squared: 0.518
anova(reg_quant_poly, reg_quant_production) 

# Director 
reg_quant_director = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                          +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                          +poly(movie_meter_IMDBpro,5)+director)
summary(reg_quant_director) # Adjusted R-squared: 0.6609
anova(reg_quant_poly, reg_quant_director) 

# Rating 
reg_quant_rating = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+maturity_rating)
summary(reg_quant_rating) # Adjusted R-squared: 0.3744
anova(reg_quant_poly, reg_quant_rating)

# Release Month
reg_quant_month = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+release_month)
summary(reg_quant_month) # Adjusted R-squared: 0.3731
anova(reg_quant_poly, reg_quant_month) 

# Country 
reg_quant_country = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                        +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                        +poly(movie_meter_IMDBpro,5)+country)
summary(reg_quant_country) # Adjusted R-squared: 0.3777
anova(reg_quant_poly, reg_quant_country) 

# Distributor
reg_quant_distributor = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                       +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                       +poly(movie_meter_IMDBpro,5)+distributor)
summary(reg_quant_distributor) # Adjusted R-squared: 0.4487 
anova(reg_quant_poly, reg_quant_distributor) 

# Actor 1
reg_quant_actor1 = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                           +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                           +poly(movie_meter_IMDBpro,5)+actor1)
summary(reg_quant_actor1) # Adjusted R-squared: 0.5599
anova(reg_quant_poly, reg_quant_actor1) 

# Actor 2
reg_quant_actor2 = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+actor2)
summary(reg_quant_actor2) # Adjusted R-squared: 0.5066
anova(reg_quant_poly, reg_quant_actor2) 

# Colour
reg_quant_colour = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+colour_film)
summary(reg_quant_colour) # Adjusted R-squared: 0.3751 
anova(reg_quant_poly, reg_quant_colour) 

# Cinematographer
reg_quant_cinema = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+cinematographer)
summary(reg_quant_cinema) # Adjusted R-squared: 0.5424 
anova(reg_quant_poly, reg_quant_cinema) 

# Action
reg_quant_action = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+action)
summary(reg_quant_action) # Adjusted R-squared: 0.3814
anova(reg_quant_poly, reg_quant_action) 

# Sci-Fi
reg_quant_scifi = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+scifi)
summary(reg_quant_scifi) # Adjusted R-squared: 0.3728 
anova(reg_quant_poly, reg_quant_scifi) 

# Thriller
reg_quant_thriller = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                     +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                     +poly(movie_meter_IMDBpro,5)+thriller)
summary(reg_quant_thriller) # Adjusted R-squared: 0.3706
anova(reg_quant_poly, reg_quant_thriller) 

# Horror
reg_quant_horror = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                     +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                     +poly(movie_meter_IMDBpro,5)+horror)
summary(reg_quant_horror) # Adjusted R-squared: 0.3932
anova(reg_quant_poly, reg_quant_horror) 

# Drama
reg_quant_drama = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+drama)
summary(reg_quant_drama) # Adjusted R-squared: 0.4111
anova(reg_quant_poly, reg_quant_drama) 

# War  
reg_quant_war = lm(movies$imdb_score~movie_budget+poly(release_year,3)
                     +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                     +poly(movie_meter_IMDBpro,5)+war)
summary(reg_quant_war) # Adjusted R-squared: 0.3694 
anova(reg_quant_poly, reg_quant_war) 

### Model Testing - LOOCV

# Quantitative at optimal degree
reg_optimal = lm(imdb_score~movie_budget+poly(release_year,3)
                +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                +poly(movie_meter_IMDBpro,5), data=movies)
summary(reg_optimal) 

# Quantitative + director
reg_director = lm(imdb_score~movie_budget+poly(release_year,3)
                 +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                 +poly(movie_meter_IMDBpro,5)+director, data=movies)
summary(reg_director) 

# Quantitative + director + actor1
reg_directoractor1 = lm(imdb_score~movie_budget+poly(release_year,3)
                  +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                  +poly(movie_meter_IMDBpro,5)+director+actor1, data=movies)
summary(reg_directoractor1) 

# Quantitative + director + cinematographer
reg_directorcinema = lm(imdb_score~movie_budget+poly(release_year,3)
                        +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                        +poly(movie_meter_IMDBpro,5)+director+cinematographer, data=movies)
summary(reg_directorcinema) 

# Quantitative + actor1
reg_actor1 = lm(imdb_score~movie_budget+poly(release_year,3)
                        +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                        +poly(movie_meter_IMDBpro,5)+actor1, data=movies)
summary(reg_actor1)

# Quantitative + actor1 + cinematographer
reg_actor1cinema = lm(imdb_score~movie_budget+poly(release_year,3)
                +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                +poly(movie_meter_IMDBpro,5)+actor1+cinematographer, data=movies)
summary(reg_actor1cinema)

# Quantitative + cinematographer
reg_cinema = lm(imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+cinematographer, data=movies)
summary(reg_cinema)

# Quantitative + director + actor1 + cinematographer
reg_directoractor1cinema = lm(imdb_score~movie_budget+poly(release_year,3)
                      +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                      +poly(movie_meter_IMDBpro,5)+director+actor1+cinematographer, data=movies)
summary(reg_directoractor1cinema)

# Quantitative + director + actor1 + cinematographer + production 
reg_directoractor1cinemaprod = lm(imdb_score~movie_budget+poly(release_year,3)
                              +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                              +poly(movie_meter_IMDBpro,5)+director+actor1
                              +cinematographer+production_company, data=movies)
summary(reg_directoractor1cinemaprod) # overfitting

# Quantitative + actor1 + cinematographer + production 
reg_actor1cinemaprod = lm(imdb_score~movie_budget+poly(release_year,3)
                                  +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                                  +poly(movie_meter_IMDBpro,5)+actor1
                                  +cinematographer+production_company, data=movies)
summary(reg_actor1cinemaprod) 
 
# Quantitative+actor1+cinematographer+production_company+actor2
reg_actor1cinemaprodactor2 = lm(imdb_score~movie_budget+poly(release_year,3)
                                  +poly(duration,5)+poly(nb_news_articles,5)+nb_faces
                                  +poly(movie_meter_IMDBpro,5)+actor1
                                  +cinematographer+production_company+actor2, data=movies)
summary(reg_actor1cinemaprodactor2) # overfitting

# Removing Outliers
movies2=movies[-c(191,316,395,492,641,1581,1806,1819),]
View(movies2)
attach(movies2)

# Modified Qualitative Variables (only tested threshold of 5)

# Actor 1 
item_count = table(movies2$actor1)
movies2$actor1_m5 = sapply(movies2$actor1, function(x) {
  if (item_count[x] == 5) {
    return(x)
  } else {
    return("Other")
  }
})

# Director
item_count = table(movies2$director)
movies2$director_m2_5 = sapply(movies2$director, function(x) {
  if (item_count[x] == 5) {
    return(x)
  } else {
    return("Other")
  }
})

# Cinematographer
item_count = table(movies2$cinematographer_m)
movies2$cinematographer_m5 = sapply(movies2$cinematographer_m, function(x) {
  if (item_count[x] == 5) {
    return(x)
  } else {
    return("Other")
  }
})

# Production Company 
item_count = table(movies2$production_company)
movies2$production_company_m5 = sapply(movies2$production_company, function(x) {
  if (item_count[x] == 5) {
    return(x)
  } else {
    return("Other")
  }
})

# Actor 2
item_count = table(movies2$actor2)
movies2$actor2_m = sapply(movies2$actor2, function(x) {
  if (item_count[x] == 1) {
    return("Other")
  } else {
    return(x)
  }
})

# Model Testing: Quantitative  

# Duration and News
poly_durationnews = glm(imdb_score~movie_budget+poly(release_year,3)
                           +nb_faces+poly(duration,5)+poly(nb_news_articles,5), data=movies2)
mse = cv.glm(movies2, poly_durationnews)$delta[1]
mse # 0.7986729

poly_durationnews1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                           +poly(duration,5)+poly(nb_news_articles,5), data=movies2)
summary(poly_durationnews1) # 0.3527

# Duration and IMDBpro
poly_durationIMDBpro = glm(imdb_score~movie_budget+poly(release_year,3)
               +nb_faces+poly(duration,5)+poly(movie_meter_IMDBpro,5), data=movies2)
mse = cv.glm(movies2, poly_durationIMDBpro)$delta[1]
mse # 0.8080894

poly_durationIMDBpro1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                           +poly(duration,5)+poly(movie_meter_IMDBpro,5), data=movies2)
summary(poly_durationIMDBpro1) # 0.34

# News and IMDBpro
poly_newsIMDBpro = glm(imdb_score~movie_budget+poly(release_year,3)
                        +nb_faces+poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5), data=movies2)
mse = cv.glm(movies2, poly_newsIMDBpro)$delta[1]
mse # 0.9108118

poly_newsIMDBpro1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                        +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5), data=movies2)
summary(poly_newsIMDBpro1) # 0.239

# Duration, News and IMDBpro
poly_durationnewsIMDBpro = glm(imdb_score~movie_budget+poly(release_year,3)
                       +nb_faces+poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)+poly(duration,5), data=movies2)
mse = cv.glm(movies2, poly_durationnewsIMDBpro)$delta[1]
mse # 0.7614703

poly_durationnewsIMDBpro1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                            +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)+poly(duration,5), data=movies2)
summary(poly_durationnewsIMDBpro1) # 0.3782 

# Model Testing: Adding Qualitative to Quantitative
  # m2: variables sorted into categories of other if they only appear once
  # m5: variables sorted into categories of other if they don't appear 5 times

# Director
model5 = glm(imdb_score~movie_budget+poly(release_year,3)
              +nb_faces+poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
             +poly(duration,5)+director_m2, data=movies2)
mse = cv.glm(movies2, model5)$delta[1]
mse # 0.8202927

model5summary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                  +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
                  +bs(duration,5)+director_m2, data=movies2)
summary(model5summary)# 0.4299

# Director + Actor 1
model6 = glm(imdb_score~movie_budget+poly(release_year,3)
             +nb_faces+poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
             +poly(duration,5)+director_m2+actor1_m, data=movies2)
mse = cv.glm(movies2, model6)$delta[1]
mse

model6summary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                   +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
                   +poly(duration,5)+director_m2+actor1_m, data=movies2)
summary(model6summary) # 0.443 

# Director + Actor 1 + Cinematographer
model7 = glm(imdb_score~movie_budget+poly(release_year,3)
             +nb_faces+poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
             +poly(duration,5)+director_m2+actor1_m+cinematographer_m2, data=movies2)
mse = cv.glm(movies2, model7)$delta[1]
mse

model7summary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                   +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
                   +poly(duration,5)+director_m2+actor1_m+cinematographer_m2, data=movies2)
summary(model7summary) # 0.4672

# Using all significant variables

modelall = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                   +poly(duration,5)+poly(nb_news_articles,5)+poly(movie_meter_IMDBpro,5)
                   +director_m2+actor1_m+cinematographer_m2+production_company_m2
                   +distributor_m+actor2_m+drama+horror+action, data=movies2)
summary(modelall) # 0.4709 

# Creating Splines - Duration, News and IMDB pro
library(splines)

# Duration 

d1 = quantile(movies2$duration,0.5)

d2 = quantile(movies2$duration,0.3333)
d3 = quantile(movies2$duration,0.6666)

d4 = quantile(movies2$duration,0.25)
d5 = quantile(movies2$duration,0.50)
d6 = quantile(movies2$duration,0.75)

d7 = quantile(movies2$duration,0.2)
d8 = quantile(movies2$duration,0.4)
d9 = quantile(movies2$duration,0.6)
d10 = quantile(movies2$duration,0.8)

splined1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
            +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
            +bs(duration,knots=c(d1),5), data=movies2)

splined2 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
             +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
             +bs(duration,knots=c(d2,d3),5), data=movies2)

splined3 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
             +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
             +bs(duration,knots=c(d4,d5,d6),5), data=movies2)

splined4 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(movie_meter_IMDBpro,5)+poly(nb_news_articles,5)
              +bs(duration,knots=c(d7,d8,d9,d10),5), data=movies2)

anova(splined1, splined2, splined3, splined4) # spline 1

# News 

n1 = quantile(nb_news_articles,0.5)

n2 = quantile(nb_news_articles,0.3333)
n3 = quantile(nb_news_articles,0.6666)

n4 = quantile(nb_news_articles,0.25)
n5 = quantile(nb_news_articles,0.50)
n6 = quantile(nb_news_articles,0.75)

n7 = quantile(nb_news_articles,0.2)
n8 = quantile(nb_news_articles,0.4)
n9 = quantile(nb_news_articles,0.6)
n10 = quantile(nb_news_articles,0.8)

splinen1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(movie_meter_IMDBpro,5)+poly(duration,5)
              +bs(nb_news_articles,knots=c(n1),5), data=movies2)

splinen2 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(movie_meter_IMDBpro,5)+poly(duration,5)
              +bs(nb_news_articles,knots=c(n2,n3),5), data=movies2)

splinen3 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(movie_meter_IMDBpro,5)+poly(duration,5)
              +bs(nb_news_articles,knots=c(n4,n5,n6),5), data=movies2)

splinen4 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(movie_meter_IMDBpro,5)+poly(duration,5)
              +bs(nb_news_articles,knots=c(n7,n8,n9,n10),5), data=movies2)

anova(splinen1, splinen3) # spline 1 

# IMDBpro

p1 = quantile(movie_meter_IMDBpro,0.5)

p2 = quantile(movie_meter_IMDBpro,0.3333)
p3 = quantile(movie_meter_IMDBpro,0.6666)

p4 = quantile(movie_meter_IMDBpro,0.25)
p5 = quantile(movie_meter_IMDBpro,0.50)
p6 = quantile(movie_meter_IMDBpro,0.75)

p7 = quantile(movie_meter_IMDBpro,0.2)
p8 = quantile(movie_meter_IMDBpro,0.4)
p9 = quantile(movie_meter_IMDBpro,0.6)
p10 = quantile(movie_meter_IMDBpro,0.8)

splinep1 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(nb_news_articles,5)+poly(duration,5)
              +bs(movie_meter_IMDBpro,knots=c(p1),5), data=movies2)

splinep2 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(nb_news_articles,5)+poly(duration,5)
              +bs(movie_meter_IMDBpro,knots=c(p2,p3),5), data=movies2)

splinep3 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(nb_news_articles,5)+poly(duration,5)
              +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5), data=movies2)

splinep4 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
              +poly(nb_news_articles,5)+poly(duration,5)
              +bs(movie_meter_IMDBpro,knots=c(p7,p8,p9,p10),5), data=movies2)

anova(splinep1, splinep2, splinep3) #  spline 3

# Quantitative (Spline) Model 
splinemodel3 = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                  +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                  +bs(nb_news_articles,knots=c(n1),5)
                  +bs(duration,knots=c(d4,d5,d6),5), data=movies2)
summary(splinemodel3) # 0.3899

splinemodel3mse = glm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                      +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                      +bs(nb_news_articles,knots=c(n1),5)
                      +bs(duration,knots=c(d4,d5,d6),5), data=movies2)
mse = cv.glm(movies2, splinemodel3mse)$delta[1]
mse # 0.7321406 

# Adding Director
splinedirectormse = glm(imdb_score~movie_budget+nb_faces
                           +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                           +poly(release_year,3)
                           +bs(nb_news_articles,knots=c(n1),5)
                           +bs(duration,knots=c(d4,d5,d6),5)
                           +director_m2_5, data=movies2)
mse = cv.glm(movies2, splinedirectormse)$delta[1]
mse # 0.7227958

splinedirectorsummary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                  +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                  +bs(nb_news_articles,knots=c(n1),5)
                  +bs(duration,knots=c(d4,d5,d6),5)+director_m2_5, data=movies2)
summary(splinedirectorsummary) # 0.3954

# Adding Director + Actor1
splinedirectoractor1mse = glm(imdb_score~movie_budget+nb_faces
                        +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                        +poly(release_year,3)
                        +bs(nb_news_articles,knots=c(n1),5)
                        +bs(duration,knots=c(d4,d5,d6),5)
                        +director_m2_5+actor1_m5, data=movies2)
mse = cv.glm(movies2, splinedirectoractor1mse)$delta[1]
mse # 0.7292675

splinedirectoractor1summary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                           +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                           +bs(nb_news_articles,knots=c(n1),5)
                           +bs(duration,knots=c(d4,d5,d6),5)+director_m2_5
                           +actor1_m5, data=movies2)
summary(splinedirectoractor1summary) # 0.3955 

# Adding Director + Actor1 + Cinematographer
splinedirectoractor1cinemamse = glm(imdb_score~movie_budget+nb_faces
                              +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                              +poly(release_year,3)
                              +bs(nb_news_articles,knots=c(n1),5)
                              +bs(duration,knots=c(d4,d5,d6),5)
                              +director_m2_5+actor1_m5
                              +cinematographer_m5, data=movies2)
mse = cv.glm(movies2, splinedirectoractor1cinemamse)$delta[1]
mse # 0.7361195

splinedirectoractor1cinemasummary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                                 +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                                 +bs(nb_news_articles,knots=c(n1),5)
                                 +bs(duration,knots=c(d4,d5,d6),5)
                                 +director_m2_5+actor1_m5
                                 +cinematographer_m5, data=movies2)
summary(splinedirectoractor1cinemasummary) # 0.3906 

# Adding Actor1 + Cinematographer + Production company
splineactor1cinemaprodmse = glm(imdb_score~movie_budget+nb_faces
                                    +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                                    +poly(release_year,3)
                                    +bs(nb_news_articles,knots=c(n1),5)
                                    +bs(duration,knots=c(d4,d5,d6),5)
                                    +actor1_m5
                                    +cinematographer_m5
                                    +production_company_m5, data=movies2)
mse = cv.glm(movies2, splineactor1cinemaprodmse)$delta[1]
mse # 0.7490659

splineactor1cinemaprodsummary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                                       +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                                       +bs(nb_news_articles,knots=c(n1),5)
                                       +bs(duration,knots=c(d4,d5,d6),5)
                                       +actor1_m5
                                       +cinematographer_m5
                                       +production_company_m5, data=movies2)
summary(splineactor1cinemaprodsummary) # 0.3852

# Adding Director + Actor1 + Production_company
splinedirectoractor1prodmse = glm(imdb_score~movie_budget+nb_faces
                              +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                              +poly(release_year,3)
                              +bs(nb_news_articles,knots=c(n1),5)
                              +bs(duration,knots=c(d4,d5,d6),5)
                              +director_m2_5+actor1_m5
                              +production_company_m5, data=movies2)
mse = cv.glm(movies2, splinedirectoractor1prodmse)$delta[1]
mse # 0.7314872

splinedirectoractor1prodsummary = lm(imdb_score~movie_budget+nb_faces+poly(release_year,3)
                                 +bs(movie_meter_IMDBpro,knots=c(p4,p5,p6),5)
                                 +bs(nb_news_articles,knots=c(n1),5)
                                 +bs(duration,knots=c(d4,d5,d6),5)
                                 +director_m2_5
                                 +actor1_m5+production_company_m5, data=movies2)
summary(splinedirectoractor1prodsummary) # 0.3957  
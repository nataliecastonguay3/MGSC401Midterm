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
imdb$production_new = ifelse(production_company %in% names(production_counts[production_counts > 1]), 
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


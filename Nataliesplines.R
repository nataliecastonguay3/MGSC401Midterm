library(splines)
library(boot)

movies = read.csv("C:/Users/ngcas/Downloads/IMDB_data_Winter_2025 (1).csv")
attach(movies)

i = 1

k1= quantile(release_year,.20) 
k2= quantile(release_year,.40) 
k3= quantile(release_year,.60)
k3= quantile(release_year,.80)

reg = lm(imdb_score~bs(release_year,knots=c(k1,k2,k3),degree=i))
print(summary(reg)$r.squared)
reg2 = glm(imdb_score~bs(release_year,knots=c(k1,k2,k3),degree=i))
print(cv.glm(movies,reg2)$delta[1])

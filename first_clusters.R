library(ggplot2)
library(fpc)
library(factoextra)

ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point()



fviz_cluster(dbscan(iris[, -5], eps = 0.4, MinPts = 4), iris[, -5], geom = "point")

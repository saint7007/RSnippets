library(ggplot2)
library(grid)
library(png)
library(gridExtra)



m <- readPNG(system.file("img", "Rlogo.png", package="png"), FALSE)

#w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.2), nrow=dim(m)[1]) #0.2 is alpha
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,2,] * 0.2), nrow=dim(m)[1]) #0.2 is alpha

library(gridExtra)
g1<-qplot(1:10, rnorm(10), geom = "blank") +
  annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf, 
                    rasterGrob(w)) +
  geom_point()

ggsave("C:/Users/S/Desktop/pallabPlot.jpg")




#df <-
# data.frame(
#   key = c("Compliance", "Information Technology"),
##   budget = c(25000, 15000)
#)

#img <- png::readPNG("C:/Users/S/Desktop/pallab.PNG")
#img<- readPNG(system.file("img", "Rlogo.png", package="png"), FALSE)

#rast <- grid::rasterGrob(img, interpolate = T)


#ggplot(data = df) +
# annotation_custom(rast, ymin = 24000, ymax = 27000, xmin = 2) +
# geom_bar(aes(x=key, y = budget), stat = "identity", alpha = 0.8)

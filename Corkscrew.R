library(svgViewR)

n <- 300
x <- seq(length=n, from=0, by=1)
y <- 10*sin(x/10)
z <- 10*cos(x/10)
data2plot <- cbind(x, y, z)

n_inter <- 100

points3da <- array(data2plot, dim = c(dim(data2plot), n_inter))
 
for (iter in 0:(n_inter-1)){
	points3da[, 1, iter] <- points3da[, 1, iter] + iter
	points3da[, 2, iter] <- 10 * (sin(points3da[, 1, iter]/10))
	points3da[, 3, iter] <- 10 * (cos(points3da[, 1, iter]/10))
}
 
svg.new(file = "Corkscrew.html")
svg.points(points3da)
svg_frame <- svg.frame(data2plot)
svg.close()
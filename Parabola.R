library(svgViewR)

n <- 101
x <- seq(length=n, from = -((n-1)/2), by=1)
y <- (x^2)/10
z <- rep(0, n)
data2plot <- cbind(x, y,z)

n_inter <- 10000

points3d <- array(data2plot, dim = c(dim(data2plot), n_inter))

for (iter in 0:(n_inter-1)){
	points3d[, 1, iter] <- points3d[,1, iter] + (iter/100)
	points3d[, 2, iter] <- (((points3d[, 1, iter])^2)/10)
	for (iter_point in 0:n){
		if (isTRUE(points3d[iter_point, 2, iter] > 250)){
			points3d[iter_point, 2, iter] <- 250
			points3d[iter_point, 1, iter] <- 50
		}
	}
}
 
svg.new(file = "Parabola.html")
svg.points(points3d)
svg_frame <- svg.frame(data2plot)
svg.close()
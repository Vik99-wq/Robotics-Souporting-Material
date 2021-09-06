library(svgViewR)

x <- seq(0, 100, by = 1)
y <- seq(0, 100, by = 1)
z <- seq(0, 100, by = 1)
data2plot <- cbind(x,y,z)

velX <- c(0, times = 101)
velY <- c(0, times = 101)
velZ <- c(0, times = 101)

n_iter <- 100

points3d <- array(data2plot, dim=c(dim(data2plot), n_iter))

for(iter in 2:(n_iter)){
	velX <- velX + rnorm(101)
	velY <- velY + rnorm(101)
	velZ <- velZ + rnorm(101)
	points3d[, 1, iter] <- points3d[, 1, (iter-1)] + velX
	points3d[, 2, iter] <- points3d[, 2, (iter-1)] + velY
	points3d[, 3, iter] <- points3d[, 3, (iter-1)] +velZ
}

svg.new(file = "randomWalk.html")
svg.points(points3d)
svg_frame <- svg.frame(data2plot)
svg.close()
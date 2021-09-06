library(svgViewR)

n <- 300
x <- seq(length=n, from=0, by=1)
y <- 10*sin(x/10)
z <- 10*cos(x/10)
data2plot <- cbind(x, y, z)

n_inter <- 100
rows <- 300
collums <- 3

points3da <- array(data2plot, dim = c(rows, collums, n_inter))

for (inter in 0:(n_inter-1)){
	points3da[, , inter] <- points3da[, , inter] * 0.001 * inter^2
}

svg.new(file = "Sin_Cos_Spiral_Fast.html")
svg.points(points3da)
svg_frame <- svg.frame(data2plot)
svg.close()

#Import all the libraries that we will need 
library(svgViewR)
library(network)
library(igraph)
library(visNetwork)
library(magrittr)
library(networkD3)
library(dplyr)
numpoints = 101

#Generate random points
x <- rnorm(numpoints, mean = 50, sd = 35)
y <- rnorm(numpoints, mean = 50, sd = 20)
z <- rnorm(numpoints, mean = 50, sd = 20)

#Bind them into an matrix
data2plot <- cbind(x,y,z)

#Set points initial velocity
velX <- c(0, times = numpoints)
velY <- c(0, times = numpoints)
velZ <- c(0, times = numpoints)

#Set aggressiveness of the correction of points when they get close 
agreement <- 0.1

#Distance from them to me in the three axis
dX <- 0
dY <- 0
dZ <- 0

#Hypotenuse of the distances from them to me
distanceTM <- 0

#The initialization of the starting and ending adjacency matrices
startingNet <- array(0, c(numpoints, numpoints))
endingNet <- array(0, c(numpoints, numpoints))

#Setting the amount of "frames" for the SVGview visualization of flocking
n_iter <- 300

#Taking data2plot and adding the time dimension of n_iter to make it into an array
points3d <- array(data2plot, dim=c(dim(data2plot), n_iter))

#Loop to edit each of the n_iter frames to make it like a movie
#Not starting at 1 because the first frame isall the points at 0, 0, 0
for(iter in 2:(n_iter)){
	#Giving each point a random x, y, and z velocity
	velX <- velX + rnorm(numpoints)
	velY <- velY + rnorm(numpoints)
	velZ <- velZ + rnorm(numpoints)
	
	#Loop through each of the points
	for (me in 1:length(x)){
		#X left boundry condition 
		if(isTRUE(points3d[me, 1, (iter-1)]) > 200){
			velX <- velX - 1
		}
		#X right boundry condition
		if(points3d[me, 1, (iter-1)] < (-200)){
			velX <- velX + 1
		}
		#Y up boundry condition 
		if(isTRUE(points3d[me, 2, (iter-1)]) > 200){
			velY <- velY - 1
		}
		#Y down boundry condition
		if(points3d[me, 2, (iter-1)] < (-200)){
			velY <- velY + 1
		}
		#Z right boundry condition 
		if(isTRUE(points3d[me, 3, (iter-1)]) > 200){
			velZ <- velZ - 1
		}
		#Z left boundry condition
		if(points3d[me, 3, (iter-1)] < (-200)){
			velZ <- velZ + 1
		}
		#Loop through each of the points
		for(them in 1:length(x)){
			#Figure out how far the "me" point is from each of the other "them" points
			dX <- points3d[them, 1, (iter-1)] - points3d[me, 1, (iter-1)]
			dY <- points3d[them, 2, (iter-1)] - points3d[me, 2, (iter-1)]
			dZ <- points3d[them, 3, (iter-1)] - points3d[me, 3, (iter-1)]
			
			#To decresse erraticness and incresse  control if apoint's velocity is over 10 set it to 10
			if (velX[me] > 10){
				velX[me] <- 10
			}
			
			#Calculate hypotenuse of of the distaste from the "me" point to the "them" point in three axis
			distanceTM <- sqrt(dX^2 + dY^2 + dZ^2)
			
			#If the them loop is trying to compare a point to itself, skip that iteration
			if(identical(me, them)){
			
			#If not, continue 	
			}else{
				if(distanceTM>20){
					#Normalize the to be added velocity
					#Then, add some velocity in all axis so the point can catch up to the flock 
					velX[me] <- velX[me] + ((dX/distanceTM)/200)
					velY[me] <- velY[me] + ((dY/distanceTM)/200)
					velZ[me] <- velZ[me] + ((dZ/distanceTM)/200)
			} else if(distanceTM<5){
					#Normalize the to be subtracted velocity
					#Then, subtrat some velocity in all axis so the point won't bump into another point 
					velX[me] <- velX[me] + ((-dX/distanceTM)/200)
					velY[me] <- velY[me] + ((-dY/distanceTM)/200)
					velZ[me] <- velZ[me] + ((-dZ/distanceTM)/200)
			} else{
					#If the point isn't too close or far from the flock
					#Moke it start to agree with the rest of the points/flock
					#So that when they come to the center of the flock from different direction they slowly start going in the same direction
					velX[me] <- velX[me] + (agreement*velX[them]/200)
					velY[me] <- velY[me] + (agreement*velY[them]/200)
					velZ[me] <- velZ[me] + (agreement*velZ[them]/200)
					
					#If they are in the goldelocks zone and agreeing
					#Then, if it is the starting or ending "frame"
					#Add this connection to the adjacency matrix
					if(iter==2){
						startingNet[me, them] <- 1
					}
					if(iter==n_iter){
						endingNet[me, them] <- 1
					}
			}
			}	
		}
	}
	
	#Add the velocity in the three axis to the position of the point, so that the point moves as the frame changes 
	points3d[, 1, iter] <- points3d[, 1, (iter-1)] + velX
	points3d[, 2, iter] <- points3d[, 2, (iter-1)] + velY
	points3d[, 3, iter] <- points3d[, 3, (iter-1)] + velZ
}

#Create the SvgViewR visualization
svg.new(file = "Flocking.html")
svg.points(points3d)
svg_frame <- svg.frame(points3d)
svg.close()

#Create network graph that shows the point locations at the beginning and end of flocking
startNet <- network(startingNet, matrix.type="adjacency", ignore.eval=FALSE)
endNet <- network(endingNet, matrix.type="adjacency", ignore.eval=FALSE)
plot(startNet)
plot(endNet)

#Convert the starting and ending Network arrays into adjacency matrixes
gstart <- graph.adjacency(startingNet)
gend <- graph.adjacency(endingNet)

#Convert the starting and ending adjacency matrixes into edge lists
startedgelist <- data.frame(get.edgelist(gstart))
endedgelist <- data.frame(get.edgelist(gend))

#Create a vector of sequential numbers starting at 1 and going to numpoints
labels = 1:numpoints

#Creating the starting nodes
startnodes <- data.frame(
	#Giving each point its own id and label
	#Data frame with the points ids and labels
	id = c(labels), 
	label = c(labels)
)

#Creating the starting edges
#Data frame with the data for visNetwork and forceNetwork starting visualizations
startedges <- data.frame(
	#Collum with "from" points
	from = c(startedgelist[,"X1"]),
	
	#Collum with "to" points
	to = c(startedgelist[,"X2"]),
	
	#Saying that there won't be any arrow pointing to the "from" point
	arrows.from.type = NA,
	
	#Saying that there will be a arrow pointing to the "to" point
	arrows.to.type = "arrow"
)

#Creating the ending nodes
endnodes <- data.frame(
	#Giving each point its own id and label
	#Data frame with the points ids and labels
	id = c(labels), 
	label = c(labels)
)

#Creating the ending edges
#Data frame with the data for visNetwork and forceNetwork ending visualizations
endedges <- data.frame(
	#Collum with "from" points
	from = c(endedgelist[,"X1"]),
	
	#Collum with "to" points
	to = c(endedgelist[,"X2"]),
	
	#Saying that there won't be any arrow pointing to the "from" point
	arrows.from.type = NA,
	
	#Saying that there will be a arrow pointing to the "to" point
	arrows.to.type = "arrow"
)

#Create the visNetworks that will open in a browser 
visNetwork(startnodes, startedges)

visNetwork(endnodes, endedges)

#Make it so that the nodes and edges shift down 1 so that they start at 0
#We need to do this because forceNetwork needs the passed in data frames to start at 0
nodes_d3start <- mutate(startnodes, id = id - 1)
edges_d3start <- mutate(startedges, from = from - 1, to = to - 1)

nodes_d3end <- mutate(endnodes, id = id - 1)
edges_d3end <- mutate(endedges, from = from - 1, to = to - 1)

#Create the forceNetwork that will open in a browser
forceNetwork(Links=edges_d3start, Nodes=nodes_d3start, Source="from", Target="to", 
             NodeID="label", Group="id", 
             opacity= 1, fontSize= 16, zoom= TRUE)

forceNetwork(Links=edges_d3end, Nodes=nodes_d3end, Source="from", Target="to", 
             NodeID="label", Group="id", 
             opacity= 1, fontSize= 16, zoom= TRUE)

#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  
  lattice_dim <- r*c
  m <- matrix(sample(c(0,1,2), lattice_dim, prob = c(1-p, p/2, p/2), replace = T), nrow = r)
  return(m)
}




#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
north_move <- function(m) {
  if(nrow(m)==1) {
    return(m)
  }
  blocked <- m[c(nrow(m), 1:(nrow(m)-1)),]!=0
  blue_particle <- m*(m==2)
  blue_particle.new <- m*(m!=2) + blue_particle*blocked + (blue_particle*!blocked)[c(2:nrow(m), 1), ] 
  return(blue_particle.new)
}

east_move <- function(m) {
  if(ncol(m)==1) {
    return(m)
  }
  blocked <-  m[, c(2:ncol(m), 1)]!=0
  red_particle <-  m*(m==1)
  red_particle.new <- m*(m!=1) + red_particle*blocked + (red_particle*!blocked)[,c(ncol(m), 1:(ncol(m)-1))] 
  return(red_particle.new)
}


bml.step <- function(m){
  
  
  updated.m <- north_move(east_move(m))
  if (!all(updated.m == m)){
    grid.new <- TRUE
  }
  else{
    grid.new <- FALSE
  }
  return(list(updated.m, grid.new))
}


#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r,c,p)
  for (i in 1:1000) {
    i <- i + 1
    output <- bml.step(m)
    x <- output[[1]]
    y <- output[[2]]
    if (y== FALSE) 
      break}
  print(i-1) #"GridLock at this # of steps"
}


#1
table(replicate(1000,bml.sim(10,10,0.26)))
table(replicate(1000,bml.sim(10,10,0.32)))
table(replicate(1000,bml.sim(10,10,0.38)))
table(replicate(1000,bml.sim(10,10,0.39)))
table(replicate(1000,bml.sim(20,20,0.26)))
table(replicate(1000,bml.sim(20,20,0.32)))
table(replicate(1000,bml.sim(20,20,0.38)))

p0.36 <- replicate(100,bml.sim(10,10,0.36))
p0.45 <- replicate(100,bml.sim(10,10,0.45))
p0.54 <- replicate(100,bml.sim(10,10,0.54))
p0.63 <- replicate(100,bml.sim(10,10,0.63))
p0.72 <- replicate(100,bml.sim(10,10,0.72))
p0.81 <- replicate(100,bml.sim(10,10,0.81))

mean0.36 <- mean(p0.36)
mean0.45 <- mean(p0.45)
mean0.54 <- mean(p0.54)
mean0.63 <- mean(p0.63)
mean0.72 <- mean(p0.72)
mean0.81 <- mean(p0.81)
gg <- c(mean0.36,mean0.45,mean0.54,mean0.63,mean0.72,mean0.81)
plot(gg, type="b", xlab="density", ylab="mean of 100 free flowing traffic sample", 
     main="Relationship between p and mean of 100 free lowing traffic sample",xaxt= 'n')
axis(side=1, at=1, label="0.36")
axis(side=1, at=2, label="0.45")
axis(side=1, at=3, label="0.54")
axis(side=1, at=4, label="0.63")
axis(side=1, at=5, label="0.72")
axis(side=1, at=6, label="0.81")
assign("last.warning", NULL, envir = baseenv())

#2 
aa<- sapply(c((1/1000)*(1:1000)), function(x) bml.sim(10,10,x))
plot(aa, type="l", xlim=c(0,1000), xlab="p" , ylab="max number of step until gridlock", xaxt= 'n', main="relationship between p and max number of step until gridlock")
axis(side=1, at=0, label="0")
axis(side=1, at=200, label="0.2")
axis(side=1, at=400, label="0.4")
axis(side=1, at=600, label="0.6")
axis(side=1, at=800, label="0.8")
axis(side=1, at=1000, label="1.0")
assign("last.warning", NULL, envir = baseenv())

#3
table(replicate(1000,bml.sim(10,10,0.8)))
table(replicate(1000,bml.sim(5,20,0.8)))
table(replicate(1000,bml.sim(4,25,0.8)))
table(replicate(1000,bml.sim(2,50,0.8)))
table(replicate(1000,bml.sim(20,20,0.5)))
table(replicate(1000,bml.sim(10,10,0.5)))
table(replicate(1000,bml.sim(5,5,0.5)))



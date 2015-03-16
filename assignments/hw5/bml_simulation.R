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
table(replicate(1000,bml.sim(10,10,0.90)))
table(replicate(1000,bml.sim(10,10,0.92)))
table(replicate(1000,bml.sim(10,10,0.94)))
table(replicate(1000,bml.sim(10,10,0.96)))
table(replicate(1000,bml.sim(20,20,0.92)))
table(replicate(1000,bml.sim(20,20,0.94)))
table(replicate(1000,bml.sim(20,20,0.96)))

p0.90 <- replicate(100,bml.sim(10,10,0.90))
p0.92 <- replicate(100,bml.sim(10,10,0.92))
p0.94 <- replicate(100,bml.sim(10,10,0.94))
p0.96 <- replicate(100,bml.sim(10,10,0.96))
p0.98 <- replicate(100,bml.sim(10,10,0.98))
p1 <- replicate(100,bml.sim(10,10,1))

mean0.90 <- mean(p0.90)
mean0.92 <- mean(p0.92)
mean0.94 <- mean(p0.94)
mean0.96 <- mean(p0.96)
mean0.98 <- mean(p0.98)
mean1 <- mean(p1)
gg <- c(mean0.36,mean0.45,mean0.54,mean0.63,mean0.72,mean0.81)
plot(gg, type="b", xlab="density", ylab="mean of 100 free flowing traffic sample", 
     main="Relationship between p and mean of 100 free lowing traffic sample",xaxt= 'n')
axis(side=1, at=1, label="0.90")
axis(side=1, at=2, label="0.92")
axis(side=1, at=3, label="0.94")
axis(side=1, at=4, label="0.96")
axis(side=1, at=5, label="0.98")
axis(side=1, at=6, label="1")
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
table(replicate(1000,bml.sim(10,10,0.98)))
table(replicate(1000,bml.sim(5,20,0.98)))
table(replicate(1000,bml.sim(4,25,0.98)))
table(replicate(1000,bml.sim(2,50,0.98)))
table(replicate(1000,bml.sim(20,20,0.96)))
table(replicate(1000,bml.sim(10,10,0.96)))
table(replicate(1000,bml.sim(5,5,0.96)))



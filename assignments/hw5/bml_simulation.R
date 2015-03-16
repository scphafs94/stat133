#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
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
  blue_particle.new <- blue_particle + blue_particle*blocked + (blue_particle*!blocked)[c(2:nrow(m), 1), ] 
  return(blue_particle.new)
}

east_move <- function(m) {
  if(ncol(m)==1) {
    return(m)
  }
  blocked <-  m[, c(2:ncol(m), 1)]!=0
  red_particle <-  m*(m==1)
  red_particle.new <- red_particle + red_particle*blocked + (red_particle*!blocked)[,c(ncol(m), 1:(ncol(m)-1))] 
  return(red_particle.new)
}


bml.step <- function(m){
  
  
  updated.m <- north_move(east_move(m))
  if (!all(updated.m == m)){
    grid.new <- T 
  }
  else{
    grid.new <- F 
  }
  return(list(updated.m, grid.new))
}


#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r,c,p)
  i <- 1
  while (i <= 1000) {
    i <- i + 1
    output <- bml.step(m)
    x <- output[[1]]
    y <- output[[2]]
    if (y== FALSE) break}
  print(i-1) #"GridLock at this # of steps"
}


1. (plot1)
table(replicate(1000,bml.sim(10,10,0.25)))
table(replicate(1000,bml.sim(10,10,0.32)))
table(replicate(1000,bml.sim(10,10,0.38)))
table(replicate(1000,bml.sim(10,10,0.39)))
table(replicate(1000,bml.sim(20,20,0.25)))
table(replicate(1000,bml.sim(20,20,0.32)))
table(replicate(1000,bml.sim(20,20,0.38)))
p032 <- replicate(100,bml.sim(10,10,0.32))
p045 <- replicate(100,bml.sim(10,10,0.45))
p052 <- replicate(100,bml.sim(10,10,0.52))
p065 <- replicate(100,bml.sim(10,10,0.65))
p074 <- replicate(100,bml.sim(10,10,0.74))
p085 <- replicate(100,bml.sim(10,10,0.85))
m.032 <- mean(p032)
m.045 <- mean(p045)
m.052 <- mean(p052)
m.065 <- mean(p065)
m.075 <- mean(p074)
m.085 <- mean(p085)
gg <- c(m.032,m.045,m.052,m.065,m.075,m.085)
plot(gg, type="b", xlab="p", ylab="average of 100 sample's free owning traffic", 
     main="The relationship between p and average of 100 sample's free owning traffic",xaxt= 'n')
axis(side=1, at=1, label="0.32")
axis(side=1, at=2, label="0.45")
axis(side=1, at=3, label="0.52")
axis(side=1, at=4, label="0.65")
axis(side=1, at=5, label="0.74")
axis(side=1, at=6, label="0.85")
assign("last.warning", NULL, envir = baseenv())
##############################################
#2 What is max step? (#plo21)
aa<- sapply(c((1/1000)*(1:1000)), function(x) bml.sim(10,10,x))
plot(aa, type="l", xlim=c(0,1000), xlab="p" , ylab="max number of step until gridlock", xaxt= 'n', main="relationship between p and max number of step until gridlock")
axis(side=1, at=0, lable="0")
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



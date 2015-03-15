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
  while (i <= 20000) {
    i <- i + 1
    output <- bml.step(m)
    x <- output[[1]]
    y <- output[[2]]
    if (y== FALSE) break}
  print(i-1) #"GridLock at this # of steps"
}



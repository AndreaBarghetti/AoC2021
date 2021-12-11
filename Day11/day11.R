library(tidyverse)
library(rlang)

# part 1 ####
input <- read_lines("Day11/input.txt")

octop <- input %>% 
  map(str_split, "", simplify=T) %>%
  map(as.integer) %>% 
  do.call(rbind, .)

charge_energy <- function(matrix) {
 init <- (matrix>9)+0
 byrow <- init %>% 
   apply(1,function(x) {
     x+lead(x,default = 0)+lag(x, default = 0)}) %>% t()
 bycol <- byrow %>% 
   apply(2,function(x) {
     x+lead(x,default = 0)+lag(x, default = 0)})
 return(bycol)
}

spread_energy <- function(matrix=octop) {
  flashed <- matrix>9
  while(sum(flashed)!=0){
    
    env_poke(flash_counter, "flashes", flash_counter$flashes+sum(flashed))
    
    matrix <- matrix + charge_energy(matrix)
    matrix[flashed]<- -1000
    flashed <- matrix>9
  }
  matrix[matrix<0] <- 0
  matrix
}

next_cycle <- function(matrix) {
  matrix <- matrix + 1
  matrix <- spread_energy(matrix)
  return(matrix)
}

flash_counter <- rlang::env(flashes=0)

record_flashes <- function(octopuses, cycles) {
  
  for (i in 1:cycles) {
    octopuses <- next_cycle(octopuses)
  }
  
  r <- flash_counter$flashes
  
  env_poke(flash_counter, "flashes", 0)
  return(r)
}

record_flashes(octop, 100)

# part 2 ####
find_sync <- function(octopuses) {
  i<-0
  while (T) {
    i<-i+1
    octopuses <- next_cycle(octopuses)
    if (all(octopuses==0)) {
      break
    }
  }
  env_poke(flash_counter, "flashes", 0)
  return(i)
}

find_sync(octop)


# animation ####
library(animation)
library(reshape2)

plot_octopuses <- function(matrix) {
  matrix %>% 
    melt() %>% 
    ggplot(aes(x=Var1, y=Var2, fill=value)) +
    scale_fill_viridis_c(option = "magma", direction = 1, limits=c(0,10)) +
    geom_tile(show.legend = F) +
    theme_void() 
} 

plot_octopuses(octop)

film_octopuses <- function(matrix, filename) {
  saveGIF(
    {
      for (i in 1:(find_sync(matrix)+50)) {
        (matrix <- next_cycle(matrix))
        print(plot_octopuses(matrix) +ggtitle(str_c(i)))
      }
    },
    loop=T,
    interval=0.05,
    ani.width = 300, 
    ani.height = 300,
    movie.name = filename, 
    outdir = getwd()
  )
}

film_octopuses(octop, "octopuses.gif")

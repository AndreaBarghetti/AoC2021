library(tidyverse)

# part 1 ####
input <- read_lines("Day6/input.txt")

fishes <- str_split(input,",") %>% unlist() %>% as.integer()

fish_pop <- sapply(0:9, function(i) {sum(fishes==i)}) %>% 
  as.numeric()

next_day <- function(fish_pop) {
  new <- fish_pop[1]
  fish_pop <- c(fish_pop[2:9],new)
  fish_pop[7]<- fish_pop[7]+new
  fish_pop
}

grow_fish <- function(fish_pop, days=80) {
  walk(1:days, function(fish_pop) {fish_pop <- next_day(fish_pop)})
  fish_pop
}

sum(grow_fish2(fish_pop))

# part 2 ####
sum(grow_fish2(fish_pop, 256)) %>% format(scientific=F)

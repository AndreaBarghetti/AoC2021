library(tidyverse)

# part 1 ####
input <- read_lines("Day7/input.txt")

crabs <- str_split(input,",") %>% unlist() %>% as.integer()

sum(abs(crabs - median(crabs)))

# part 2 ####
crab_space <- function(x) {
  map_int(x, function(x) {
    if (x==0) return(x)
    sum(1:x)
  })
}

# just try all positions and find min sum
map_int(min(crabs):max(crabs), function(x) {
  sum(crab_space(abs(crabs-x)))
}) %>% min()

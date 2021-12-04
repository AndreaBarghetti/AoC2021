library(tidyverse)
input <- read_lines("Day1/input.txt") %>% as.numeric()

# part1 ####
sum(input > lag(input), na.rm = T)

# part2 ####
slideinput <- input + lag(input) + lead(input)
sum(slideinput > lag(slideinput), na.rm = T)

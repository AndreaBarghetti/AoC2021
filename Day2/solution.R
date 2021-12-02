library(tidyverse)

# Part 1 ####
input <- read_lines("Day2/input.txt")

get_dir <- function(str) str_extract(str, "[a-z]+")
get_value <- function(str) as.integer(str_extract(str, "[0-9]+"))

go1 <- function(input, x=0,y=0) {
  for (str in input) {
    value = get_value(str)
    if (get_dir(str)=="forward") {x = x + value}
    if (get_dir(str)=="up") {y = y - value}
    if (get_dir(str)=="down") {y = y + value}
  }
  x*y
} 

go1(input)

# Part 2 ####
go2 <- function(input, x=0,y=0,aim=0) {
  for (str in input) {
    value = get_value(str)
    if (get_dir(str)=="up") {aim = aim - value}
    if (get_dir(str)=="down") {aim = aim + value}
    if (get_dir(str)=="forward") {
      x = x + value
      y = y + aim*value
    }
  }
  x*y
} 

go2(input)

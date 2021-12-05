library(tidyverse)

# part 1 ####
input <- read_lines("Day5/input.txt")
  
lines <- map(input,  function(x) {
  str_extract_all(x, "[0-9]+") %>% 
    unlist() %>% as.integer()+1
})

max_x <- max(map(lines, `[`, c(1,3)) %>% unlist())
max_y <- max(map(lines, `[`, c(2,4)) %>% unlist())

space <- matrix(0, nrow = max_y, ncol=max_x)

is_straight <- function(line) {
    line[1]==line[3]|line[2]==line[4]
}
is_verti <- function(line) {
  line[2]==line[4]
}
is_hori <- function(line) {
  line[1]==line[3]
}
straight_lines <- lines[map_lgl(lines, is_straight)]
hori_lines <- straight_lines[map_lgl(straight_lines, is_hori)]
verti_lines <- straight_lines[map_lgl(straight_lines, is_verti)]

add_s_line <- function(matrix, line) {
  matrix[c(line[1]:line[3]),c(line[2]:line[4])] <- matrix[c(line[1]:line[3]),c(line[2]:line[4])]+1
  matrix
}

add_s_lines <- function(matrix, lines) {
  for (line in lines) {
    matrix <- add_s_line(matrix,line)
  }
  matrix
}
sum(add_s_lines(space, straight_lines) >=2)

# part 2 ####
dia_lines <- lines[!map_lgl(lines, is_straight)]

add_d_line <- function(matrix, line) {
  coords <- cbind(c(line[1]:line[3]),c(line[2]:line[4]))
  for (i in 1:nrow(coords)) {
    matrix[coords[i,][1],coords[i,][2]]<-matrix[coords[i,][1],coords[i,][2]]+1
  }
  matrix
}

add_d_lines <- function(matrix, lines) {
  for (line in lines) {
    matrix <- add_d_line(matrix,line)
  }
  matrix
}

sum((add_s_lines(space, straight_lines) %>% 
  add_d_lines(dia_lines))>=2)

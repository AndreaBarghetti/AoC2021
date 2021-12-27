library(tidyverse)

# part 1 ####
input <- read_lines("Day15/input.txt")

matrix <- input %>% 
  map(str_split, "", simplify=T) %>% 
  map(as.integer) %>% 
  do.call(rbind,.)

penalty_matrix <- matrix(Inf,nrow(matrix),ncol(matrix))
# assign last piece
penalty_matrix[nrow(matrix),ncol(matrix)]<-matrix[nrow(matrix),ncol(matrix)]


index_position_df <- cross_df(list(row=1:nrow(matrix),col=1:ncol(matrix))) %>% 
  mutate(index=row_number()) #%>% 
  # mutate(value = map2_dbl(row, col, function(row, col) {matrix[row,col]}),
  #        penalty = map2_dbl(row, col, function(row, col) {penalty_matrix[row,col]}))

find_min_step <- function(row, col, matrix, penalty) {
  left <- penalty[row,] %>% lag(1, default = Inf) %>% pluck(col)
  right <- penalty[row,] %>% lead(1, default = Inf)%>% pluck(col)
  up <- penalty[,col] %>% lag(1, default = Inf) %>% pluck(row)
  down <- penalty[,col] %>% lead(1, default = Inf) %>% pluck(row)
  min <- min(left, right, up, down)
  matrix[row,col]+min
}

#  find_min_step(10,9, matrix, penalty_matrix)
for (i in (sum(dim(matrix))-1):2) {
  diagonal_df <- index_position_df %>% 
    filter(row+col==i)
  indexes <- diagonal_df$index
  replacementes <- diagonal_df %>% 
    pmap_dbl(function(row, col, ...) {find_min_step(row,col, matrix, penalty_matrix)})
  penalty_matrix[indexes] <- replacementes
}

penalty_matrix[1,1]-matrix[1,1]

# part 2 ####
big_matrix <- rbind(matrix, matrix+1,matrix+2,matrix+3,matrix+4) %>% 
  cbind(.,.+1,.+2,.+3,.+4) %% 9
big_matrix[big_matrix==0] <- 9

matrix <- big_matrix

penalty_matrix <- matrix(Inf,nrow(matrix),ncol(matrix))
penalty_matrix[nrow(matrix),ncol(matrix)]<-matrix[nrow(matrix),ncol(matrix)]

index_position_df <- cross_df(list(row=1:nrow(matrix),col=1:ncol(matrix))) %>% 
  mutate(index=row_number()) 

#repeat until stable
for (i in (sum(dim(matrix))-1):2) {
  diagonal_df <- index_position_df %>% 
    filter(row+col==i)
  indexes <- diagonal_df$index
  replacementes <- diagonal_df %>% 
    pmap_dbl(function(row, col, ...) {find_min_step(row,col, matrix, penalty_matrix)})
  penalty_matrix[indexes] <- replacementes
}

penalty_matrix[1,1]-matrix[1,1]


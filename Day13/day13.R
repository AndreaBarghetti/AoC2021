library(tidyverse)


# this solution does not work when there are folds <=2
# to be fixed . avoid that: matrix[1,] returns a vector

# part 1 ####
input <- read_lines("Day13/input.txt")

coords <- input %>% 
  str_subset("^[0-9]+,[0-9]+") %>% 
  str_split(",", simplify = T) %>% 
  apply(2, as.integer)+1

folds <- str_subset(input, "fold") %>% 
  map_chr(str_extract, "[xy]=[0-9]+") %>% 
  str_split("=", simplify = T) %>% 
  `colnames<-`(c("axis","value")) %>% 
  as_tibble() %>% 
  mutate(value=as.integer(value)+1)

mat <- matrix(0, ncol = max(coords[,1]), nrow = max(coords[,2]))

walk2(coords[,1], coords[,2], function(x,y) {
  mat[y,x]<<-1
  })

fold_paper <- function(matrix, given_pos = c("x","y"), position) {
  given_pos = match.arg(given_pos)
  
  nrow <- nrow(matrix)
  ncol <- ncol(matrix)
  
  if (given_pos=="x") {
    
    width<-min(position-1, ncol-position)
    matrix[,(position-width):(position-1)] <- matrix[,(position-width):(position-1)] + matrix[,(position+width):(position+1)]
    return(matrix[,1:(position-1)])
  }
  
  if (given_pos=="y") {
    
    width<-min(position-1, nrow-position)
    matrix[(position-width):(position-1),] <- matrix[(position-width):(position-1),] + matrix[(position+width):(position+1),]
    return(matrix[1:(position-1),])
  }
}

sum(fold_paper(mat, folds$axis[1], folds$value[1])!=0)

# part 2 ####
fold_all <- function(matrix, folds) {
  walk2(folds$axis, folds$value, function(axis,value) {
    matrix <<- fold_paper(matrix, axis, value) })
  matrix
}

ifelse(fold_all(mat, folds)==0,".","#") %>% 
  apply(1,str_c,collapse = "")


# Visualization ####

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
    p1 <- (position-width):(position-1)
    p2 <- (position+width):(position+1)
    p3 <- 1:(position-1)
    
    matrix[,p1] <- matrix[,p1] + matrix[,p2]
    return(matrix[,p3])
  }
  
  if (given_pos=="y") {
    
    #DRY this
    width<-min(position-1, nrow-position)
    p1 <- (position-width):(position-1)
    p2 <- (position+width):(position+1)
    p3 <- 1:(position-1)
    
    matrix[p1,] <- matrix[p1,] + matrix[p2,]
    return(matrix[p3,])
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
library(animation)

plot_paper <- function(matrix) {
  ifelse(matrix==0,0,1) %>% 
    t() %>% 
    reshape2::melt() %>% 
    ggplot(aes(x=Var1, y=-Var2, fill=as.factor(value))) +
    geom_tile(show.legend = F) +
    theme_void() +
    scale_fill_manual(values = c("black","white"))
} 

fold_animate <- function(matrix, folds, filename) {
  saveGIF({
    print(plot_paper(matrix))
    for(i in seq_along(folds$axis)) {
      matrix <- fold_paper(matrix, folds$axis[i], folds$value[i])
      print(plot_paper(matrix))
    }
  },
  loop=T,
  interval=0.5,
  ani.width = 300, 
  ani.height = 300,
  movie.name = filename, 
  outdir = getwd()
  )
}

fold_animate(mat, folds, "folding.gif")


library(tidyverse)

# part 1 ####
input <- read_lines("Day20/input.txt")
# input <- read_lines("Day20/test.txt")

parse_input <- function(input) {
  
  break_line <- which(input=="")
  
  iea <- input[1:break_line] %>% 
    str_c(collapse = "") %>% 
    str_replace_all(c("#"="1", "\\."="0")) %>% 
    str_split("", simplify = T) %>% 
    as.integer()
  
  image <- input[(break_line+1):length(input)] %>% 
    str_replace_all(c("#"="1", "\\."="0")) %>% 
    str_split("", simplify = T)
  image <- image %>% 
    as.integer() %>% 
    matrix(ncol = ncol(image))
  
  return(list(image=image, iea=iea))
}

# expand image by x pixels on all sides
expand_image <- function(image, n=1, default=0) {
  
  exp_image <- matrix(default, nrow = nrow(image)+n*2, ncol = ncol(image)+n*2)
  
  exp_image[(n+1):(nrow(exp_image)-n),(n+1):(ncol(exp_image)-n)] <- image
  
  return(exp_image)
  
}

# extract 9 pixel around a coordinate
extract_square <- function(image,x,y, n=1, default) {

  x <- x+n #n
  y <- y+n #n

  exp_image <-  expand_image(image, n=n, default=default)

  exp_image[(y-1):(y+1),(x-1):(x+1)]

}

# extract_square <- function(image,row,col, space=0) {
#   
#   image[(row-1):(row+1),(col-1):(col+1), drop=F]
#   
# }
  

#convert square of pixels to bin
square2bin <- function(square) {
  
  as.integer(t(square))
  
}

# binary to dec
bin2dec <- function(bin)
{
  pow <- 2 ^ ((length(bin) - 1):0)
  sum(pow[bin == 1])
}


# pixel to decimal
pixel_to_dec <- function(image, x, y, default) {
  image %>% 
    extract_square(x,y,default=default) %>% 
    square2bin() %>% 
    bin2dec()
}

#from pixel to enhanced pixel
enhance_pixel <- function(image, x, y, iea, default) {
  dec <- pixel_to_dec(image,x,y, default=default)
  iea[dec+1]
}

enhance_image <- function(image, iea, n, default=0) {
  
  enh_img <- image
  
  for (i in 1:n) {
    
    exp_image <- expand_image(enh_img, default = default)
    row_index <- rep(1:nrow(exp_image),ncol(exp_image))
    col_index <- rep(1:ncol(exp_image), each=nrow(exp_image))
    
    enh_img <- map2_int(col_index,row_index, function(x,y) {
      enhance_pixel(exp_image,x,y,iea,default)
    }) %>% 
      matrix(nrow = nrow(exp_image))
    
    # this is because the first bit of iea is 1
    # and the last in 0
    default <- ifelse(default,0,1)
    
  }
  
  enh_img
}


image <- parse_input(input)$image
iea <- parse_input(input)$iea

sum(enhance_image(image, iea, 2)==1 )

# part 2 ####
sum(enhance_image(image, iea, 50)==1)


# Visualization ####
library(animation)

plot_space <- function(image) {
  image %>% 
    reshape2::melt() %>% 
    ggplot(aes(x=Var1, y=-Var2, fill=as.factor(value))) +
    geom_tile(show.legend = F) +
    theme_void() +
    scale_fill_manual(values = c("black","white"))
} 

animate <- function(image, iters, filename, default=0) {
  saveGIF({
    print(plot_space(image))
    for(i in 1:iters) {
      image <- enhance_image(image,iea = iea, 1, default = default)
      print(plot_space(image))
      default <- ifelse(default,0,1)
    }
  },
  loop=T,
  interval=0.25,
  ani.width = 300, 
  ani.height = 300,
  movie.name = filename, 
  outdir = getwd()
  )
}

animate(image, iters = 50, filename="day20animation.gif")

library(tidyverse)
library(magrittr)

# part 1 ####
input <- read_lines("Day16/input.txt")

hextobin_df <- read_lines("Day16/hextobin.txt") %>% 
  str_split(" = ") %>% 
  map(function(x) {setNames(x, c("hex","bin"))}) %>% 
  purrr::reduce(bind_rows) 

hextobin_recode <- setNames(hextobin_df$bin,hextobin_df$hex)

input_to_bin <- function(input) {
  bincode <- input %>%  str_split("") %>% unlist() %>% recode(!!!hextobin_recode) %>% 
    str_c(collapse = "")
  bincode
}


bin2dec <- function(x)
{
  b <- as.numeric(unlist(strsplit(x, "")))
  pow <- 2 ^ ((length(b) - 1):0)
  sum(pow[b == 1])
}

read_header <- function(code) {
  version <- str_sub(code, 1,3) %>% bin2dec()
  type <- str_sub(code, 4,6) %>% bin2dec()
  return(list(version=version, type=type))
}

read_packet_info <- function(code) {
  header <- read_header(code)
  
  type_len <- NA_integer_
  packet_length <- NA_integer_
  n_packets <- NA_integer_

  if(header$type!=4) {
    type_len<-str_sub(code, 7,7)
    if(type_len=="0") {packet_length <- str_sub(code,8,22) %>% bin2dec()}
    if(type_len=="1") {n_packets <- str_sub(code,8,18) %>% bin2dec() }
  }
  return(list(header=header, 
              type_len=type_len, 
              packet_length=packet_length,
              n_packets=n_packets))
}

read_packet_value <- function(packet) {
  chunks <- list()
  while(T) {
    new_chunck <- str_extract(packet, "[01]{5}")
    chunks <- c(chunks, new_chunck)
    packet <- str_sub(packet,6,-1)
    if (str_sub(new_chunck,1,1)=="0") {break}
  }
  value <- chunks %>% map(str_sub, 2,5) %>% 
    purrr::reduce(str_c, collapse="") %>% 
    bin2dec()
  length <- length(chunks)*5
  return(list(value=value, length=length))
}


process_code <- function(bincode) {
  unread <- bincode
  packet_id <- 1
  result <- list()
  while(str_detect(unread,"1")) {
    # print(unread)
    packet <- read_packet(unread)
    result[[packet_id]] <- packet$content
    unread <- str_sub(unread, (packet$length) +1, -1)
    packet_id <- packet_id + 1
  }
  return(result)
}


# must return packet$content and packet$length
read_packet <- function(bincode) {

  unread <- bincode
  info <- read_packet_info(unread)
  unread <- str_sub(unread,7,-1)
  
  #if type4 read as value
  if (info$header$type==4) {
    packet <- read_packet_value(unread)
    unread <- str_sub(unread,packet$length+1,-1)
    content <- list(info=info, value=packet$value, subpackets = list())
  }
  # if not type4
  if (info$header$type!=4) {
    
    # read by length
    if (info$type_len==0) {
      unread <- str_sub(unread, 17,-1)
      subpackets <- process_code(str_sub(unread,1,info$packet_length))
      unread <- str_sub(unread,info$packet_length+1,-1)
      content <- list(info=info, value=NA, subpackets=subpackets)
    }
    
    # read by n packets
    if (info$type_len==1) {
      unread <- str_sub(unread, 13,-1)
      
      subpackets <- list()
      
      for (i in 1:info$n_packets) {
        packet <- read_packet(unread)
        subpackets[[i]] <- packet$content
        unread <- str_sub(unread, packet$length+1, -1)
      }
      
      content <- list(info=info, value=NA, subpackets=subpackets)
    }
    
    # subpackets operations:
    #sum
    if (info$header$type==0) {
      content$value <- sapply(subpackets, function(packet) {
        packet$value
      }) %>% sum()
    }
    
    # product
    else if (info$header$type==1) {
      content$value <- sapply(subpackets, function(packet) {
        packet$value
      }) %>% prod()
    }
    
    # minimum
    else if (info$header$type==2) {
      content$value <- sapply(subpackets, function(packet) {
        packet$value
      }) %>% min()
    }
    
    # maximum
    else if (info$header$type==3) {
      content$value <- sapply(subpackets, function(packet) {
        packet$value
      }) %>% max()
    }
    
    # greater than
    else if (info$header$type==5) {
      content$value <- as.integer(subpackets[[1]]$value > subpackets[[2]]$value)
    }
    
    # less than
    else if (info$header$type==6) {
      content$value <- as.integer(subpackets[[1]]$value < subpackets[[2]]$value)
    }
    
    # equal to
    else if (info$header$type==7) {
      content$value <- as.integer(subpackets[[1]]$value == subpackets[[2]]$value)
    }
  }
  
  length <- nchar(bincode) - nchar(unread)
  
  return(list(content=content,
              length=length))
}

sum_versions <- function(packet_list) {
  unlisted <- unlist(packet_list)
  unlisted[unlisted %>% 
             names() %>% grep("version",.)] %>% as.integer() %>% sum()
}

# result
input %>% 
  input_to_bin() %>% 
  process_code() %>% 
  sum_versions()


# part 2 ####
input %>% 
  input_to_bin() %>% 
  process_code() %>% 
  pluck(1) %>% 
  pluck("value") %>% format(scientific=F)

library(tidyverse)

# part 1 ####
input <- read_lines("Day10/input.txt")

score_table <- c(")"= 3,"]"= 57,"}"= 1197, ">" = 25137, "correct"=0)

catch_error <- function(code, correct_value = c("logical","string")) {
  
  correct_value = match.arg(correct_value)
  
  code <- code %>% 
    str_split("") %>% unlist()
  
  string <- ""
  for (sym in code) {
    if (sym %in% c("(","[","{","<")) {
      
      (string <- str_c(string, sym, sep=""))
    }
    
    if (sym %in% c(")","]","}",">")) {
      closesym <- case_when(sym==")"~"(",
                            sym=="]"~"[",
                            sym=="}"~"{",
                            sym==">"~"<")
      
      if (closesym != str_sub(string,-1)) {
        return(sym)
      }
      (string <- str_remove(string, str_c("\\",closesym, "$")))
    }
  }
  if (correct_value=="logical") {return("correct")}
  if (correct_value=="string") {return(string)}
}

error_detected <- map_chr(input, catch_error) %>% table()

sum(score_table[names(error_detected)] * error_detected)

# part 2 ####
score_table2 <- c(")"= 1,"]"= 2,"}"= 3, ">" = 4)

keep <- map_chr(input, catch_error)=="correct"
input2 <- input[keep]

get_score <- function(code) {
  score<-0
  code <- str_split(code,"") %>% unlist()
  for(sym in code) {
    score = score*5
    score = score + score_table2[[sym]]
  }
  score
}

added_code <-map_chr(input2, catch_error, "string") %>% 
  map_chr(function(x) {
    x %>% str_replace_all(c("\\("=")",
                            "\\["="]",
                            "\\{"="}",
                            "\\<"=">")) %>% 
      stringi::stri_reverse()
  })

scores <- map_dbl(added_code, get_score) %>% sort()

scores[(length(scores)+1)/2]
                                            
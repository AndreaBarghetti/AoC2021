library(tidyverse)

# part 1 ####
input <- read_lines("Day19/input.txt")
#input <- read_lines("Day19/test.txt")

# parse input
parse_string <- function(string) {
  str_extract_all(string, "-*[0-9]+") %>% 
    unlist() %>% 
    as.integer()
}

parse_input <- function(input) {
  
  scanners <- list()
  string=input[1]
  for (string in input) {
    if(str_detect(string, "scanner")) {
      scanner_num <- parse_string(string) %>% as.character()
    }
    else if (string=="") {
    }
    else {
      scanners[[scanner_num]] <- rbind(scanners[[scanner_num]],parse_string(string))
    }
  }
  scanners
}

scanners <- parse_input(input)

# list all 24 possible orientation
sides <- gtools::permutations(3,3)
directions <- gtools::permutations(v = c(-1,1),n = 2,r=3, repeats.allowed = T)
#directions <- directions[apply(directions,1,function(x) {sum(x==-1)%in%c(0,2)}),]

orientations <- map(1:nrow(sides), function(i) {
  t(sides[i,] * t(directions))
}) %>% purrr::reduce(rbind)

# list all 24 orientation of a scanner
rotate_scanner <- function(scanner) {
  scan <- scanner
  map(1:nrow(orientations), function(i) {
    ori <- orientations[i,]
    scan <- scan[,abs(ori)]
    scan <- t(t(scan) * abs(ori)/ori)
    scan
  })
}

# align two vectors until 12 matches are found
# return all values that subtracted to y make such alignment 
align_axis <- function(x,y) {
  diffs <- sapply(y, function(i) {c(x-i)}) %>% c()
  res <- which(table(diffs)>=12) %>% names() %>% as.integer()
  res
}

# check if two oriented scanners have 12+ common beacons
# if true return the second scanner with beacon coordinates aligned to scanner 1
align_scanners <- function(s1,s2) {
  
  # group all possible errors into one general fail
  tryCatch(expr = {
    
    setx <- s1[,1]
    sety <- s1[,2]
    setz <- s1[,3]
    
    s2ls <- rotate_scanner(s2)
    
    filter_x <- sapply(s2ls, function(scan) {
      length(align_axis(setx, scan[,1]))>0
    })
    
    s2ls <- s2ls[filter_x]
    
    filter_y <- sapply(s2ls, function(scan) {
      length(align_axis(sety, scan[,2]))>0
    })
    
    s2ls <- s2ls[filter_y]
    
    filter_z <- sapply(s2ls, function(scan) {
      length(align_axis(setz, scan[,3]))>0
    })
    
    s2ls <- s2ls[filter_z]
    
    # if one is found:
    s2_oriented <- s2ls[[1]]
    
    adjx <- align_axis(setx, s2_oriented[,1])
    adjy <- align_axis(sety, s2_oriented[,2])
    adjz <- align_axis(setz, s2_oriented[,3])
    
    s2_aligned <- t(t(s2_oriented) + c(adjx, adjy, adjz))
    
    #s2_aligned
    message("alignment successful!")
    list(success=T, value=s2_aligned, scanner_position=c(x=adjx, y=adjy, z=adjz))
  },
  error = function(cnd) {
    message("alignment impossible")
    
    return(list(success=F, value=NA, scanner_position=NA))
  })
}

align_all_scanners <- function(scanners) {
  
  # set positions of first scanner to 0
  scanner_positions <- list()
  scanner_positions[names(scanners)] <- NA
  scanner_positions[["0"]] <- c(x=0,y=0,z=0)
  
  # keep track of which are aligned
  scans_aligned <- scanner_positions %>% 
    map_lgl(function(x) {!is.na(x) %>% any()})
  
  # keep track of which you checked
  scans_checked <- setNames(rep(F,length(scans_aligned)),names(scans_aligned))
  
  #repeat until all scanner positions are found
  while(!all(scans_aligned)) {
    
    # pick the first aligned ID among the ones not checked yet
    ref_ID <- names(scans_aligned[scans_aligned & !scans_checked])[1]
    
    if(is.na(ref_ID)) {
      warning("nothing left to do!")
      break
    }
    
    message("checking scan:",ref_ID)
    # mark that you checked it 
    scans_checked[ref_ID] <- T
    
    #check it against all scanners not checked nor aligned yet
    for (scanID in names(scans_checked)[!(scans_aligned | scans_checked)]) {
      
      message("\t vs scan:",scanID,"...")
      
      align_res <- align_scanners(scanners[[ref_ID]], scanners[[scanID]])
      
      if (!align_res$success) {next}
      
      else {
        scanner_positions[[scanID]] <- align_res$scanner_position
        scanners[[scanID]] <- align_res$value
      }
        
    }
    
    scans_aligned <- scanner_positions %>% 
      map_lgl(function(x) {!is.na(x) %>% any()})
    
  }
  
  return(list(scanners=scanners, 
              scanner_positions=scanner_positions))
}

res <- align_all_scanners(scanners)

res$scanners %>% 
  purrr::reduce(rbind) %>% 
  unique() %>% nrow()

# part 2 ####
res$scanner_positions %>% 
  purrr::reduce(rbind) %>% 
  dist(method = "man") %>% 
  max()

# Visualization ####
library(plotly)

beacons_df <- res$scanners %>% 
  purrr::reduce(rbind) %>% 
  unique() %>% 
  as_tibble() %>% 
  setNames(c("x","y","z")) %>% 
  mutate(type="beacon")

scanners_df <- res$scanner_positions %>% 
  purrr::reduce(rbind) %>% 
  unique() %>% 
  as_tibble() %>% 
  setNames(c("x","y","z")) %>% 
  mutate(type="scanner")

df <- bind_rows(beacons, scanners)

plotly::plot_ly(data = df, 
                x=~x, 
                y=~y, 
                z=~z, 
                # type="scatter3d", 
                # mode="markers", 
                color=~type,
                colors = c('black', 'green')) %>% 
  add_markers()

ggplot(scanners, aes(x=x,y=y)) +
  geom_point(shape=21, col="green", size=50, fill="green", alpha = .3) +
  geom_point(data=beamers, shape=10, col="black", size=2)
  

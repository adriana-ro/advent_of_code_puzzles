#====================================================================================================================================================#
# notes: Advent of Code Day 12
# author: Adriana Rodriguez
# date: 12/12/2020
#====================================================================================================================================================#

#========================#
# ==== Load Packages ====
#========================#

  # load packages 
  library(data.table)
  library(stringr)

#====================#
# ==== Load Data ====
#====================#

  # load the file 
  puzzle_12 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_12.txt", header = F, fill = T)

#=================#
# ==== Part 1 ====
#=================#
 
  # 
  puzzle_12 <- data.table(V1 = c("F10", "N3", "F7", "R90", "F11"))
  
  
  
  puzzle_12[, input_position := substr(V1, 1, 1)]
  puzzle_12[, input_value    := as.numeric(substr(V1, 2, nchar(V1)))]
  
  #location_1 = c("east, 0")
  #location_2 = c("north, 0")
  
  location_1 = list(position = "east", value = 0)
  location_2 = list(position = "north", value = 0)
  
  position_1 = "east"
  value_1 = 0
  position_2 = "north"
  value_2 = 0
  
  # i = 1
  
  for (i in 1:nrow(puzzle_12)) {
    
    sub_puzzle <- puzzle_12[i]
    
    if(grepl("L|R", puzzle_12[i]$V1)) {
      
      if(grepl("L", puzzle_12[i]$V1))
      
    }
    
    
    else if(grepl("F", puzzle_12[i]$V1)){
      
      value_1 = value_1 + puzzle_12[i]$input_value
      
    }
    
  }
  
  
  
######################################
  input <- readLines("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_12.txt")

parse_input <- function(input){
    input <- as.data.frame(str_match(input, "([A-Z])(\\d+)")[,-1])
    input %>% 
        mutate(V2 = as.numeric(V2), 
               V2 = case_when(V1 == "R" ~360-V2, 
                              TRUE ~ V2))
    
}

input <- parse_input(input)

# Using complex plane system! New trick discovered

movement <- list(E = 1 +0i, 
                 W = -1 + 0i,
                 N = 0 + 1i,
                 S = 0 - 1i)

move_ship <- function(input, waypoint = (1 + 0i), ship = (0 + 0i), part1 = TRUE){
    for(i in 1:nrow(input)){
        cur_step <- input[i, 1]
        count <- input[i, 2]
        if(cur_step %in% c("R", "L")){
            count <- count / 90
            waypoint <- waypoint * (0 + 1i)^count
        } else if(cur_step == "F"){
            ship <- ship + waypoint * count
        } else if(part1){
            ship <- ship + movement[[cur_step]] * count
        } else {
            waypoint <- waypoint + movement[[cur_step]] * count
        }
    }
    abs(Re(ship)) + abs(Im(ship))
}

# part 1
move_ship(input)

# part 2
move_ship(input, waypoint = (10 + 1i), ship = (0+0i), part1 = FALSE)
  
############

f <- file("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_12.txt", "r")

# part 1
# part1
x <- y <- 0
direction <- "E"
while (TRUE) {
  line <- readLines(f, n = 1)
  if (length(line) == 0) {
    break
  }
  order <- substring(line, 1, 1)
  measure <- strtoi(substring(line, 2))
  if (order == "F") {
    order <- direction
  }
  if (order == "N") {
    y <- y + measure
  }
  if (order == "S") {
    y <- y - measure
  }
  if (order == "E") {
    x <- x + measure
  }
  if (order == "W") {
    x <- x - measure
  }
  if (order == "R" || order == "L") {
    if (measure == 180) {
      if (direction == "E")
        direction <- "W"
      else if (direction == "W")
        direction <- "E"
      else if (direction == "S")
        direction <- "N"
      else if (direction == "N")
        direction <- "S"
    }
    if ((order == "R" &&
         measure == 90) || (order == "L" && measure == 270)) {
      if (direction == "E")
        direction <- "S"
      else if (direction == "S")
        direction <- "W"
      else if (direction == "W")
        direction <- "N"
      else if (direction == "N")
        direction <- "E"
    }
    if ((order == "L" &&
         measure == 90) || (order == "R" && measure == 270)) {
      if (direction == "E")
        direction <- "N"
      else if (direction == "S")
        direction <- "E"
      else if (direction == "W")
        direction <- "S"
      else if (direction == "N")
        direction <- "W"
    }
  }
}
part1 <- abs(x) + abs(y)










# part2
ship <- c(0, 0)
waypoint <- c(10, 1)
movements <- list(
  E = c(1, 0),
  S = c(0,-1),
  W = c(-1, 0),
  N = c(0, 1)
)
while (TRUE) {
  line <- readLines(f, n = 1)
  if (length(line) == 0) {
    break
  }
  order <- substring(line, 1, 1)
  measure <- strtoi(substring(line, 2))
  if (order == "N" || order == "E" || order == "S" || order == "W") {
    waypoint <- waypoint + (measure * movements[order][[1]])
  }
  else if (order == "F") {
    ship <- ship + (waypoint * measure)
  }
  else if (order == "R" || order == "L") {
    angle <- measure * pi / 180
    if (order == "R")
      angle <- angle * -1
    M <-
      matrix(c(cos(angle),-sin(angle), sin(angle), cos(angle)), 2, 2)
    waypoint <- waypoint %*% M
  }
}
part2 <- abs(ship[1]) + abs(ship[2])

  
#====================================================================================================================================================#
# notes: Advent of Code Day 3 
# author: Adriana Rodriguez
# date: 12/03/2020
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
  puzzle_3 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_3.txt", header = F)

  
#==============#
# ==== try ====
#==============#

  # 
  puzzle_3 <- data.table(V1 = c("..##.........##.........##.........##.........##.........##.......", 
                                "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
                                ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
"..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
"..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....", 
".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
".#........#.#........#.#........#.#........#.#........#.#........#",
"#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
"#...##....##...##....##...##....##...##....##...##....##...##....#",
".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"
))
  
#=================#
# ==== Part 1 ====
#=================#

 ## pattern is right 3 and 1 down, and in the map your position is moving 3 over 
 #puzzle_3[, position :=  1: .N ]
 #
 ## recode first 
 #puzzle_3[1, position := 0]
 #
 #
 ## add sedonc 
 #puzzle_3[, second_position := 1: .N]
  
  # replace 
  puzzle_3[, recoded := V1]
  puzzle_3[, recoded := gsub("#", "1", recoded)]
  puzzle_3[, recoded := gsub("\\.", "0", recoded)]
  
# for (i in 1:nrow(puzzle_3)) {
#   
#   
#   if (i == 1) {
#     
#     
#     puzzle_3[i, position := 1]
#     
#     stored_num <- puzzle_3[i]$position
#     
#   }
#   
#   
#   else {
#     
#     if(stored_num == 31 | stored_num == 30) { 
#     #if(stored_num == 7) { 
#       
#       if(stored_num == 31) {
#       puzzle_3[i, position := 0]
#     
#     stored_num <- puzzle_3[i]$position
#       } 
#       
#       else {
#         
#         puzzle_3[i, position := 1]
#         
#     stored_num <- puzzle_3[i]$position
#       }
#       
#       #stored_num = 1
#     }
#     
#     
#     else {
#     puzzle_3[i, position := stored_num + 3]
#     
#     stored_num <- puzzle_3[i]$position
#     
#     }
#     
#     }
#     
#   
# }
  
  
  
  
   for (i in 1:nrow(puzzle_3)) {
    
    
    if (i == 1 | stored_num == 31) {
      
      
      puzzle_3[i, position := 1]
      
      stored_num <- puzzle_3[i]$position
      
    }
    
    
    else {
      
      
      puzzle_3[i, position := stored_num + 3]
      
      stored_num <- puzzle_3[i]$position
      
    }
      
      }
      
    
  
 
  puzzle_3[, value_at_position := substr(recoded, position, position)]
   
  ea_table(puzzle_3, "value_at_position")
  
  # for every row in the puzzle file 
  for(i in 1:nrow(puzzle_3)){ 
    
    if (puzzle_3[i]$position == 0) {
    
    puzzle_3[i, value_at_position := 0]
    
    } 
    
    else { 
    # 
    puzzle_3[i , value_at_position := transpose(str_split(V1,""))[as.numeric(position)]]
   
    }
    # end for loop
    }

  
  
  ea_table(puzzle_3, "value_at_position") # not 75 # not 78 # not 94 # not 80 # not 91 # not 5 # not 242 # NEED to try 68 # not 233
  
  
#=======================#
# ==== other person ====
#=======================#

  # Load libraries
library(tidyr)
library(stringr)

# Read in data
input <- read.delim("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_3.txt", header = FALSE, text = "character")


# Find out how many characters per entry
chars <- nchar(input[1, ])

# Change periods to 0 and trees to 1
input$V1 <-gsub(x = input$V1, "\\.", replacement = "0")
input$V1 <-gsub(x = input$V1, "#", replacement = "1")

# Separate into columns
map <- str_split_fixed(input$V1, "", chars)
map <- data.frame(apply(map, 2, as.numeric))

# Copy the same map 10 times, and then some
largemap <- cbind(map, map, map, map, map, map, map, map, map, map)
largermap <- cbind(largemap, largemap, largemap, largemap, largemap)

# We need to go down 323 times, so let's create coordinates
rownums <- seq(1:323)
colnums <- seq(1, 967, by = 3)

# Initiate container
result <- 0

# Loop over the coordinates
for (i in 1:323) {
result <- result + largermap[rownums[i], colnums[i]]
}
result # NOT 233 # yes 232 
  
# 1st 

# We need to go down 323 times, so let's create coordinates
rownums <- seq(1:323)
colnums <- seq(1, 967, by = 1)

# Initiate container
result <- 0

# Loop over the coordinates
for (i in 1:323) {
result <- result + largermap[rownums[i], colnums[i]]
}
result # 86 
  

# 3rd 

# We need to go down 323 times, so let's create coordinates
rownums <- seq(1:323)
colnums <- seq(1, 967, by = 5)

# Initiate container
result <- 0

# Loop over the coordinates
for (i in 1:323) {
result <- result + largermap[rownums[i], colnums[i]]
}
result # 0  
  

#==================#
# ==== another ====
#==================#
library(readr)

input <- read_tsv('/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_3.txt',
                  col_names = FALSE)

ntrees <- 0

nrows <- nrow(input)

nchars <- nchar(input[1, 1])

pos <- 1

i <- 1

right <- 1

down <- 2

while(i <= nrows){
  
  square <- substr(input[i, 1], pos, pos)
  
  if(square == "#"){
    
    ntrees <- ntrees + 1
    
  }

  
  if(pos + right > nchars){
    
    pos <- pos + right - nchars
    
  }else{
    
    pos <- pos + right
    
  }

  i <- i + down
  
}

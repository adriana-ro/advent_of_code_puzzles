#====================================================================================================================================================#
# notes: Advent of Code Day 8
# author: Adriana Rodriguez
# date: 12/08/2020
#====================================================================================================================================================#

#========================#
# ==== Load Packages ====
#========================#

  # load packages 
  library(data.table)
  library(stringr)
  library(stringi)

#====================#
# ==== Load Data ====
#====================#

  # load the file 
  puzzle_8 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_8.txt", header = F, fill = T)

#=================#
# ==== Part 1 ====
#=================#
  
  # 
  puzzle_8 <- data.table(V1 = c("nop",
                         "acc",  
                         "jmp",
                         "acc", 
                         "jmp",
                         "acc", 
                         "acc",
                         "jmp", 
                         "acc"),  
                          V2 = c(+0, 
                          +1, 
                          +4 ,
                          +3 ,
                          -3 ,
                          -99,
                          +1 ,
                          -4 ,
                          +6 ))
                         
  
  i = 1
  
  stored_latest_number = 0
  
repeat{
   
  if(puzzle_8[i]$V1 == "nop") {
    
    puzzle_8[ i, latest_number := stored_latest_number]
    
    stored_latest_number <- puzzle_8[i]$latest_number
    
    puzzle_8[i, position := i]
    puzzle_8[i, flag_has_gone := 1]
    
    i = i + 1 
    
    
    
  }
  
  if(puzzle_8[i]$V1 == "acc") {
   
    puzzle_8[ i, latest_number := stored_latest_number + V2]
    
    stored_latest_number <- puzzle_8[i]$latest_number
    
    puzzle_8[i, position := i]
    puzzle_8[i, flag_has_gone := 1]
     
      
    #i = i + puzzle_8[i]$V2
    i = i + 1
    
  }
    
  if(puzzle_8[i]$V1 == "jmp") {
    
  
    
    stored_latest_number <- stored_latest_number
    
     puzzle_8[i, position := i]
     puzzle_8[i, flag_has_gone := 1]
     
       i = i + puzzle_8[i]$V2
  }
    
    
    if(!is.na(puzzle_8[i]$flag_has_gone)) {
      
      print(stored_latest_number)
      break
    }
    
  }
  
 

####################################  
   
 puzzle_8 <- copy(initial_puzzle)
  
  initial_puzzle <- copy(puzzle_8)
  
   
  i_initial = 1
 # 
 # stored_latest_number = 0
  
repeat{
   
  if(initial_puzzle[i_initial]$V1 == "nop"| 
     initial_puzzle[i_initial]$V1 == "jmp" ) {
  
    # make copy 
    puzzle_8 <- copy(initial_puzzle)
    
    
    if(initial_puzzle[i_initial]$V1 == "nop") {
    
    puzzle_8[i_initial,, V1 := "jmp"]
    } 
    else {
      
    puzzle_8[i_initial, V1 := "nop"]
    
    }
      
    i = 1
  
    stored_latest_number = 0
    
    repeat{
    
  if(puzzle_8[i]$V1 == "nop") {
    
    puzzle_8[ i, latest_number := stored_latest_number]
    
    stored_latest_number <- puzzle_8[i]$latest_number
    
    puzzle_8[i, position := i]
    puzzle_8[i, flag_has_gone := 1]
    
    i = i + 1 
    
    
    
  }
  
  if(puzzle_8[i]$V1 == "acc") {
   
    puzzle_8[ i, latest_number := stored_latest_number + V2]
    
    stored_latest_number <- puzzle_8[i]$latest_number
    
    puzzle_8[i, position := i]
    puzzle_8[i, flag_has_gone := 1]
     
      
    #i = i + puzzle_8[i]$V2
    i = i + 1
    
  }
    
  if(puzzle_8[i]$V1 == "jmp") {
    
  
    
    stored_latest_number <- stored_latest_number
    
     puzzle_8[i, position := i]
     puzzle_8[i, flag_has_gone := 1]
     
       i = i + puzzle_8[i]$V2
  }
    
    
    if(!is.na(puzzle_8[i]$flag_has_gone)) {
      
      i_initial = 1 + i_initial
      puzzle_8[, flag_has_gone := NA]
      print(paste0("The jpm or nop in ", i, " row is not it and got to ", stored_latest_number))
      break
    }
    
    
  }
  
    }
  else { 
    i_initial = i_initial + 1
    }
  
 
   } # end first repeat
  
 

  # when it breaks, just get the latest stored number that didn't make it 
  stored_latest_number
   
 
  

  
  
  

  
  
  
                           
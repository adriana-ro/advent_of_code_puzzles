#====================================================================================================================================================#
# notes: Advent of Code Day 5 
# author: Adriana Rodriguez
# date: 12/05/2020
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
  puzzle_5 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_5.txt", header = F)

#=================#
# ==== Part 1 ====
#=================#
  
  #=======================#
  # ==== create xwalk ====
  #=======================#

  # set initial row and column tables 
  row_xwalk <- data.table(expand.grid(range = c("1_2", "2_3", "3_4", "4_5", "5-6", "6-7"), row_num = 1:nrow(puzzle_5)))
  col_xwalk <- data.table(expand.grid(range = c("1-2", "2-3"), row_num = 1:nrow(puzzle_5)))
  
  # add old range name, new range name, and letter position for row range columns 
  row_xwalk[, old_range_name := paste0("row_range_", substr(range, 1,1))]
  row_xwalk[, new_range_name := paste0("row_range_", substr(range, 3,3))]
  row_xwalk[, letter_position := substr(range, 3,3)]
  
  # add old range name, new range name, and letter position for column columns  
  col_xwalk[, old_range_name := paste0("col_range_", substr(range, 1,1))]
  col_xwalk[, new_range_name := paste0("col_range_", substr(range, 3,3))]
  col_xwalk[, letter_position := substr(range, 3,3)]
  
  # set manual position for these, too since they don't follow the order 
  col_xwalk[letter_position == 2, letter_position := 9]
  col_xwalk[letter_position == 3, letter_position := 10]
  
  # stack together
  position_xwalk <- rbind(row_xwalk, col_xwalk)
    
  #==========================#
  # ==== create function ====
  #==========================#

  # write a function that will check every letter at each of the 10 positions in the "V1" group codes     
  position_fun <- function(data, row_num, old_range_name, new_range_name, letter_position) {
  
  # if letter if F or L 
  if (grepl("F|L", substr(data[row_num]$V1, letter_position,letter_position))) {
    
    # get lower end number 
    data[row_num, lower_end := transpose(str_split(get(old_range_name),"-"))[1]]
    
    # get upper end number 
    data[row_num, upper_end := (((as.numeric(transpose(str_split(get(old_range_name),"-"))[2]) - as.numeric(transpose(str_split(get(old_range_name),"-"))[1]))-1)/2) + 
          as.numeric(transpose(str_split(get(old_range_name),"-"))[1])]
   
    # put together  
    data[row_num, paste0(new_range_name) := paste0(lower_end, "-", upper_end)]
    
    # delete columns 
    data[, c("lower_end", "upper_end") := NULL]
    
  }
  
  # if letter not F or L (aka R or F)
  else {
    
    # get lower end number
    data[row_num, lower_end := (((as.numeric(transpose(str_split(get(old_range_name),"-"))[2]) - as.numeric(transpose(str_split(get(old_range_name),"-"))[1]))+1)/2) + 
                                  as.numeric(transpose(str_split(get(old_range_name),"-"))[1])]
    
    # get upper end number
    data[row_num, upper_end := transpose(str_split(get(old_range_name),"-"))[2]]
    
    # put range together 
    data[row_num, paste0(new_range_name) := paste0(lower_end, "-", upper_end)]
    
    # delete column 
    data[, c("lower_end", "upper_end") := NULL]
    
    }
  
  # end function
  } 
  
 
  #=======================================================#
  # ==== start getting number ranges and run function ====
  #=======================================================#

  # set manual values for first row 
  puzzle_5[grepl("F", substr(V1, 1,1)), row_range_1 := paste0(0, "-", 126/2)]
  puzzle_5[grepl("B", substr(V1, 1,1)), row_range_1 := paste0(128/2, "-", 127)]
    
  # set manual values for first column 
  puzzle_5[grepl("L", substr(V1, 8,8)), col_range_1 := paste0(0, "-", 6/2)]
  puzzle_5[grepl("R", substr(V1, 8,8)), col_range_1 := paste0(8/2, "-", 7)]
 
  # run function across all columns in the dataset using the xwalk 
  purrr::walk(1:nrow(position_xwalk), ~position_fun(data            = puzzle_5, 
                                                    row_num         = position_xwalk[.x]$row_num,
                                                    old_range_name  = position_xwalk[.x]$old_range_name,
                                                    new_range_name  = position_xwalk[.x]$new_range_name,
                                                    letter_position = position_xwalk[.x]$letter_position))
  
  #=============================#
  # ==== final calculations ====
  #=============================#
  
  # the the final number for row and column
  puzzle_5[, final_row  := transpose(str_split(row_range_7,"-"))[1]]
  puzzle_5[, final_column  := transpose(str_split(col_range_3,"-"))[1]]
  
  # multiply row by 8 and add 5, per the puzzle instructions
  puzzle_5[, seat_ID := as.numeric(final_row) * 8 + as.numeric(final_column)]
  
  
  # find max and solved
  max(puzzle_5$seat_ID) # 965 
  
#=================#
# ==== part 2 ====
#=================#

  # order the seat IDs, from low to high  
  puzzle_5 <- puzzle_5[setorder(puzzle_5, "seat_ID")]
  
  # add index numbers based on the row number, starting from the very first seat ID number that appears
  puzzle_5[, order_num := 1: .N + (puzzle_5[1]$seat_ID - 1)]
  
  # the first instance where the order number and puzzle number don't match is the solution!  
  puzzle_5[seat_ID != order_num][1]$order_num #524 
  
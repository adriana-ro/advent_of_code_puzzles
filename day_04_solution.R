#====================================================================================================================================================#
# notes: Advent of Code Day 4 
# author: Adriana Rodriguez
# date: 12/04/2020
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
  puzzle_4 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_4.txt", header = F, fill = T)

  
#=================#
# ==== Part 1 ====
#=================#
  
  passport_num <- 0
  
  # assign passport number 
  for(i in 1:nrow(puzzle_4)) {
    
    if(all(puzzle_4[i] == "" | is.na(puzzle_4[i]))){
      
      puzzle_4[i, pp_num := 999999999]
      
      passport_num <- stored_num 
      
    }
    
    else {
      puzzle_4[i, pp_num := passport_num + 1]
    
      stored_num <- puzzle_4[i]$pp_num
    }
    
    
  }
  
  
  # now look at valid fields 
  
  # flags 
  puzzle_4[, flag_byr := 0]
  puzzle_4[grepl("byr", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_byr := 1]
  
  puzzle_4[, flag_iyr := 0]
  puzzle_4[grepl("iyr", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_iyr := 1]
  
  puzzle_4[, flag_eyr := 0]
  puzzle_4[grepl("eyr", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_eyr := 1]
  
  puzzle_4[, flag_hgt := 0]
  puzzle_4[grepl("hgt", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_hgt := 1]
  
  puzzle_4[, flag_hcl := 0]
  puzzle_4[grepl("hcl", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_hcl := 1]
  
  puzzle_4[, flag_ecl := 0]
  puzzle_4[grepl("ecl", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_ecl := 1]
  
  puzzle_4[, flag_pid := 0]
  puzzle_4[grepl("pid", paste(V1, V2, V3, V4, V5, V6, V7, V8)), flag_pid := 1]

  
  
  
  # take max 
  puzzle_4[, max_flag_byr := max(flag_byr), by = c("pp_num")]
  puzzle_4[, max_flag_iyr := max(flag_iyr), by = c("pp_num")]
  puzzle_4[, max_flag_eyr := max(flag_eyr), by = c("pp_num")]
  puzzle_4[, max_flag_hgt := max(flag_hgt), by = c("pp_num")]
  puzzle_4[, max_flag_hcl := max(flag_hcl), by = c("pp_num")]
  puzzle_4[, max_flag_ecl := max(flag_ecl), by = c("pp_num")]
  puzzle_4[, max_flag_pid := max(flag_pid), by = c("pp_num")]
  
  
  # okay to miss cid only 
  puzzle_4[, meets_all_reqs := 0]
  puzzle_4[max_flag_byr == 1 &
           max_flag_iyr == 1 &
           max_flag_eyr == 1 &
           max_flag_hgt == 1 &
           max_flag_hcl == 1 &
           max_flag_ecl == 1 &
           max_flag_pid == 1 
    , meets_all_reqs := 1]
  
  
  # 
  final_data <- ea_no_dups(ea_subset(puzzle_4, select = c(pp_num, meets_all_reqs)))
  
  nrow(final_data[meets_all_reqs == 1]) # 206

#=================#
# ==== Part 2 ====
#=================#

  # columns 
  cols_to_check <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  
  xwalk <- data.table(expand.grid(cols_to_check = cols_to_check, number = 1:nrow(puzzle_4)))
  #xwalk <- xwalk[1:8184]
  
  
  puzzle_4[puzzle_4 == ""] <- "no_data"
  
  #puzzle_4 <- puzzle_4[455]
  #puzzle_4 <- puzzle_4[400:500]
  
  #byr
  #iyr
  #eyr
  #hgt
  #hcl
  #ecl
  #pid
  #i =2
  
  for(i in 1:nrow(xwalk)) {
    
    row_num <- xwalk[i]$number
    col_to_check <- as.character(xwalk[i]$cols_to_check)
    
    # byr 
    if(puzzle_4[row_num, grepl("byr", get(col_to_check))] == T) {
      
     
      puzzle_4[row_num & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 1920 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2002 
                         , flag_byr2 := 1]
      }
    
    
    # iyr 
     if(puzzle_4[row_num, grepl("iyr", get(col_to_check))] == T) {
      
      
      puzzle_4[row_num & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 2010 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2020 
                         , flag_iyr2 := 1]
      }
    
    # eyr 
     if(puzzle_4[row_num, grepl("eyr", get(col_to_check))] == T) {
      
      
      puzzle_4[row_num & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 2020 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2030 
                         , flag_eyr2 := 1]
      }
    
    # hgt 
     if(puzzle_4[row_num, grepl("hgt", get(col_to_check))] == T) {
      
    
      puzzle_4[(row_num & grepl("in$", get(col_to_check)) &
               as.numeric(gsub("in", "", ea_scan(get(col_to_check), 2, ":"))) >= 59 & 
               as.numeric(gsub("in", "", ea_scan(get(col_to_check), 2, ":"))) <= 76 ) | 
                (row_num & grepl("cm$", get(col_to_check)) &
               as.numeric(gsub("cm", "", ea_scan(get(col_to_check), 2, ":"))) >= 150 & 
               as.numeric(gsub("cm", "", ea_scan(get(col_to_check), 2, ":"))) <= 193 )
               , flag_hgt2 := 1]
      }
    
     #hcl 
     if(puzzle_4[row_num, grepl("hcl", get(col_to_check))] == T) {
      
    
      puzzle_4[row_num & 
                 grepl("#", ea_scan(get(col_to_check), 2, ":")) &
                 nchar(gsub("#", "", ea_scan(get(col_to_check), 2, ":"))) == 6 & 
                 !grepl("g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", ea_scan(get(col_to_check), 2, ":")), flag_hcl2 := 1]
      
      }
    
    
     # ecl 
     if(puzzle_4[row_num, grepl("ecl", get(col_to_check))] == T) {
      
     
      puzzle_4[row_num & 
                 grepl("amb|blu|brn|gry|grn|hzl|oth", get(col_to_check)) &
                 nchar( ea_scan(get(col_to_check), 2, ":")) == 3  , flag_ecl2 := 1]
      }
    
    
    # pid 
     if(puzzle_4[row_num, grepl("pid", get(col_to_check))] == T) {
      
     
      puzzle_4[row_num & nchar( ea_scan(get(col_to_check), 2, ":")) == 9  , flag_pid2 := 1]
      }
  
  }
  
  
  ##################################### again 
  
  
  for(i in 1:nrow(xwalk)) {
    
    row_num <- xwalk[i]$number
    col_to_check <- as.character(xwalk[i]$cols_to_check)
    
    # byr 
    if(puzzle_4[row_num, grepl("byr", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_byr2 := 0]
      puzzle_4[row_num & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 1920 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2002 
                         , flag_byr2 := 1]
    }
    
    
    # iyr 
     else if(puzzle_4[row_num, grepl("iyr", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_iyr2 := 0]
      puzzle_4[row_num & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 2010 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2020 
                         , flag_iyr2 := 1]
      }
    
    # eyr 
     else if(puzzle_4[row_num, grepl("eyr", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_eyr2 := 0]
      puzzle_4[row_num & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 2020 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2030 
                         , flag_eyr2 := 1]
      }
    
    # hgt 
     else if(puzzle_4[row_num, grepl("hgt", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_hgt2 := 0]
      puzzle_4[(row_num & grepl("in$", get(col_to_check)) &
               as.numeric(gsub("in", "", ea_scan(get(col_to_check), 2, ":"))) >= 59 & 
               as.numeric(gsub("in", "", ea_scan(get(col_to_check), 2, ":"))) <= 76 ) | 
                (row_num & grepl("cm$", get(col_to_check)) &
               as.numeric(gsub("cm", "", ea_scan(get(col_to_check), 2, ":"))) >= 150 & 
               as.numeric(gsub("cm", "", ea_scan(get(col_to_check), 2, ":"))) <= 193 )
               , flag_hgt2 := 1]
      }
    
     #hcl 
     else if(puzzle_4[row_num, grepl("hcl", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_hcl2 := 0]
      puzzle_4[row_num & 
                 grepl("#", ea_scan(get(col_to_check), 2, ":")) &
                 nchar(gsub("#", "", ea_scan(get(col_to_check), 2, ":"))) == 6 & 
                 !grepl("g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", ea_scan(get(col_to_check), 2, ":")), flag_hcl2 := 1]
      
      }
    
    
     # ecl 
     else if(puzzle_4[row_num, grepl("ecl", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_ecl2 := 0]
      puzzle_4[row_num & 
                 grepl("amb|blu|brn|gry|grn|hzl|oth", get(col_to_check)) &
                 nchar( ea_scan(get(col_to_check), 2, ":")) == 3  , flag_ecl2 := 1]
      }
    
    
    # pid 
     else if(puzzle_4[row_num, grepl("pid", get(col_to_check))] == T) {
      
      puzzle_4[row_num, flag_pid2 := 0]
      puzzle_4[row_num & nchar( ea_scan(get(col_to_check), 2, ":")) == 9  , flag_pid2 := 1]
      }
  
      else {
        
        print("no match")
      }
  
      
      }
  
  
  
############# another way 
  
  
   
  
  for(i in length(cols_to_check)) {
    
    col_to_check <- cols_to_check[i]
    
    col_to_check <- "V1"
    col_to_check <- "V2"
    col_to_check <- "V3"
    col_to_check <- "V4"
    col_to_check <- "V5"
    col_to_check <- "V6"
    col_to_check <- "V7"
    col_to_check <- "V8"
    
    # byr 
      #puzzle_4[, flag_byr2 := 0]
      puzzle_4[grepl("byr", get(col_to_check)) & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 1920 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2002 
                         , flag_byr2 := 1]
   
    # iyr 
 
      #puzzle_4[, flag_iyr2 := 0]
      puzzle_4[grepl("iyr", get(col_to_check)) & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 2010 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2020 
                         , flag_iyr2 := 1]
     
    # eyr 
      #puzzle_4[, flag_eyr2 := 0]
      puzzle_4[grepl("eyr", get(col_to_check)) & nchar(ea_scan(get(col_to_check), 2, ":")) == 4 &  
               as.numeric(ea_scan(get(col_to_check), 2, ":")) >= 2020 & 
               as.numeric(ea_scan(get(col_to_check), 2, ":"))  <= 2030 
                         , flag_eyr2 := 1]
    
    # hgt 
      #puzzle_4[, flag_hgt2 := 0]
      puzzle_4[(grepl("hgt", get(col_to_check)) & grepl("in$", get(col_to_check)) &
               as.numeric(gsub("in", "", ea_scan(get(col_to_check), 2, ":"))) >= 59 & 
               as.numeric(gsub("in", "", ea_scan(get(col_to_check), 2, ":"))) <= 76 ) | 
                (grepl("hgt", get(col_to_check)) & grepl("cm$", get(col_to_check)) &
               as.numeric(gsub("cm", "", ea_scan(get(col_to_check), 2, ":"))) >= 150 & 
               as.numeric(gsub("cm", "", ea_scan(get(col_to_check), 2, ":"))) <= 193 )
               , flag_hgt2 := 1]
    
     #hcl 
      #puzzle_4[, flag_hcl2 := 0]
      puzzle_4[grepl("hcl", get(col_to_check)) & 
                 grepl("#", ea_scan(get(col_to_check), 2, ":")) &
                 nchar(gsub("#", "", ea_scan(get(col_to_check), 2, ":"))) == 6 & 
                 !grepl("g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", ea_scan(get(col_to_check), 2, ":")), flag_hcl2 := 1]
      
    
     # ecl 
   
      #puzzle_4[, flag_ecl2 := 0]
      puzzle_4[ grepl("ecl", get(col_to_check)) & 
                 grepl("amb|blu|brn|gry|grn|hzl|oth", get(col_to_check)) &
                 nchar( ea_scan(get(col_to_check), 2, ":")) == 3  , flag_ecl2 := 1]
    
    # pid 
     #puzzle_4[, flag_pid2 := 0]
    puzzle_4[grepl("pid", get(col_to_check)) & nchar( ea_scan(get(col_to_check), 2, ":")) == 9  , flag_pid2 := 1]

  
  
  }
  
  

  # set any nas to 0 
  puzzle_4[is.na(puzzle_4)] <- 0
  
  
  # take max 
  puzzle_4[, max_flag_byr2 := max(flag_byr2), by = c("pp_num")]
  puzzle_4[, max_flag_iyr2 := max(flag_iyr2), by = c("pp_num")]
  puzzle_4[, max_flag_eyr2 := max(flag_eyr2), by = c("pp_num")]
  puzzle_4[, max_flag_hgt2 := max(flag_hgt2), by = c("pp_num")]
  puzzle_4[, max_flag_hcl2 := max(flag_hcl2), by = c("pp_num")]
  puzzle_4[, max_flag_ecl2 := max(flag_ecl2), by = c("pp_num")]
  puzzle_4[, max_flag_pid2 := max(flag_pid2), by = c("pp_num")]
  
  
  # okay to miss cid only 
  puzzle_4[, meets_all_reqs2 := 0]
  puzzle_4[max_flag_byr2 == 1 &
           max_flag_iyr2 == 1 &
           max_flag_eyr2 == 1 &
           max_flag_hgt2 == 1 &
           max_flag_hcl2 == 1 &
           max_flag_ecl2 == 1 &
           max_flag_pid2 == 1 
    , meets_all_reqs2 := 1]
  
  
   # 
  final_data2 <- ea_no_dups(ea_subset(puzzle_4, select = c(pp_num, meets_all_reqs,meets_all_reqs2)))
  
  nrow(final_data2[meets_all_reqs == 1 & meets_all_reqs2 == 1]) # 125 -- not right and too high # correct answer is 123! 

  

  
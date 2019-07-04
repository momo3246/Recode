#' @title Remove excessive comma
#'
#' @description This function is to remove any trailing, leading as well as consecutive comma in between.
#'
#' @param vector
#' character vector with excessive comma
#'
#' @return character vector without excessive comma
#'
#' @examples
#' excess_comma(c(",,1,,,2,,3,,,"))
#'
#' @export excess_comma

#Remove any trailing, leading as well as consecutive commma in between
excess_comma <- function(vector) {
  results <- gsub("^,*|(?<=,),|,*$", "", vector, perl=T)
  ifelse(is.na(results), "", results)#Just to turn NA (NOT "NA" string) into empty string
}

#' @title Remove particular code(s)
#'
#' @description This function is to remove code(s) in either SA or MA question
#'
#' @param vector
#' character vector (MA question) or single numeric value (SA question)
#'
#' @param remove
#' character string (only 1 code to remove) or character vector (more than 1 code to remove)
#'
#' @examples
#' remove_code(c("1", "2,4", "3,5"), remove=c(2,3))#remove more than 1 code in MA question
#' remove_code(c("1", "2,4", "3,5"), remove=2)#remove only 1 code in MA question
#' remove_code(c(1, 2, 3), remove=3)#remove 1 code in SA question
#'
#' @export remove_code

#Can remove more than 1 answer
remove_code <- function(vector, remove) {
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check if the vector represent a SA or MA question; Check for MA condition
    message("You are processing a MA question")
    vector_numeric <- lapply(strsplit(vector, ","), FUN = as.numeric)
    results <- lapply(vector_numeric, function(x)
      paste(setdiff(x, remove), collapse=","))
    results <- unlist(results)
    ifelse(results == "NA", "", results)#Just to turn "NA" string into empty string
  } else {#SA condition
    message("You are processing a SA question")
    results <- ifelse(vector == remove, "", vector)#Replace element to be removed by empty string
    ifelse(is.na(results), "", results)#Just to turn NA (NOT "NA" string) into empty string
  }
}

#' @title Remove particular code(s)
#'
#' @description This function is to remove code(s) in either SA or MA question
#'
#' @param vector
#'
#' @param remove
#'
#' @return NULL
#'
#' @examples
#'
#' @export
#Remove_code (no message)
remove_code_no_message <- function(vector, remove) {
  if ( any(str_detect(vector, ",")==TRUE, na.rm = TRUE) ) {#Check if the vector represent a SA or MA question; Check for MA condition
    vector_numeric <- lapply(strsplit(vector, ","), FUN = as.numeric)
    results <- lapply(vector_numeric, function(x)
      paste(setdiff(x, remove), collapse=","))
    results <- unlist(results)
    ifelse(results == "NA", "", results)#Just to turn "NA" string into empty string
  } else {#SA condition
    results <- ifelse(vector == remove, "", vector)#Replace element to be removed by empty string
    ifelse(is.na(results), "", results)#Just to turn NA (NOT "NA" string) into empty string
  }
}

#' @title Remove duplicate(s)
#'
#' @description This function is to remove any duplicate in MA question
#'
#' @param vector
#' character vector
#'
#' @examples
#' dup_remove(c("1,2,3,2,3,3"))
#'
#' @import stringr
#'
#' @export dup_remove
#Remove duplicate code
dup_remove  <- function(vector) {
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check for MA
    message("You are processing a MA question")
    results <- sapply(strsplit(vector, ",", fixed = TRUE), function(x)
      paste(unique(x), collapse = ","))
    ifelse(results == "NA", "", results)#Just to turn NA string into empty string
  } else {
    message("You are processing a SA question")
    print(vector)
  }
}

#' @title Back code OTHERS into pre-coded question
#'
#' @description This function is to back code verbatim into a pre-coded question
#'
#' @param raw
#' dataframe with 2 columns
#'
#' @param coded
#' dataframe with 2 columns
#'
#' @details
#' The raw and coded dataframes should have one common column for SN matching. If no need for SN matching, just populate both SN columns with the same vector.
#' The function works by replacing code "97" in the raw data with codes in the coded data, given that the manually coded column is not empty.
#'
#' @examples
#' raw <- data.frame(SN=c(1, 2000, 3, 4), raw_data=c("1,2,97", "1,3", "97", "1,2,97"), stringsAsFactors=FALSE)##Populate a dataframe
#' coded <- data.frame(SN=c(2000, 1, 3, 4), coded_data=c(NA, "2,3", "9", "97"), stringsAsFactors=FALSE)##Populate another dataframe
#' ##You will see the output that shows 1) all duplicate should be removed and 2) SA question is also applicable
#' back_code(raw, coded)
#'
#' @import stringr
#'
#' @import plyr
#'
#' @export back_code
#Back code OTHERS into pre-coded question
back_code <- function(raw, coded) {
  if ( any(str_detect(raw[, 2], ",")==TRUE) ) {#Check for MA
    message("You are processing a MA question")
    combined <- join(raw, coded)
    raw_without_97 <- ifelse(str_detect(combined[, 2], "97") & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], 97), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
    recoded <- ifelse(str_detect(combined[, 2], "97") & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
    recoded <- excess_comma(recoded)#Remove excess comma
    dup_remove(recoded)#Remove any duplicates in resulted recoded data

  } else {
    message("You are processing a SA question")
    combined <- join(raw, coded)
    raw_without_97 <- ifelse(str_detect(combined[, 2], "97") & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], 97), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
    recoded <- ifelse(str_detect(combined[, 2], "97") & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
    excess_comma(recoded)#Do not remove duplicate for SA question
  }
}

#' @title Combine 2 questions together
#'
#' @description This function is to combine 2 questions together. The returned value should have no duplicate and excessive comma.
#'
#' @param vector1
#' character vector
#'
#' @param vector2
#' character vector
#'
#' @examples
#' vector1 <- c("1", "2", NA, NA)
#' vector2 <- c("3", "2,3,4", NA, "1")
#' combine(vector1, vector2)
#’
#'@export combine
#'
combine <- function(vector1, vector2) {
  vector1 <- ifelse(is.na(vector1), "", vector1)#Replace NA with empty string
  vector2 <- ifelse(is.na(vector2), "", vector2)#Replace NA with empty string
  combined <- ifelse(vector1=="" & vector2=="", "", #Check for both empty string
                    ifelse(vector1 != "" & vector2 != "", dup_remove(paste(vector1, vector2, sep=",")), #Check for neither empty string
                    excess_comma(paste(vector1, vector2, sep=",")))) #Remainder falls into the category of either one is empty string
  print(combined)
}


#' @title Attach textual label to pre-coded data
#'
#' @description This function is to attach textual label to numeric data by using a code spec
#'
#' @param data
#' dataframe
#'
#' @param code_spec
#' dataframe with 3 columns: first one being "varible name", second one being "code", and thrid one being "label"
#'
#' @details
#' This function would show output covering 3 cases:
#' 1) The variable being processed is not included in code spec, in which case the variable would be left unchanged;
#' 2) The variable being processed is a SA question, and
#' 3) The variable being process is a MA question
#'
#' Note that if the code is not specified in code spec, the cell containing that code would be showing NA, rather than displaying its original code
#'
#' Use xlsx::write.xlsx() to export, with the argument showNA being set as FALSE.
#
#' @examples
#' data <- data.frame(vQ1=c(1, 3, 4, 99),
#'                    vQ2=c("1,2", "3,4", "5,3", "1,2,99"),
#'                    vQ3=c(NA, NA, NA, NA),
#'                    vQ4=c(NA, 1, 3, "2,4"),
#'                    vQ5=c(1,2,3,4), stringsAsFactors=FALSE)##Populate the "data" dataframe
#' code_spec <- data.frame(variable_name=rep(c("vQ1", "vQ2", "vQ3", "vQ4"), each=5),
#'                         code=rep(c(1,2,3,4,5), times=4),
#'                         label=rep(c("1. Ricoh", "2. Fuji Xerox", "3. Canon", "4. Konica Minolta", "5. Sharp"), times=4))##Populate the "code_spec" dataframe
#'
#' labelling(data, code_spec)
#'
#' @import dplyr
#'
#' @export labelling
labelling <- function (data, code_spec) {
  data <- as.data.frame(data) #Coerce into dataframe
  code_spec <- as.data.frame(code_spec) #Coerce into dataframe
  col_names <- colnames(data)
  for ( i in col_names ) {
    if ( nrow(code_spec[code_spec[, 1] == i, ]) == 0 ) {#Check if it is specified in code spec
      message(paste('The variable "', i, '" was not specified in your code spec!', sep=""))
    } else if ( is.na(any(str_detect(data[, i], ",")))==TRUE ) {#Check if it is an empty column
      message(paste('The variable "', i, '" is an empty column!', sep=""))
    } else if ( any(str_detect(data[, i], ",")==TRUE) ) {#Check if it is a MA question
      message(paste('The variable "', i, '" is a MA question!', sep=""))
      vector_numeric <- lapply(strsplit(data[, i], ","), FUN = as.numeric)#Convert c("1,2,3,4") to c(1,2,3,4) in MA question
      #print(vector_numeric)
      ith_variable <- code_spec[, 1] == i
      matched_list <- lapply(vector_numeric, function(x)
        match(x, code_spec[ith_variable, 2]))
      #print(matched_list)
      results <- lapply(matched_list, function(x)
        code_spec[ith_variable, 3][x])
      results <- lapply(results, function(x)
        paste(x, collapse=","))
      results <- unlist(results)
      results <- ifelse(results=="NA", "", results)#Turn "NA" string into empty string
      #print(results)
      data <- mutate_at(data, c(i), function(x) {x <- results})
    } else {#This condition handles SA question
      message(paste('The variable "', i, '" is a SA question!', sep=""))
      results <- code_spec[code_spec[, 1] == i, ]
      results <- code_spec[code_spec[, 1] == i, 3][match(data[, i], results[, 2])]
      data <- mutate_at(data, c(i), function(x) {x <- results})
    }
  }
  return(data)
}







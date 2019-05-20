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
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check if the vector represent a SA or MA question; Check for MA condition
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
#' @export dup_remove
#Remove duplicate code
dup_remove  <- function(vector) {
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check for MA
    results <- sapply(strsplit(vector, ",", fixed = TRUE), function(x)
      paste(unique(x), collapse = ","))
    ifelse(results == "NA", "", results)#Just to turn NA string into empty string
  } else {
    stop("NOT applicable to SA question")
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
#' datafrane with 2 columns
#'
#' @details
#' The raw and coded dataframes should have one common column for SN mathing. If no need for SN matching, just populate both SN columns with the same vector.
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








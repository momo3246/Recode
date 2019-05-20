#' @title Recodes or transforms data collected from market reserach study
#'
#' @description This package performs various operations in data transfomation commonly seen in market reserach study
#'
#' @param vector
#'
#' @return results
#'
#' @examples
#'
#' @export excess_comma

#Remove any trailing, leading as well as consecutive commma in between
excess_comma <- function(vector) {
  results <- gsub("^,*|(?<=,),|,*$", "", vector, perl=T)
  ifelse(is.na(results), "", results)#Just to turn NA (NOT "NA" string) into empty string
}

#' @title Recodes or transforms data collected from market reserach study
#'
#' @description This package performs various operations in data transfomation commonly seen in market reserach study
#'
#' @param vector
#'
#' @param remove
#'
#' @return NULL
#'
#' @examples
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

#' @title Recodes or transforms data collected from market reserach study
#'
#' @description This package performs various operations in data transfomation commonly seen in market reserach study
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

#' @title Recodes or transforms data collected from market reserach study
#'
#' @description This package performs various operations in data transfomation commonly seen in market reserach study
#'
#' @param vector
#'
#' @return results
#'
#' @examples
#'
#' @export dup_remove
#Remove duplicate code
dup_remove  <- function(vector) {
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check for MA
    results <- sapply(strsplit(vector, ",", fixed = TRUE), function(x)
      paste(unique(x), collapse = ","))
    results <- ifelse(results == "NA", "", results)#Just to turn NA string into empty string
  } else {
    stop("NOT applicable to SA question")
  }
}

#' @title Recodes or transforms data collected from market reserach study
#'
#' @description This package performs various operations in data transfomation commonly seen in market reserach study
#'
#' @param raw
#'
#' @param coded
#'
#' @return recoded
#'
#' @examples
#'
#' @import stringr
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
    recoded <- dup_remove(recoded)#Remove any duplicates in resulted recoded data
  } else {
    message("You are processing a SA question")
    combined <- join(raw, coded)
    raw_without_97 <- ifelse(str_detect(combined[, 2], "97") & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], 97), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
    recoded <- ifelse(str_detect(combined[, 2], "97") & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
    recoded <- excess_comma(recoded)#Do not remove duplicate for SA question
  }
}








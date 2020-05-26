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
#' excess_comma(c(",,1,,,2,,3,,,"))# "1,2,3"
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
#' remove_code(c("1", "2,4", "3,5"), remove=c(2,3))#remove more than 1 code in MA question; "1" "4" "5"
#' remove_code(c("1", "2,4", "3,5"), remove=2)#remove only 1 code in MA question; "1"   "4"   "3,5"
#' remove_code(c(1, 2, 3), remove=3)#remove 1 code in SA question; "1" "2" ""
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
#' character vector (MA question) or single numeric value (SA question)
#'
#' @param remove
#' character string (only 1 code to remove) or character vector (more than 1 code to remove)
#'
#' @return NULL
#'
#' @examples

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
#' dup_remove(c("1,2,3,2,3,3"))# "1,2,3"
#'
#' @import stringr
#'
#' @export dup_remove
#Remove duplicate code
dup_remove  <- function(vector) {
  if ( any(str_detect(vector, ",")==TRUE, na.rm = TRUE) ) {#Check for MA
    message("You are processing a MA question")
    results <- sapply(strsplit(vector, ",", fixed = TRUE), function(x)
      paste(unique(x), collapse = ","))
    ifelse(results == "NA", "", results)#Just to turn NA string into empty string
  } else {
    message("You are processing a SA question")
    print(vector)
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
#Remove duplicate code
dup_remove_no_message  <- function(vector) {
  if ( any(str_detect(vector, ",")==TRUE, na.rm = TRUE) ) {#Check for MA
    results <- sapply(strsplit(vector, ",", fixed = TRUE), function(x)
      paste(unique(x), collapse = ","))
    ifelse(results == "NA", "", results)#Just to turn NA string into empty string
  } else {
    print(vector)
  }
}

#' @title Back code OTHERS into pre-coded question
#'
#' @description This function is to back code verbatim into a pre-coded question
#'
#' @param raw
#' dataframe with 2 columns (for backcoding with SN matching); vector (for backcoding without SN mathcing)
#'
#' @param coded
#' dataframe with 2 columns (for backcoding with SN matching); vector (for backcoding without SN mathcing)
#'
#' @param others_code
#' Single integer
#'
#' @param SN_matching
#' Logical
#'
#' @details
#' Inputs of this function vary upon usage; If no need for SN matching, set FALSE in the argument "SN_matching", or vice
#'
#'
#' @examples
#' raw <- data.frame(SN=c(1, 2000, 3, 4), raw_data=c("1,2,97", "1,3", "97", "1,2,97"), stringsAsFactors=FALSE)##Populate a dataframe
#' coded <- data.frame(SN=c(2000, 1, 3, 4), coded_data=c(NA, "2,3", "9", "97"), stringsAsFactors=FALSE)##Populate another dataframe
#' back_code(raw, coded, others_code = 97, SN_matching = TRUE)
#' #[[1]] "1,2,3"  "1,3"    "9"      "1,2,97";
#' #[[2]]
#' #SN raw_data coded_data results
#' #1    1   1,2,97        2,3   1,2,3
#' #2 2000      1,3       <NA>     1,3
#' #3    3       97          9       9
#' #4    4   1,2,97         97  1,2,97
#'
#'# For matched SN
#'raw1 <- data.frame(raw_data=c("1,2,97", "1,3", "97", "1,2,97"), stringsAsFactors=FALSE)##Populate a dataframe
#'coded1 <- data.frame(coded_data=c("5", NA, "9", "8"), stringsAsFactors=FALSE)##Populate another dataframe
#'back_code(raw1$raw_data, coded1$coded_data, others_code = 97, SN_matching = FALSE)
#'#[[1]]
#'#[1] "1,2,5" "1,3"   "9"     "1,2,8"
#'
#'#[[2]]
#'#raw      coded results
#'#[1,] "1,2,97" "5"   "1,2,5"
#'#[2,] "1,3"    NA    "1,3"
#'#[3,] "97"     "9"   "9"
#'#[4,] "1,2,97" "8"   "1,2,8"
#'
#'# For OTHERS code other than code 97
#'raw2 <- data.frame(raw_data=c("1,2,91", "1,3", "91", "1,2,91"), stringsAsFactors=FALSE)##Populate a dataframe
#'coded2 <- data.frame(coded_data=c("5", NA, "9", "8"), stringsAsFactors=FALSE)##Populate another dataframe
#'back_code(raw2$raw_data, coded2$coded_data, others_code = 91, SN_matching = FALSE)
#'
#' @import stringr
#'
#' @import plyr
#'
#' @export back_code
#Back code OTHERS into pre-coded question
back_code <- function(raw, coded, others_code, SN_matching) {
  if ( SN_matching == TRUE) {
    if ( any(str_detect(raw[, 2], ",")==TRUE, na.rm = TRUE) ) {#Check for MA
      message("You are processing a MA question")
      combined <- join(raw, coded)
      raw_without_97 <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], others_code), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      recoded <- excess_comma(recoded)#Remove excess comma
      single_results <- dup_remove_no_message(recoded)#Remove any duplicates in resulted recoded data
      detailed_results <- cbind(merge(raw, coded, sort = FALSE), results = single_results)
      return(list(single_results, detailed_results))
    } else {
      message("You are processing a SA question")
      combined <- join(raw, coded)
      raw_without_97 <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], others_code), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      single_results <- excess_comma(recoded)#Do not remove duplicate for SA question
      detailed_results <- cbind(merge(raw, coded, sort = FALSE), results = single_results)
      return(list(single_results, detailed_results))
      }
  } else {
    if ( any(str_detect(raw, ",")==TRUE, na.rm = TRUE) ) {#Check for MA
      message("You are processing a MA question")
      raw_without_97 <- ifelse(check_code(raw, others_code) & !is.na(coded), remove_code_no_message(raw, others_code), raw)#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(raw, others_code) & !is.na(coded), paste(raw_without_97, coded, sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      recoded <- excess_comma(recoded)#Remove excess comma
      single_results <- dup_remove_no_message(recoded)#Remove any duplicates in resulted recoded data
      detailed_results <- cbind(raw = raw, coded = coded, results = single_results)
      return(list(single_results, detailed_results))
    } else {
      message("You are processing a SA question")
      raw_without_97 <- ifelse(check_code(raw, others_code) & !is.na(coded), remove_code_no_message(raw, others_code), raw)#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(raw, others_code) & !is.na(coded), paste(raw_without_97, coded, sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      single_results <- excess_comma(recoded)#Do not remove duplicate for SA question
      detailed_results <- cbind(raw = raw, coded = coded, results = single_results)
      return(list(single_results, detailed_results))
      }
    }
}

##Return a list in which consists of a dataframe (contains SN, raw data, coded data and results) and a vector (contains results only)

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
#' combine(vector1, vector2) #"1,3"   "2,3,4" ""      "1"
#’
#'@export combine
#'
combine <- function(vector1, vector2) {
  vector1 <- ifelse(is.na(vector1), "", vector1)#Replace NA with empty string
  vector2 <- ifelse(is.na(vector2), "", vector2)#Replace NA with empty string
  combined <- ifelse(vector1=="" & vector2=="", "", #Check for both empty string
                     ifelse(vector1 != "" & vector2 != "", dup_remove(paste(vector1, vector2, sep=",")), #Check for neither empty string
                            excess_comma(paste(vector1, vector2, sep=",")))) #Remainder falls into the category of either one is empty string
  return(combined)
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

#' @title Check particular code
#'
#' @description This function is to check if a particualr code exists
#'
#' @param vector
#' character vector (for MA question); numeric vector (for SA question)
#'
#' @param code
#' integer
#'
#' @return logical vector
#'
#' @examples
#' check_code(c("1,2,33"), 3) #FALSE
#'
#' @export check_code

#Check code in a numeric manner
check_code <- function(vector, code) {
  vector_numeric = lapply(strsplit(vector, ","), FUN = as.numeric)
  results = lapply(vector_numeric, function(x) any(ifelse(x == code, TRUE, FALSE)))
  results = unlist(results)
  return(results)
}

#' @title Get variables for tabulation
#'
#' @description This function is to get variables for tabulation
#'
#' @param start_with
#' character
#'
#' @return list
#'
#' @examples
#' get_tab_var("Q")
#'
#' @import stringr
#'
#' @export get_tab_var

get_tab_var = function(start_with) {
  variable_names_all = ls(envir = .GlobalEnv)
  variable_names_tab.v = variable_names_all[str_starts(variable_names_all, start_with)]
  variable_names_tab.l = sapply(variable_names_tab.v, get)
  return(variable_names_tab.l)
}

#' @title Check if the variable has unique answer
#'
#' @description This function is to check if the variable contains unique answer. Return TRUE when it does while return FALSE when it does not. For example, DK (code 98) should exist in a MA question alone; if DK (code 98) exist along with others codes in a MA question, the function will return FALSE.
#'
#' @param vector
#' variable to be checked
#'
#' @param unique_code
#' specify unique code
#'
#' @return vector
#'
#' @examples
#' check_unique(c("1,2,3", "1,2", "1"), 1)#FALSE FALSE TRUE
#'
#' @export check_unique

check_unique <- function(vector, unique_code) {
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check if the vector represent a SA or MA question; Check for MA condition
    message("You are processing a MA question")
    vector_numeric <- lapply(strsplit(vector, ","), FUN = as.numeric)
    results <- lapply(vector_numeric, function(x)
      ifelse(length(x)>1 && (unique_code %in% x), FALSE, TRUE))
    results <- unlist(results)
    return(results)
  } else {#SA condition
    message("You are processing a SA question. All answers should be unique")
  }
}

#' @title Replace particular code(s)
#'
#' @description This function is to replace code(s) in either SA or MA question
#'
#' @param vector
#' character vector (MA question) or single numeric value (SA question)
#'
#' @param to_be_replaced
#' character string (only 1 code to be replaced) or character vector (more than 1 code to be replaced)
#'
#' @param replacement
#' character string (only 1 code as replacement) or character vector (more than 1 code as replacement)
#'
#' @examples
#' replace_code(c("1", "2,4", "3,5"), 1, 2)#"2"   "2,4" "3,5"
#' replace_code(c(1, 2, 3), 1, 99)#99  2  3
#'
#' @export replace_code

#Can remove more than 1 answer
replace_code <- function(vector, to_be_replaced, replacement) {
  if ( any(str_detect(vector, ",")==TRUE) ) {#Check if the vector represents a SA or MA question; Check for MA condition
    message("You are processing a MA question")
    vector_numeric <- lapply(strsplit(vector, ","), FUN = as.numeric)
    results <- lapply(vector_numeric, function(x)
      paste(replace(x, x==to_be_replaced, replacement), collapse=","))
    results <- unlist(results)
    ifelse(results == "NA", "", results)#Just to turn "NA" string into empty string
  } else {#SA condition
    message("You are processing a SA question")
    results <- replace(vector, vector==to_be_replaced, replacement)#Replace element to be removed by empty string
    ifelse(is.na(results), "", results)#Just to turn NA (NOT "NA" string) into empty string
  }
}

#' @title Shift elements in vector by specific distance and direction
#'
#' @description This function is to shift elements in vector by specific distance and direction
#'
#' @param vector
#' numeric or character vector
#'
#' @param distance
#' integer
#'
#' @param direction
#' c("left", "right")
#'
#' @examples
#' shifter(1:4, 0, "left")#1 2 3 4
#' shifter(1:4, 1, "left")#2 3 4 1
#' shifter(1:4, 1, "left")#Distance to be shifted exceeds length of vector
#'
#' @export shifter

# Reference: https://stackoverflow.com/questions/30542128/circular-shifting-arrays-in-r-by-distance-n
shifter <- function(vector=x, distance=n, direction = c("left", "right")) {
  if (distance > length(vector)) {
    stop("Distance to be shifted exceeds length of vector")
  }
  if (direction == "left") {
    if (distance == 0) {
      results = vector
      return(results)
    } else {
      results = c(tail(vector, -distance), head(vector, distance))
      return(results)
    }
  } else if (direction == "right") {
    if (distance == 0) {
      results = vector
      return(results)
    } else {
      results = c(tail(vector, distance), head(vector, -distance))
      return(results)
    }
  }
}

#' @title Create rotational combination in JS array format
#'
#' @description This function is exclusively used for Rail Gen 2.0 project series to create rotational combination in JS array format
#'
#' @param vector
#' numeric vector
#'
#' @param direction
#' c("left", "right")
#'
#' @examples
#' rotate_JS(c(1:4), "right")#"1:[0,1,2,3,4]," "2:[0,4,1,2,3]," "3:[0,3,4,1,2]," "4:[0,2,3,4,1],"
#' rotate_JS(c(1:4), "left")#"1:[0,1,2,3,4]," "2:[0,2,3,4,1]," "3:[0,3,4,1,2]," "4:[0,4,1,2,3],"
#'
#' @import tibble
#'
#' @export rotate_JS

rotate_JS <- function (vector=x, direction=c("left", "right")) {
  # Initiate empty dataframe
  df = data.frame(matrix(NA, nrow = length(vector), ncol = length(vector)))
  # Rotate the elements and put each rotation into the dataframe
  if (direction == "right") {
    for (n in 1:length(vector)) {
      df[ , n] = shifter(vector, n-1, "right")
    }
  } else if (direction == "left") {
    for (n in 1:length(vector)) {
      df[ , n] = shifter(vector, n-1, "left")
    }
  }
  # Transpose the dataframe to get the correct orientation
  df = t(df)
  # Append the columns "row_id" and "dummy"
  df = as.data.frame(cbind("row_id"=c(1:length(vector)), "dummy"=rep(0, length(vector)), df))
  # Append the first syntax column with value ":["
  df1 = add_column(df, "syntax1" = rep(":[", length(vector)), .after = 1)# Fixed

  total_no_of_cols = 2*length(vector)+4# No. of columns in total
  index_vector = c(3:total_no_of_cols)# Second syntax column starts from column 3
  index_vector_r = index_vector[c(TRUE, FALSE)]# Only alternate columns need syntax column

  #Reference from https://stackoverflow.com/questions/45741498/add-column-in-tibble-with-variable-column-name
  #Append syntax columns with value ","
  for (i in head(seq_along(index_vector_r), -1)) {
    df1 = add_column(df1, !!(paste("syntax", i+1, sep="")) := rep(",", length(vector)), .after = index_vector_r[i])
  }

  #Append the last syntax column with value "],"
  df2 = add_column(df1, !!(paste("syntax", length(vector)+2, sep="")) := rep("],", length(vector)), .after = total_no_of_cols-1)# Fixed

  return(apply(df2, 1, paste, collapse=""))
}

#' @title Back code OTHERS into pre-coded question (compatible with pipe operator)
#'
#' @description This function is to back code verbatim into a pre-coded question
#'
#' @param raw
#' dataframe with 2 columns (for backcoding with SN matching); vector (for backcoding without SN mathcing)
#'
#' @param coded
#' dataframe with 2 columns (for backcoding with SN matching); vector (for backcoding without SN mathcing)
#'
#' @param others_code
#' Single integer
#'
#' @param SN_matching
#' Logical
#'
#' @details
#' Inputs of this function vary upon usage; If no need for SN matching, set FALSE in the argument "SN_matching", or vice
#'
#'
#' @examples
#' raw <- data.frame(SN=c(1, 2000, 3, 4), raw_data=c("1,2,97", "1,3", "97", "1,2,97"), stringsAsFactors=FALSE)##Populate a dataframe
#' coded <- data.frame(SN=c(2000, 1, 3, 4), coded_data=c(NA, "2,3", "9", "97"), stringsAsFactors=FALSE)##Populate another dataframe
#' back_code(raw, coded, others_code = 97, SN_matching = TRUE)
#' #[[1]] "1,2,3"  "1,3"    "9"      "1,2,97";
#' #[[2]]
#' #SN raw_data coded_data results
#' #1    1   1,2,97        2,3   1,2,3
#' #2 2000      1,3       <NA>     1,3
#' #3    3       97          9       9
#' #4    4   1,2,97         97  1,2,97
#'
#'# For matched SN
#'raw1 <- data.frame(raw_data=c("1,2,97", "1,3", "97", "1,2,97"), stringsAsFactors=FALSE)##Populate a dataframe
#'coded1 <- data.frame(coded_data=c("5", NA, "9", "8"), stringsAsFactors=FALSE)##Populate another dataframe
#'back_code(raw1$raw_data, coded1$coded_data, others_code = 97, SN_matching = FALSE)
#'#[[1]]
#'#[1] "1,2,5" "1,3"   "9"     "1,2,8"
#'
#'#[[2]]
#'#raw      coded results
#'#[1,] "1,2,97" "5"   "1,2,5"
#'#[2,] "1,3"    NA    "1,3"
#'#[3,] "97"     "9"   "9"
#'#[4,] "1,2,97" "8"   "1,2,8"
#'
#'# For OTHERS code other than code 97
#'raw2 <- data.frame(raw_data=c("1,2,91", "1,3", "91", "1,2,91"), stringsAsFactors=FALSE)##Populate a dataframe
#'coded2 <- data.frame(coded_data=c("5", NA, "9", "8"), stringsAsFactors=FALSE)##Populate another dataframe
#'back_code(raw2$raw_data, coded2$coded_data, others_code = 91, SN_matching = FALSE)
#'
#' @import stringr
#'
#' @import plyr
#'
#' @export back_code_v
#'
#returns single results only, which can be used in pipe operator subsequently
back_code_v <- function(raw, coded, others_code, SN_matching) {
  if ( SN_matching == TRUE) {
    if ( any(str_detect(raw[, 2], ",")==TRUE, na.rm = TRUE) ) {#Check for MA
      message("You are processing a MA question")
      combined <- join(raw, coded)
      raw_without_97 <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], others_code), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      recoded <- excess_comma(recoded)#Remove excess comma
      single_results <- dup_remove_no_message(recoded)#Remove any duplicates in resulted recoded data
      #detailed_results <- cbind(merge(raw, coded, sort = FALSE), results = single_results)
      return(single_results)
    } else {
      message("You are processing a SA question")
      combined <- join(raw, coded)
      raw_without_97 <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), remove_code_no_message(combined[, 2], others_code), combined[, 2])#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(combined[, 2], others_code) & !is.na(combined[, 3]), paste(raw_without_97, combined[, 3], sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      single_results <- excess_comma(recoded)#Do not remove duplicate for SA question
      #detailed_results <- cbind(merge(raw, coded, sort = FALSE), results = single_results)
      return(single_results)
    }
  } else {
    if ( any(str_detect(raw, ",")==TRUE, na.rm = TRUE) ) {#Check for MA
      message("You are processing a MA question")
      raw_without_97 <- ifelse(check_code(raw, others_code) & !is.na(coded), remove_code_no_message(raw, others_code), raw)#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(raw, others_code) & !is.na(coded), paste(raw_without_97, coded, sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      recoded <- excess_comma(recoded)#Remove excess comma
      single_results <- dup_remove_no_message(recoded)#Remove any duplicates in resulted recoded data
      #detailed_results <- cbind(raw = raw, coded = coded, results = single_results)
      return(single_results)
    } else {
      message("You are processing a SA question")
      raw_without_97 <- ifelse(check_code(raw, others_code) & !is.na(coded), remove_code_no_message(raw, others_code), raw)#Remove code "97" if raw data has it and coded data is not NA
      recoded <- ifelse(check_code(raw, others_code) & !is.na(coded), paste(raw_without_97, coded, sep=","), raw_without_97)#Paste coded data to raw data (but without code "97" in some cases according to condtion as above)
      single_results <- excess_comma(recoded)#Do not remove duplicate for SA question
      #detailed_results <- cbind(raw = raw, coded = coded, results = single_results)
      return(single_results)
    }
  }
}

#' @title Get the number of mentions in a MA response
#'
#' @description This function is to Get the number of mentions in a MA response
#'
#' @param vector
#' character vector (MA question)
#'
#' @examples
#' mentions(c("1", "2,4", "3,5"))#1 2 2
#'
#' @export mentions
#'
mentions <- function(vector) {
  vector_temp = ifelse(is.na(vector)==TRUE, "", vector)
  vector_numeric = lapply(strsplit(vector_temp, ","), FUN = as.numeric)
  results = lapply(vector_numeric, length)
  results = unlist(results)
  return(results)
}

#' @title Transform ranking variables
#'
#' @description This function is to transform ranking variables to comma-separated MA question
#'
#' @param vector
#' character vector
#'
#' @param no_R
#' single integer
#'
#' @examples
#' #Create a ranking dataframe
#' ranking_df = data.frame(attribute1 = c("3", "3", "2", "2"), attribute2 = c("1", "1", "1", "3"), attribute3 = c("2", "2", "4", "1"), attribute4 = c("4", "4", "3", "4"))
#' #Apply the function rowwise
#' apply(ranking_df, 1, rank_trans, no_R = 4)#"2,3,1,4" "2,3,1,4" "2,1,4,3" "3,1,2,4"; the first response means rank 1, the second means rank 2 and so on
#'
#' @details
#' Argument "no_R" indicates how many attributes the respondents should rank
#' When codes of attributes are not consecutive, e.g. attribute 4 corresponds to code 8, instead of code 4, function replace_code() can be used to replace those code 4 by code 8
#'
#' @export rank_trans
#'
rank_trans = function (vector, no_R) {
  result = c()
  for (i in 1:no_R) {
    result = append(result, match(i, vector))
  }
  result = result[!is.na(result)]
  return(paste(as.character(result), collapse =","))
}




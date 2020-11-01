## Reading in text files
input <- read.delim("https://raw.github.com/jdk1114/Ninety-One/main/Examples.txt", header = FALSE, stringsAsFactors = FALSE)

## Manually entering an input
# input <- "The pump is 10022 KGs deep underground"

## disabling scientific notation 
options(scipen = 999)

## Creating vectors where the word is the name of the element and the number is the element
singleDigits <- c(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, nine = 9)
teens <- c(eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16,
           seventeen = 17, eighteen = 18, nineteen = 19)
tens = c(ten = 10, twenty = 20, thirty = 30, forty = 40, fifty = 50, sixty = 60, seventy = 70, eighty = 80, ninety = 90)
doubleDigits <- c(teens, tens)
magnitudes <- c("thousand", "million", "billion", "trillion")

## Convert input to a data.frame to ensure consistency in data type when either typing in an input or reading in multiple inputs from a text file
input <- data.frame(input, stringsAsFactors = FALSE)

## Function that converts numbers to words
numbersToWords <- function(textInput) {
  ## Extracting number from input and checking whether it is valid
  numberInput <- gsub(".*?([0-9]+).*", "\\1", textInput)
  
  ## Check whether number is valid - first initialise output to NULL
  output <- NULL
  
  ## 1. There should only be one continuous string of numbers
  digitStartLocations <- gregexpr("([0-9]+)", textInput)[[1]]
  
  if (length(digitStartLocations) > 1) {
    output <- "number invalid"
  } 
  
  ## 2. There should not be any characters directly before or after the number
  # To find end locations of digits need to reverse the order of the string
  reversedInput <- paste(rev(strsplit(textInput, "")[[1]]), collapse = "")
  digitEndLocations <- sort(nchar(textInput) - gregexpr("([0-9]+)", reversedInput)[[1]] + 1)
  if (digitStartLocations[1] != 1) {
    if (strsplit(textInput, "")[[1]][digitStartLocations[1] - 1] != " ") {
      output <- "number invalid"
    }
  }
  if (digitEndLocations[1] != nchar(textInput)) {
    if (strsplit(textInput, "")[[1]][digitEndLocations[1] + 1] != " ") {
      output <- "number invalid"
    }
  }
  
  #####################
  ## Operations if number is  valid
  if (is.null(output)) {
    ## Separating the digits into a vector to 
    digitsSplit <- strsplit(numberInput, "")[[1]]
    
    # Determine length of number to call correct functions in operations
    numberOfDigits <- length(digitsSplit)
    
    ## Remove first digit until it is not zero if length is greater than one
    while(digitsSplit[1] %in% "0" & numberOfDigits > 1) {
      digitsSplit <- digitsSplit[-1]
      numberOfDigits <- length(digitsSplit)
    }
    
    ## Functions that return numbers as words
    singleDigitsFn <- function(digits) {
      names(singleDigits)[singleDigits == as.numeric(digits)]
    }
    
    doubleDigitFn <- function(digits) {
      ## If two digits are in the stored numbers then use these. Otherwise, use first digit and tens
      ifelse(as.numeric(paste(digits, collapse = "")) %in% doubleDigits,
             names(doubleDigits)[doubleDigits == as.numeric(paste(digits, collapse = ""))],
             paste(names(tens)[tens == as.numeric(paste0(digits[1], 0))],
                   ifelse(digits[2] != "0", singleDigitsFn(digits[2]), ""),
                   sep = ifelse(!(digits[1] %in% "0"), "-", ""))
      )
    }
    
    hundredsFn <- function(digits) {
      ## Calculate what the last two digits will output
      lastDigits <- doubleDigitFn(digits[-1])
      
      ## Return the number to prefix the word 'hundred' with
      paste0(ifelse(!(digits[1] %in% "0"),
                    paste(singleDigitsFn(digits[1]), "hundred"),
                    ""
      ), 
      ## If the last two digits are both not zero then include 'and' to combine the first digit and the last two digits
      paste0(ifelse(digits[1] != 0 & lastDigits != "", " and ", ""),
             ifelse(lastDigits != "", lastDigits, "")
      )
      )
    }
    
    magnitudesFn <- function(digits) {
      ## Used for any numbers greater than or equal to 1000.
      
      ## Calculate the modular to determine whether it is a single digit, a double digit or a triple digit magnitude prefix
      prefixMagnitude <- (length(digits) - 1) %% 3 + 1
      
      ## If all the digits sent to the function are zero then return nothing
      if (!(all(digits[1:prefixMagnitude] %in% "0"))) {
        ## Find the first values that are non-zero
        digits <- strsplit(as.character(as.numeric(paste(digits, collapse = ""))), "")[[1]]
        
        ## Calculate the modular to determine whether it is a single digit, a double digit or a triple digit magnitude prefix
        prefixMagnitude <- (length(digits) - 1) %% 3 + 1
        
        ## Choose prefix from list depending on the number of digits in the number, e.g. 4, 5 or 6 digits is in the thousands, 7, 8 or 9 digits is in the millions, ...
        prefix <- magnitudes[floor((length(digits) - 1) / 3)]
        
        output <- paste(switch(as.character(prefixMagnitude),
                               "1" = singleDigitsFn(digits[1]), 
                               "2" = doubleDigitFn(digits[1:2]),
                               "3" = hundredsFn(digits[1:3])),
                        prefix
        )
      } else {
        output <- ""
      }
    }
    
    ## Call the appropriate function based on the length of the digits
    if (numberOfDigits == 1) {
      output <- singleDigitsFn(digitsSplit) 
    } else if (numberOfDigits == 2) {
      output <- doubleDigitFn(digitsSplit)
    } else if (numberOfDigits == 3) {
      output <- hundredsFn(digitsSplit)
    } else {
      ## Calculate the modular to determine whether it is a single digit, a double digit or a triple digit magnitude prefix
      prefixMagnitude <- (length(digitsSplit) - 1) %% 3 + 1
      
      output <- magnitudesFn(digitsSplit)
      
      ## Creating a while loop which will run as long as there are still more than 3 digits left to assign wording to
      updatedDigitsSplit <- digitsSplit[-(1:prefixMagnitude)]
      
      while (length(updatedDigitsSplit) > 3) {
        ## Create new variable with next output stored
        nextOutput <- magnitudesFn(updatedDigitsSplit)
        output <- paste(output, nextOutput, sep = ifelse(nextOutput != "", ", ", ""))
        
        prefixMagnitude <- (length(updatedDigitsSplit) - 1) %% 3 + 1
        updatedDigitsSplit <- updatedDigitsSplit[-(1:prefixMagnitude)]
      }
      ## Create new variable with next output stored
      nextOutput <- hundredsFn(updatedDigitsSplit)
      output <- paste(output, nextOutput, sep = ifelse(nextOutput != "", 
                                                       ifelse(updatedDigitsSplit[1] %in% "0", 
                                                              " and ",
                                                              ", "
                                                       ), "")
      )
    }
  }
  
  return(output)
}

## Looping over each input value
output <- sapply(1:nrow(input), function(i) numbersToWords(input[i,]))

output

rankall <- function(outcome, num = "best") {
  ## Reads outcome data 
  
  file_data <- read.csv("C:/Chris/R/hospital/outcome-of-care-measures.csv", sep = ",")
  ## Checks that state and outcome are valid 
  
  valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia") 
  if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
  
  
  header_name <- NULL 
  
   if (outcome == "heart attack") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  else if (outcome == "heart failure") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  else header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 

  hosps <- c() 

  states <- c() 
 
  ## For each state, finds the hospital of the given rank 
  for (state in valid_states) { 
    
    ranked_hosp <- c() 
    data <- file_data[file_data$State == state,]         
    sorted_data <- data[order(as.numeric(as.character(data[,header_name])), as.character(data[,"Hospital.Name"])),] 
    sorted_data <- sorted_data[!sorted_data[,header_name] == "Not Available",] 
    if (num == "best") { 
      ranked_hosp <- best(state, outcome) 
    } else if (num == "worst") { 
      ranked_hosp <- as.character(tail(sorted_data[,"Hospital.Name"], n = 1)) 
    } else { 
      ranked_hosp <- as.character(sorted_data[,"Hospital.Name"][num]) 
    } 
    hosps <- c(hosps, ranked_hosp) 
  } 
  result <- data.frame(hosps, valid_states) 
  colnames(result) <- c("hospital", "state") 
  return(result) 
} 

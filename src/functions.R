library(tidyverse)
library(stringr)


#
# Report whether a drug appears between start and end date
#
# Inputs:
# presc       prescriptions in common format
# bnf_code    bnf code for drug to the level of interest (e.g. '01', '0101', '0101010G0BNAABY', etc)
# start_date  start date for period of interest
# end_date    end date for period of interest
#
ever_used <- function(presc, bnf_code, start_date, end_date){

  # Pre-conditions
  stopifnot(is.data.frame(presc))
  stopifnot(end_date >= start_date)
  stopifnot(is.character(bnf_code))
  stopifnot(str_length(bnf_code) <= 15) # Note: allow 0 length string for has any prescription

  
  return (
  presc %>%
    filter(disp_date > start_date & disp_date < end_date) %>% 
    select(prochi, disp_date, bnf_item_code) %>%            
    mutate(is_match = ifelse(substr(bnf_item_code, 1, str_length(bnf_code)) == bnf_code, 1, NA)) %>%
    group_by(prochi) %>%
    summarise(has_drug=ifelse(sum(!is.na(is_match))>0, 1, 0)) 
  )

  
}



#
# Construct a df summarising number of prescriptions at specific level of BNF
#
# Inputs:
# presc       prescriptions in common format
# bnf_level   Level of bnf code to summarise at (e.g. 2 = '01', 4 = '0101', 15='0101010G0BNAABY', etc)
# start_date  start date for period of interest
# end_date    end date for period of interest
#
# Outputs:
# data frame (prochino, n1, n2, n3, nY)
# n1 - nY = number of distinct *days* on which a prescription within a specific
# level/section of the BNF has been prescribed.
#
count_bnf_class <- function(presc, start_date, end_date, bnf_level=2){
  
  # Pre-conditions
  stopifnot(is.data.frame(presc))
  stopifnot(end_date >= start_date)
  stopifnot(is.numeric(bnf_level))
  stopifnot(bnf_level <= 15) 
  
  
  return (
    presc %>%
    filter(disp_date > start_date & disp_date < end_date) %>%
    select(prochi, disp_date, bnf_item_code) %>%  
    distinct() %>%
    mutate(level=substr(bnf_item_code, 1, bnf_level)) %>%
    group_by(prochi, level) %>%
    tally() %>%
    spread(level, n, fill=0)
  )
}




#
# Construct an index of polypharmacy.
#
# Here the default is assumed to be number of bnf chapters represented in timeframe.
#
# Inputs:
# presc       prescriptions in common format
# start_date  start date for period of interest
# end_date    end date for period of interest
# bnf_level   Level of bnf code to index at. Default is across chapters.
#
# Outputs:
# data frame (prochino, poly_index)
# poly_index will reflect the total number of sections represented at the requested level.
# For chapter level, this can be 0-23. Other levels will have different possible totals.
#

polypharmacy <- function(presc, start_date, end_date, bnf_level=2){

  
  # Pre-conditions
  stopifnot(is.data.frame(presc))
  stopifnot(end_date >= start_date)
  stopifnot(is.numeric(bnf_level))
  stopifnot(bnf_level <= 15) 
  
  # Retro R since I can't figure out dplyr syntax
  
  # Calculate poly_index for every subject with a prescription
  bnf_counts <- count_bnf_class(presc, start_date, end_date, bnf_level)
  bnf_counts[,2:ncol(bnf_counts)] = ifelse(bnf_counts[,2:ncol(bnf_counts)] == 0, 0, 1)
  bnf_counts$poly_index <- rowSums(bnf_counts[,2:ncol(bnf_counts)])
  
  # Create a df containing all the subjects
  all_bnf_counts <- presc %>%
    select(prochi) %>%
    unique %>%
    left_join(bnf_counts, by="prochi")

  # Set NA poly_indices to 0
  all_bnf_counts[is.na(all_bnf_counts$poly_index),]$poly_index = 0
  
  return(all_bnf_counts %>% select(prochi, poly_index))
  
}




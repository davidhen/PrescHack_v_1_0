#
# Driver for functions written on Prescription hack meeting.
#
# Source this to View the output of each function call
#

rm(list=ls())

# load data locally
source('~/PrescHack/src/load_data.R', echo=TRUE)
       
# load the functions
source('~/PrescHack/src/functions.R', echo=TRUE)

# The source data contains some dodgy BNF codes. Remove these.
presc <- presc %>% filter(str_length(bnf_item_code) == 15)
View(presc)

# Get a list of subjects with prescriptions from chapter 2
# Note that specify
df_ever_used <- ever_used(presc, '02', '2005-01-01', '2006-01-01')
View(df_ever_used)


# Get count of prescriptions in each chapter
# Note that this could be at a different level (paragraph, sub-section)
# Only reports on subjects that have at least one prescription in chapter.
# Could add 0 counts for all other subjects in or after call.
df_bnf_counts <- count_bnf_class(presc, '2005-01-01', '2005-06-12')
View(df_bnf_counts)

# Retrieve an index of polypharmacy for each subject
# This function returns a status for every subject in the source dataset
df_polypharmacy <- polypharmacy(presc, '2005-01-01', '2006-01-01')
View(df_polypharmacy)
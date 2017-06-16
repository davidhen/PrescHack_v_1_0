library(readr)

# read the csv file into a dataframe
presc <- read_csv("~/PrescHack/data/Prescribing2.csv",
                col_types = cols(DISP_DATE = col_date(format = "%Y-%m-%d"),
                PAID_DATE = col_date(format = "%Y-%m-%d"),
                PRESC_DATE = col_date(format = "%Y-%m-%d")))


# lower case all the field names
names(presc) <- tolower(names(presc))



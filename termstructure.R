#========================================================================================
#                                                                     T E R M  -  S T R U C T U  R E     C O D E 
#                                                                        Lucas Dubois & Myriam  Lamborelle
#========================================================================================


#1.0  Libraries :
#========================================================================================

library(readsdmx)
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)

#2.0  Data Treatment :
#========================================================================================

path<- "/Users/lucasdubois/Desktop/LaTeX/MACRO&FI/Data/germanbonds.xlsx"
raw <- read_excel(path)
names(raw)

# Since we have too many "dates"  we can keep only the first one!
yields <- raw %>%
  select(
    date = Date...1,
    GER1M,
    GER3M,
    GER6M,
    GER1Y,
    GER2Y,
    GER3Y,
    GER5Y,
    GER10Y,
    GER20Y,
    GER30Y
  )

#Now, standirizing the date variable:
yields <- yields %>%
  mutate(
    date = as.Date(date),
    date = as.yearmon(date),
    date = as.Date(date, frac = 1)   # end of month
  )

# Check:
str(yields)
head(yields)
summary(yields)
names(yields)

#Check for NA:
sum(is.na(yields)) # No NAs :)

# 3.0 First PLOT:
#========================================================================================

selected <- yields %>%
  select(date, GER1M, GER1Y, GER5Y, GER10Y, GER30Y) %>%
  pivot_longer(-date, names_to = "maturity", values_to = "yield")

ggplot(selected, aes(date, yield, color = maturity)) +
  geom_line(linewidth = 0.9) +
  theme_minimal() +
  labs(
    title = "German Bond Yields",
    x = "",
    y = "Yield (%)"
  )


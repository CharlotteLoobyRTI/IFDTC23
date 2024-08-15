
install.packages("tidyverse")

library(tidyverse)



library(kableExtra)
library(ggplot2)
dataset <- PII_data1

#Import CSVs

dataset <- read.csv("...")

dataset <- read_csv("C:/Users/clooby/OneDrive - Research Triangle Institute/Documents/IFDTC_Materials/PII_toydata1.csv")

dataset <- read_csv("filepath.csv")

#De-identifying data

#Create id
dataset$id <- seq(1:nrow(dataset))

#Remove PII
dataset_deident <- dataset[ , !(names(dataset) %in% c("name", "dob", "race", "address", "address_pretty"))]


dataset_deident <- dataset %>%
  mutate(id = seq(1:nrow(dataset))) %>%
  select(-c(name, dob, race, address, address_pretty)) 



#Formatting dates

library(eeptools) #This is just for age_calc

dataset <- dataset %>%
  mutate(dob_new = as.Date(as.character(dob), format = "%m/%d/%Y")) %>%
  mutate(birthyear = year(dob_new)) %>%
  mutate(birthmonth_name = months(dob_new)) %>%
  mutate(age_days = as.numeric(Sys.Date() - dob_new)) %>%
  mutate(age = age_days / 365.25) %>%
  mutate(age_floor = floor(age))
  # mutate(age = age_calc(dob_new, Sys.Date(), units="years") %>% round(0))

head(dataset$dob)

dataset$dob_new <- as.Date(as.character(dataset$dob), format = "%m/%d/%Y")

dataset$dob_year <- year(dataset$dob_new)

head(dataset$dob_year)

dataset$dob_month_name <- months(dataset$dob_new)

head(dataset$dob_month_name)

dataset$dob_month <- month(dataset$dob_new)

head(dataset$dob_month)

dataset$dob_day <- day(dataset$dob_new)

head(dataset$dob_day)

dataset$age_days <- as.numeric(Sys.Date() - dob_new)

dataset$age <- age_days / 365.25

dataset$age_floor <- floor(age)



#Formatting addresses

library(campfin)
head(dataset$address)

#Make addresses pretty, extract state, check for valid state
dataset <- dataset %>% 
  mutate(address_pretty = normal_address(address)) %>%
  mutate(state = word(address_pretty, -2, sep = " ")) %>%
  mutate(state_valid = (state %in% state.abb))

dataset$address_pretty <- normal_address(dataset$address)

dataset$state <- word(dataset$address_pretty, -2, sep = " ")

dataset$state_valid <- (dataset$state %in% state.abb)

#Are there any addresses with invalid state?
table(dataset$state_valid, useNA='ifany')


#Check out the first six records with invalid state
dataset %>% 
  filter(state_valid==F) %>% 
  select(address_pretty, state) %>% 
  head

table(dataset$state[dataset$state_valid==FALSE], useNA='ifany')

head(dataset$address_pretty[dataset$state=="PE"])

#Fix with ifelse statement
dataset <- dataset %>%
  mutate(state = ifelse(state=="PE", "PA", state)) %>%
  mutate(state_valid = (state %in% state.abb))


dataset$state <- ifelse(dataset$state=="PE", 
                        "PA", 
                        dataset$state)

#Check again
table(dataset$state_valid)

dataset$state_valid <- (dataset$state %in% state.abb)

table(dataset$state_valid, useNA='ifany')


#Create summary statistics
children_count <- data.frame(table(dataset$children, useNA='ifany'))
names(children_count) <- c("Children", "Count")

children_count$Percent <- (children_count$Count / nrow(dataset))*100

children_count <- dataset %>%
  group_by(children) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / nrow(dataset))*100)



median_income_by_state <- dataset %>%
  group_by(state) %>%
  summarise(med_income = median(income))

med_income_state <- aggregate(dataset$income, list(dataset$state), median)
names(med_income_state) <- c("State", "Median Income")



#Create tables using ktable

library(formattable)

median_income_by_state %>%
  mutate(med_income = currency(med_income)) %>%
  kbl(caption = "Median Income by State",
    col.names = c("State", "Median Income")) %>%
  kable_styling

kable_paper(kbl(median_income_by_state,
    caption="Median Income by State",
    col.names = c("State", "Median Income")))


kable_styling(kbl(children_count, 
  caption="Count and Percent of Number of Children of Respondents",
  col.names=c("Children", "Count", "Percent")),
  c("striped", "condensed"),
  full_width=F) %>%
  row_spec(3, color="black", background="yellow")

kbl(children_count)

#Create graphs using ggplot2
dataset <- dataset %>%
  mutate(car = as.factor(car))

ggplot(dataset, aes(x=income, y=sqft)) +
  geom_point()

ggplot(dataset, aes(x=pet, fill=has_kids)) +
  geom_bar()
  

ggplot(dataset, aes(x=race)) +
  geom_bar()

ggplot(dataset, aes(x=race)) +
  geom_bar(fill="forestgreen") + 
  labs(x="Race",
       y="Count",
       title="Count of Race")
  
ggplot(dataset, aes(x=has_kids, y=sleephours)) +
  geom_boxplot()

library(wrap_labs)

ggplot(dataset, aes(x=has_kids, y=sleephours)) +
  geom_boxplot(fill="lightblue") +
  labs(x="Has kids",
       y="Hours of sleep per night",
       title="This is a really long title about\nthe hours of nightly sleep",
       subtitle="Grouped by having children") 
  
library(scales)

ggplot(dataset, aes(x=has_kids, y=sqft)) +
  geom_boxplot(fill="pink") +
  labs(x="Has kids",
       y="Square footage of home",
       title="Square footage of home\nby has children") +
  scale_y_continuous(label=comma)


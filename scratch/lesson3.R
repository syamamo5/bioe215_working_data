library(tidyverse)

# Read and inspect
surveys <- read_csv("data/portal_data_joined.csv") # <- is "Alt" + "-"
head(surveys)
summary(surveys)


#Q1: What’s the type of column species_id? Of hindfoot_length?

typeof(surveys$species_id)
# character


#Q2: How many rows and columns are in surveys?

dim(surveys)
# row 34786, column 13

select(surveys, plot_id, species_id, weight)
select(surveys, plot_id, weight_g = weight)
select(surveys, -record_id, -species_id)

filter(surveys, year == 1995) # "==" is for comparison
filter(surveys, year == 1995, plot_id ==7)
filter(surveys, month == 2 | day == 20) # "|" is or


#Q3: filter() surveys to records collected in November where hindfoot_length is greater than 36.0

filter(surveys, month == 11, hindfoot_length > 36.0)

#Q4: Fix these errors

filter(surveys, year = 1995) # need "=="
filter(surveys, polt_id == 2) # spelling!!!!!!!



# < Pipes >

select(filter(surveys, year == 1995), plot_id, weight)

#or

surveys_psw <- surveys %>% # %>% is "then", shift + ctrl + m
  filter(year == 1995) %>%
  select(plot_id, weight)


# Q5: Use pipes to subset surveys to animals collected before 1995 retaining just the columns year, sex, and weight

surveys_Q5 <- surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight)


surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  view()

surveys %>%
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) %>%
  view()


# Q6: Create a new data frame from the surveys data that meets the following criteria: contains only the species_id column and a new column called hindfoot_cm containing the hindfoot_length values (currently in mm) converted to centimeters. In this hindfoot_cm column, there are no NAs and all values are less than 3.

surveys_Q6 <- surveys %>%
  filter(!is.na(weight)) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  select(species_id, hindfoot_cm) %>% 
  view()

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# NaN = not a number

surveys %>% 
  drop_na(weight) %>% # get rid of NA
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight), # summarize drops the last group :(
            min_weight = min(weight),
            .groups = "drop")
  arrange(mean_weight) # arrange(desc(mean_weight))


  
# Q7: How many animals were caught in each plot_type surveyed? (hint: see ?n)

surveys %>%
  drop_na(weight) %>%
  group_by(plot_type) %>% 
  summarize(how_many_animals = n())
  

# Q8: Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). Also add the number of observations (hint: see ?n).

surveys %>%
  drop_na(hindfoot_length) %>%
  group_by(species_id) %>% 
  summarize(mean_length = mean(hindfoot_length),
            min_length = min(hindfoot_length),
            max_length = max(hindfoot_length),
            number_of_observations = n(),
            .groups = "drop")  


# Q9: What was the heaviest animal measured in each year? Return the columns year, genus, species_id, and weight.

surveys %>%
  drop_na(weight) %>%
  group_by(year,genus, species_id) %>% 
  summarize(weight = max(weight),
            .groups = "drop") # how to call species_id?
  
  

# Lost in abyss...






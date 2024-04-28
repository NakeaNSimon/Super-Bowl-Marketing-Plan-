# Super-Bowl-Marketing-Plan-
TV, halftime shows, and the Big Game, we cleaned and explored using data from previous Super Bowls 
#Project: TV, Halftime Shows, and the Big Game

##Load the tidyverse package 
library(set_of_8_packages)

head(halftime_musicians)
#Load Packages 
library(readr)

#1. Load the CSV Data 

##TV
tv <- read_csv("tv.csv")
View(tv)

##Yearly SuperBowls 
library(readr)
super_bowls <- read_csv("super_bowls.csv")
View(super_bowls)

##Halftime Musicians 
library(readr)
halftime_musicians <- read_csv("halftime_musicians.csv")
View(halftime_musicians)

## Display the first six rows of each tibble
head(tv)
head(super_bowls)
head(halftime_musicians)

# 2. Summary of the TV data 

###I noticed from the TV data database there are 38 Nulls/NA's for total us viewers. 

#Summary of the superbowl data 
### You can see that the dataset appears whole except for missing values in the backup quarterback columns (qb_winner_2 and qb_loser_2), which make sense given most starting QBs in the Super Bowl (qb_winner_1 and qb_loser_1) play the entire game.

#Summary of Halftime Musicians data 
#### Looking at the data here there are about 46 songs missing from the dataset. Since the first Super Bowl was played on 1967 many of these songs may have no been recorded. 

# 3. Combined Points Distribution 

##Reduce the size of the plots
options(repr.plot.width = 5, repr.plot.height = 4
        
##Plot a histogram of combined points
ggplot(super_bowls, aes(combined_pts)) +
  geom_histogram(binwidth = 5, color="darkblue", fill="lightblue") +
  labs(x = "Combined Points", y = "Number of Super Bowls")
  
##Display the highest- and lowest-scoring Super Bowls
super_bowls  %>% 
  filter(combined_pts > 70 | combined_pts < 25)

# 4. Point Difference Distribution 

##Reduce the size of the plots
options(repr.plot.width = 5, repr.plot.height = 4)

##Plot a histogram of point differences
ggplot(super_bowls, aes(difference_pts)) +
  geom_histogram(binwidth = 2, fill = "darkblue") +
  labs(x = "Point Difference", y = "Number of Super Bowls")

##Display the closest game and largest blow out
super_bowls  %>% 
  filter(difference_pts == min(40) | difference_pts == max(50))

#5.Do blowouts translate to lost viewers?

## Filter out Super Bowl I and join the game data and TV data
games_tv <- tv  %>% 
  filter(super_bowl != 1)  %>% 
  inner_join(super_bowls, by = "super_bowl")

## Create a scatter plot with a linear regression model
ggplot(games_tv, aes(x = difference_pts, y = share_household), color = "blue") +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Point Difference", y = "Viewership (household share)")

games_tv_plot  <- games_tv %>% 
  gather(key = "category", value = "value", avg_us_viewers, rating_household, ad_cost)  %>% 
  mutate(cat_name = case_when(category == "avg_us_viewers" ~ "Average number of US viewers",
                              category == "rating_household" ~ "Household rating",
                              category == "ad_cost" ~ "Advertisement cost (USD)",
                              TRUE ~ as.character(category)))

## Plot the data
ggplot(games_tv_plot, aes(super_bowl, value)) +
  geom_line() +
  facet_wrap(~ cat_name, scales = "free", nrow = 3) + 
  labs(x = "Super Bowl", y = "") +
  theme_minimal()

# 7. Halftime shows weren't always this great

## Filter and diplay halftime musicians before and including Super Bowl XXVII
( pre_MJ  <- halftime_musicians  %>% 
    filter(super_bowl <= 27) )

# 8. Who has the most halftime show appearances?

## Display the musicians who performed more than once
halftime_musicians  %>% 
  count(musician, sort = TRUE)  %>% 
  filter(n > 1) 

# 9. Who performed the most songs in a halftime show?

## Remove marching bands and data before Super Bowl XX
musicians_songs  <- halftime_musicians  %>% 
  filter(!str_detect(musician, "Marching"),
         !str_detect(musician, "Spirit"),
         super_bowl > 20)

## Plot a histogram of the number of songs per performance
ggplot(musicians_songs, aes(num_songs)) + 
  geom_histogram(binwidth = 1, fill = "pink") +
  labs(x = "Number of songs per halftime show", y = "Number of musicians")

## Display the musicians with more than four songs per show
musicians_songs  %>% 
  filter(num_songs > 4)  %>% 
  arrange(desc(num_songs))

## Display the musicians who performed more than once
halftime_musicians  %>% 
  count(musician, sort = TRUE)  %>% 
  filter(n > 1) 

#Conclusion 
## Who do you think will win Super Bowl LIII?

# 2018-2019 conference champions
patriots <-  "New England Patriots"
rams  <- "Los Angeles Rams"

# Who will win Super Bowl LIII?
super_bowl_LIII_winner  <-  "Elvis"
paste("The winner of Super Bowl LIII will be the", super_bowl_LIII_winner)

#'The winner of Super Bowl LIII will be the Elvis'

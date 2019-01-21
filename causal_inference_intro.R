# load packages

library(dplyr)
library(CausalImpact)
library(babynames)
library(ggplot2)
library(tidyr)
library(xts)


# load ggplot2 theme

source("theme_chris.R")


# look at babynames dataframe

glimpse(babynames)

summary(babynames$year)


# Look at girls called Anya trend

babynames %>%
  filter(sex == "F") %>%
  filter(name == "Anya") %>%
  ggplot(aes(x = year,
             y = n)) +
  geom_line() +
  labs(title = "Number of Girls Born and Called Anya in the USA by Year",
       x = "Year",
       y = "Number") +
  theme_chris()


# select girls' names at random to use as controls

girls_names <- 
  babynames %>%
  filter(sex == "F",
         n > 500) %>%
  dplyr::select(name) %>%
  unique()

set.seed(1)
control_names <- girls_names[sample(nrow(girls_names), 10), ]
control_names <-
  as.vector(control_names$name)


#------ Causal effect for Anya ------

# build dataset with control names

buffy_names <-
  babynames %>%
  filter(sex == "F",
         name == "Anya" | name %in% control_names,
         year > 1979) %>%
  dplyr::select(year, name, n)


# plot names over time

ggplot(buffy_names, aes(x = year,
                        y = n,
                        colour = name)) + 
  geom_line() +
  labs(title = "Girls' Names Registered Each Year in USA Since 1980",
       x = "Year",
       y = "Number of babies") +
  theme_chris()


# turn years into dates beginning 1st Jan of that year and create wide dataframe

buffy_names_2 <-
  buffy_names %>%
  mutate(year = paste0(year, "-01-01")) %>%
  mutate(year = as.Date(year, "%Y-%m-%d")) %>%
  spread(name, n)


# create dataframe for CausalImpact with target variable (Anya) as first column (year to be removed)

buffy_names_3 <-
  buffy_names_2 %>%
  dplyr::select(year, Anya, Aleah, Carmella, Cheryl, Jordan,
                Kathryn, Kiera, Kylee, Raelyn, Stacey)


# convert to xts time series

buffy_names_4 <- 
  buffy_names_3 %>%
  dplyr::select(-year) %>%
  as.xts(order.by = buffy_names_3$year)

head(buffy_names_4)


# create pre and post periods

pre_period <- as.Date(c("1980-01-01", "1998-01-01"))
post_period <- as.Date(c("2004-01-01", "2017-01-01"))


# perform causal inference analysis

buffy_causal <- CausalImpact(buffy_names_4, 
                             pre.period = pre_period, 
                             post.period = post_period)

plot(buffy_causal, "original")
summary(buffy_causal)
summary(buffy_causal, "report")

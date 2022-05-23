# Armaan Azeem
# INFO 201 B
# Professor Deeb-Swihart
# 10 May 2022

# A3: Data Visualisation (Incarceration)

# --------------------------------------------------------------------------

# Loading in libraries

library(stringr)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
library(htmlwidgets)

# Loading the dataset into variable 'incarcerations'

incarcerations <- read.csv("incarceration_trends.csv")

# Number of rows and columns in the dataset: 'rows_incarceration',
# 'columns_incarceration'

rows_incarceration <- nrow(incarcerations)
columns_incarceration <- ncol(incarcerations)

# Removing columns that contain information that do not pertain to 
# the research area of interest

incarcerations <- incarcerations[c(2, 4:7, 10:14, 21, 28:33)]

# ------------------------------ CRI ANALYSIS ------------------------------

# Extract 'incarcerations' information to the state level 
# ('state_level_incarcerations)

state_level_incarcerations <- incarcerations %>%
  group_by(year, state) %>%
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE),
    aapi_pop_15to64 = sum(aapi_pop_15to64, na.rm = TRUE),
    black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE),
    latinx_pop_15to64 = sum(latinx_pop_15to64, na.rm = TRUE),
    native_pop_15to64 = sum(native_pop_15to64, na.rm = TRUE),
    white_pop_15to64 = sum(white_pop_15to64, na.rm = TRUE),
    total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
    aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
    black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
    latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
    native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
    white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
    other_race_jail_pop = sum(other_race_jail_pop, na.rm = TRUE)
  ) 

state_level_incarcerations <- as.data.frame(state_level_incarcerations)

# Adding a column for each ethnicity's proportion to the total 15-64 population

state_level_incarcerations <- state_level_incarcerations %>%
  mutate(
    aapi_pop_prop = aapi_pop_15to64 / total_pop_15to64,
    black_pop_prop = black_pop_15to64 / total_pop_15to64,
    latinx_pop_prop = latinx_pop_15to64 / total_pop_15to64,
    native_pop_prop = native_pop_15to64 / total_pop_15to64,
    white_pop_prop = white_pop_15to64 / total_pop_15to64,
)

# Calculating the standard deviation for each ethnicity proportion set

state_pop_sd <- state_level_incarcerations[17:21]

state_pop_sd <-transform(state_pop_sd, pop_sd=apply(
  state_pop_sd,
  1, 
  sd, 
  na.rm = TRUE
  )
)

state_pop_sd <- state_pop_sd[6]

# Mutating the 'state_level_incarcerations' dataframe to include a column
# for state population standard deviations

state_level_incarcerations <- cbind(state_level_incarcerations, state_pop_sd)

# Finding the average standard deviation over the 48-year period for each state

state_avg_sd <- state_level_incarcerations %>%
  group_by(state) %>%
  summarise(avg_sd = mean(pop_sd, na.rm = TRUE))

state_avg_sd <- as.data.frame(state_avg_sd)

# Obtaining the state with the smallest standard deviation ('state_smallest_sd')

state_smallest_sd <- state_avg_sd[state_avg_sd$avg_sd == min(state_avg_sd$avg_sd), 1]
smallest_sd <- min(state_avg_sd$avg_sd)

# Obtaining the state with the largest standard deviation ('state_largest_sd')

state_largest_sd <- state_avg_sd[state_avg_sd$avg_sd == max(state_avg_sd$avg_sd), 1]
largest_sd <- max(state_avg_sd$avg_sd)

# Adding a column for each ethnicity's proportion to the total 15-64 
# jail population to 'state_level_incarcerations'

state_level_incarcerations <- state_level_incarcerations %>%
  mutate(
    aapi_jail_prop = aapi_jail_pop / total_jail_pop,
    black_jail_prop = black_jail_pop / total_jail_pop,
    latinx_jail_prop = latinx_jail_pop / total_jail_pop,
    native_jail_prop = native_jail_pop / total_jail_pop,
    white_jail_prop = white_jail_pop / total_jail_pop,
    other_race_jail_prop = other_race_jail_pop / total_jail_pop,
  )

# Calculating the standard deviation for each ethnicity jail proportion set

jail <- state_level_incarcerations[23:28]

jail <-transform(jail, jail_pop_sd=apply(
  jail,
  1, 
  sd, 
  na.rm = TRUE
  )
)

jail <- jail[7]

# Mutating the 'state_level_incarcerations' dataframe to include a column
# for jail population standard deviations

state_level_incarcerations <- cbind(state_level_incarcerations, jail)

# Obtaining the standard deviation for California (lowest pop. SD)
# 'ca_jail_sd'

jail_avg_sd <- state_level_incarcerations %>%
  group_by(state) %>%
  summarise(jail_sd = mean(jail_pop_sd, na.rm = TRUE))

jail_avg_sd <- as.data.frame(jail_avg_sd)

ca_sd <- jail_avg_sd[state_avg_sd$state == "CA", 2]

# Obtaining the standard deviation for Maine (highest pop. SD)
# 'me_jail_sd'

me_sd <- jail_avg_sd[state_avg_sd$state == "ME", 2]

# CHI SQUARE TEST - Finding the state with the most uniformly ethnicity 
# distribution (15-64 population)

# Execute a chi-square test among each row of ethnicity proportion
# distributions

state_pop <- state_level_incarcerations[c(1, 2, 17:21)]

state_pop <- state_level_incarcerations %>%
  group_by(state) %>%
  summarise(
    aapi_pop_prop = sum(aapi_pop_prop, na.rm = TRUE),
    black_pop_prop = sum(black_pop_prop, na.rm = TRUE),
    latinx_pop_prop = sum(latinx_pop_prop, na.rm = TRUE),
    native_pop_prop = sum(native_pop_prop, na.rm = TRUE),
    white_pop_prop = sum(white_pop_prop, na.rm = TRUE)
  )

state_pop <- as.data.frame(state_pop)

pop_chisq <- apply(X = state_pop[,2:6], MARGIN = 1, FUN = chisq.test)
pop_chisq <- unlist(lapply(pop_chisq, "[", "statistic"), use.names = FALSE)
pop_chisq <- as.data.frame(pop_chisq)

# Add a column of the chi-square test results to 'state_pop'

state_pop <- cbind(state_pop, pop_chisq)

# Find the state with the lowest test result ('state_highest_uniformity')

state_highest_uniformity <- state_pop[state_pop$pop_chisq == min(state_pop$pop_chisq), 1]
lowest_chisq <- min(state_pop$pop_chisq)
  
  # Find the state with the highest test result ('state_lowest_uniformity')
  
state_lowest_uniformity <- state_pop[state_pop$pop_chisq == max(state_pop$pop_chisq), 1]
highest_chisq <- max(state_pop$pop_chisq)

# CHI SQUARE TEST - Finding the chi-square value of the incarcerated ethnicity
# distributions of California and Maine

jail_pop <- state_level_incarcerations[c(1,2, 23:28)]

jail_pop <- state_level_incarcerations %>%
  group_by(state) %>%
  summarise(
    aapi_jail_prop = sum(aapi_jail_prop, na.rm = TRUE),
    black_jail_prop = sum(black_jail_prop, na.rm = TRUE),
    latinx_jail_prop = sum(latinx_jail_prop, na.rm = TRUE),
    native_jail_prop = sum(native_jail_prop, na.rm = TRUE),
    white_jail_prop = sum(white_jail_prop, na.rm = TRUE),
    other_race_jail_prop = sum(other_race_jail_prop, na.rm = TRUE),
    
  )

jail_pop <- as.data.frame(jail_pop)

# There is no ethnicity proportion data available for the states of CT, DE
# HI, RI, or VT. Since these states are not in the research scope of interest
# (and will prevent the chi-squared test function from running), these five
# states will be removed from the dataframe.

jail_pop <- jail_pop %>%
  filter(!aapi_jail_prop == 0)

jail_chisq <- apply(X = jail_pop[,2:7], MARGIN = 1, FUN = chisq.test)
jail_chisq <- unlist(lapply(jail_chisq, "[", "statistic"), use.names = FALSE)
jail_chisq <- as.data.frame(jail_chisq)

# Add a column of the chi-square test results to 'jail_pop'

jail_pop <- cbind(jail_pop, jail_chisq)

# Find the California's test result ('ca_jail_uniformity')

ca_jail_uniformity <- jail_pop[jail_pop$state == "CA", 8]
  
# Find the Maine's test result ('me_jail_uniformity')
  
me_jail_uniformity <- jail_pop[jail_pop$state == "ME", 8]

# ---------------------------- ALASKA ANALYSIS -----------------------------

# Subset the 'state_level_incarcerations' dataframe to only include Alaska into
# variable 'alaska_incarcerations'

alaska_incarcerations <- state_level_incarcerations %>%
  filter(state == "AK") 

# Retrieving the standard deviation of Alaska's ethnicity distribution 

alaska_sd <- state_avg_sd[state_avg_sd$state == "AK", 2]

# Retrieving the standard deviation of Alaska's jail ethnicity distribution 

alaska_jail_sd <- jail_avg_sd[jail_avg_sd$state == "AK", 2]

# Retrieving Alaska's 15-64 population chi-sq. result

alaska_pop_uniformity <- state_pop[state_pop$state == "AK", 7] 

# Retrieving Alaska's incarcerated population chi-sq. result

alaska_jail_uniformity <- jail_pop[jail_pop$state == "AK", 8] 

# -------------------------------- GRAPHS ----------------------------------

# Line graph for California's 15-64 population over time ('ca_pop_graph')

ca_pop_data <- state_level_incarcerations %>%
  filter(state == "CA") %>%
  select(-c(2, 3, 5:29))

ca15to64 <- ggplot(ca_pop_data, aes(
    x = year, 
    y = total_pop_15to64)) +
  geom_line(colour='#72866f') +
  geom_point(colour='#72866b') + 
  ggtitle("CA 15-64 Population over Time") +
  xlab("Year") + 
  ylab("Population (Ages 15-64)") 

# Line graph for Maine's 15-64 population over time ('ca_pop_graph')

me_pop_data <- state_level_incarcerations %>%
  filter(state == "ME") %>%
  select(-c(2, 3, 5:29))

me15to64 <- ggplot(me_pop_data, aes(
    x = year, 
    y = total_pop_15to64)) +
  geom_line(colour='#72866f') +
  geom_point(colour='#72866f') + 
  ggtitle("ME 15-64 Population over Time") +
  xlab("Year") + 
  ylab("Population (Ages 15-64)")

# Line graph for Alaska's 15-64 population over time ('ak_pop_graph')

ak_pop_data <- state_level_incarcerations %>%
  filter(state == "AK") %>%
  select(-c(2, 3, 5:29))

ak15to64 <- ggplot(ak_pop_data, aes(
    x = year, 
    y = total_pop_15to64)) +
  geom_line(colour='#72866f') +
  geom_point(colour='#72866f') + 
  ggtitle("AK 15-64 Population over Time") +
  xlab("Year") + 
  ylab("Population (Ages 15-64)")

# Bar chart for California's 15-64 population (averaged over the span of the
# entire 48 years, via ethnicity group)

state_avg_pop_prop <- state_level_incarcerations %>%
  group_by(state) %>%
  summarise(
    aapi = mean(aapi_pop_prop, na.rm = TRUE),
    black = mean(black_pop_prop, na.rm = TRUE),
    latinx = mean(latinx_pop_prop, na.rm = TRUE),
    native = mean(native_pop_prop, na.rm = TRUE),
    white = mean(white_pop_prop, na.rm = TRUE)
    )

state_avg_pop_prop <- as.data.frame(state_avg_pop_prop)

ca_pop_prop <- filter(
  state_avg_pop_prop,
  state == "CA"
)

ca_ethnicity_prop <- as.numeric(ca_pop_prop[2:6])

ca_ethnicities <- data.frame(
  ethnicity = colnames(ca_pop_prop[,2:6]),
  pop_prop = ca_ethnicity_prop
)

capopprop <- ggplot(ca_ethnicities, aes(
    x = ethnicity, 
    y = pop_prop, 
    fill=ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("CA Average 15-64 Population Proportions By Ethnicity") +
  scale_fill_manual("ethnicity", values = c("aapi" = "#D6CAB2", 
                                            "black" = "#A0957C",  
                                            "latinx" = "#D6BB69", 
                                            "native" = "#4E6545", 
                                            "white" = "#CEE9C3"))  

# Bar chart for California's jail population (averaged over the span of the
# entire 48 years, via ethnicity group)

state_avg_jail_prop <- state_level_incarcerations %>%
  group_by(state) %>%
  summarise(
    aapi = mean(aapi_jail_prop, na.rm = TRUE),
    black = mean(black_jail_prop, na.rm = TRUE),
    latinx = mean(latinx_jail_prop, na.rm = TRUE),
    native = mean(native_jail_prop, na.rm = TRUE),
    white = mean(white_jail_prop, na.rm = TRUE),
    other = mean(other_race_jail_prop, na.rm = TRUE)
    )

state_avg_jail_prop <- as.data.frame(state_avg_jail_prop)

ca_jail_prop <- filter(
  state_avg_jail_prop,
  state == "CA"
)

ca_jail_ethnicity_prop <- as.numeric(ca_jail_prop[2:7])

ca_jail_ethnicities <- data.frame(
  ethnicity = colnames(ca_jail_prop[,2:7]),
  jail_prop = ca_jail_ethnicity_prop
)

cajailprop <- ggplot(ca_jail_ethnicities, aes(
    x = ethnicity, 
    y = jail_prop, 
    fill=ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("CA Average Jail Proportions By Ethnicity") +
  scale_fill_manual("ethnicity", values = c("aapi" = "#D6CAB2", 
                                            "black" = "#A0957C",  
                                            "latinx" = "#D6BB69", 
                                            "native" = "#4E6545", 
                                            "white" = "#CEE9C3",
                                            "other" = "#BAC8B4")) 

# Bar chart for Maine's 15-64 population (averaged over the span of the
# entire 48 years, via ethnicity group)

me_pop_prop <- filter(
  state_avg_pop_prop,
  state == "ME"
)

me_ethnicity_prop <- as.numeric(me_pop_prop[2:6])

me_ethnicities <- data.frame(
  ethnicity = colnames(me_pop_prop[,2:6]),
  pop_prop = me_ethnicity_prop
)

mepopprop <- ggplot(me_ethnicities, aes(
    x = ethnicity, 
    y = pop_prop, 
    fill=ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("ME Average 15-64 Population Proportions By Ethnicity") +
  scale_fill_manual("ethnicity", values = c("aapi" = "#D6CAB2", 
                                            "black" = "#A0957C",  
                                            "latinx" = "#D6BB69", 
                                            "native" = "#4E6545", 
                                            "white" = "#CEE9C3"))  

# Bar chart for Maine's jail population (averaged over the span of the
# entire 48 years, via ethnicity group)

me_jail_prop <- filter(
  state_avg_jail_prop,
  state == "ME"
)

me_jail_ethnicity_prop <- as.numeric(me_jail_prop[2:7])

me_jail_ethnicities <- data.frame(
  ethnicity = colnames(me_jail_prop[,2:7]),
  jail_prop = me_jail_ethnicity_prop
)

mejailprop <- ggplot(me_jail_ethnicities, aes(
    x = ethnicity, 
    y = jail_prop, 
    fill=ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("ME Average Jail Proportions By Ethnicity") +
  scale_fill_manual("ethnicity", values = c("aapi" = "#D6CAB2", 
                                            "black" = "#A0957C",  
                                            "latinx" = "#D6BB69", 
                                            "native" = "#4E6545", 
                                            "white" = "#CEE9C3",
                                            "other" = "#BAC8B4"))  

# Bar chart for Alaska's 15-64 population (averaged over the span of the
# entire 48 years, via ethnicity group)

ak_pop_prop <- filter(
  state_avg_pop_prop,
  state == "AK"
)

ak_ethnicity_prop <- as.numeric(ak_pop_prop[2:6])

ak_ethnicities <- data.frame(
  ethnicity = colnames(ak_pop_prop[,2:6]),
  pop_prop = ak_ethnicity_prop
)

akpopprop <- ggplot(ak_ethnicities, aes(
    x = ethnicity, 
    y = pop_prop, 
    fill=ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("AK Average 15-64 Population Proportions By Ethnicity") +
  scale_fill_manual("ethnicity", values = c("aapi" = "#D6CAB2", 
                                            "black" = "#A0957C",  
                                            "latinx" = "#D6BB69", 
                                            "native" = "#4E6545", 
                                            "white" = "#CEE9C3"))  

# Bar chart for Alaska's jail population (averaged over the span of the
# entire 48 years, via ethnicity group)

ak_jail_prop <- filter(
  state_avg_jail_prop,
  state == "AK"
)

ak_jail_ethnicity_prop <- as.numeric(ak_jail_prop[2:7])

ak_jail_ethnicities <- data.frame(
  ethnicity = colnames(ak_jail_prop[,2:7]),
  jail_prop = ak_jail_ethnicity_prop
)

akjailprop <- ggplot(ak_jail_ethnicities, aes(
    x = ethnicity, 
    y = jail_prop, 
    fill=ethnicity)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("AK Average Jail Proportions By Ethnicity") +
  scale_fill_manual("ethnicity", values = c("aapi" = "#D6CAB2", 
                                            "black" = "#A0957C",  
                                            "latinx" = "#D6BB69", 
                                            "native" = "#4E6545", 
                                            "white" = "#CEE9C3",
                                            "other" = "#BAC8B4")) 

# Converting the above graphs to ggplotly, so they can be exported as 
# interactive graphs via .html files for embedding 

ggplotly(ca15to64)
ggplotly(me15to64)
ggplotly(ak15to64)
ggplotly(capopprop)
ggplotly(mepopprop)
ggplotly(akpopprop)
ggplotly(cajailprop)
ggplotly(mejailprop)
ggplotly(akjailprop)

# GEOGRAPHICAL MAP: jail population chi-sq. results among the states

# Loading map data

states <- map_data("state")

# Refining 'states' to include state abbreviations instead of the full name

# states <- states %>%
#   group_by(region) %>%
#   slice(1)
# 
# states <- as.data.frame(states)

states[states == "alabama"] <- "AL"
states[states == "arizona"] <- "AZ"
states[states == "arkansas"] <- "AR"
states[states == "california"] <- "CA"
states[states == "colorado"] <- "CO"
states[states == "connecticut"] <- "CT"
states[states == "district of columbia"] <- "DC"
states[states == "florida"] <- "FL"
states[states == "georgia"] <- "GA"
states[states == "idaho"] <- "ID"
states[states == "illinois"] <- "IL"
states[states == "delaware"] <- "DE"
states[states == "indiana"] <- "IN"
states[states == "iowa"] <- "IA"
states[states == "kansas"] <- "KS"
states[states == "kentucky"] <- "KY"
states[states == "louisiana"] <- "LA"
states[states == "maine"] <- "ME"
states[states == "maryland"] <- "MD"
states[states == "massachusetts"] <- "MA"
states[states == "michigan"] <- "MI"
states[states == "minnesota"] <- "MN"
states[states == "mississippi"] <- "MS"
states[states == "missouri"] <- "MO"
states[states == "montana"] <- "MT"
states[states == "nebraska"] <- "NE"
states[states == "nevada"] <- "NV"
states[states == "new hampshire"] <- "NH"
states[states == "new jersey"] <- "NJ"
states[states == "new mexico"] <- "NM"
states[states == "new york"] <- "NY"
states[states == "north carolina"] <- "NC"
states[states == "north dakota"] <- "ND"
states[states == "ohio"] <- "OH"
states[states == "oklahoma"] <- "OK"
states[states == "oregon"] <- "OR"
states[states == "pennsylvania"] <- "PA"
states[states == "rhode island"] <- "RI"
states[states == "south carolina"] <- "SC"
states[states == "south dakota"] <- "SD"
states[states == "tennessee"] <- "TN"
states[states == "texas"] <- "TX"
states[states == "utah"] <- "UT"
states[states == "vermont"] <- "VT"
states[states == "virginia"] <- "VA"
states[states == "washington"] <- "WA"
states[states == "west virginia"] <- "WV"
states[states == "wisconsin"] <- "WI"
states[states == "wyoming"] <- "WY"

# Removing CT, DE, HI, RI, and VT

states <- states %>%
  filter(region != "CT", region != "DE", region != "HI", region != "RI",
         region != "VT")

# Bias: Alaska is not included within the map's configuration. Thus, Alaska
# will have to be removed from 'jail_pop' and this bias will be noted.

jail_pop <- jail_pop %>%
  filter(state != "AK") %>%
  select(state, jail_chisq)

names(jail_pop)[names(jail_pop) == 'state'] <- 'region'

# Adding jail chi-sq. column to 'states'

states <- inner_join(states, jail_pop, by = "region")


statechisq <- ggplot() + 
  geom_polygon( data=states, 
                aes(x=long, y=lat, group=group, fill = jail_chisq), 
                color="white", size = 0.2) +
  scale_fill_continuous(name="Jail Chi-Sq. Value", 
                        high = "#CEE9C3", low = "#4E6545") +
  ggtitle("Incarcerated Ethnicity Group Chi-Square Values Across the US")

# Converting to an interactive graph

ggplotly(statechisq)
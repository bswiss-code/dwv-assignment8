#' ---
#' title: "Assignment 8"
#' author: "Bryn Swiston"
#' format:
#'   pdf: default
#' execute:
#'   echo: false
#'   warning: false
#'   message: false
#' ---
#' 
## ------------------------------------------------------------------------------------------------------------------------
#I will now open and activate the necessary libraries
library(haven)
library(tidyverse)
library(readr)

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I will now read in two data sets. I will be using the Policy Moods dataset by Stimson, James A., and K. Elizabeth Coggins, and the TV News Policy Agenda Data by Joe Uscinski. 
POLICYMOODS <-read_csv('/Users/brynswiston/Documents/Data Wrangling Library/DWV Assignment 8 Files/publicopinion-moods-dataset.csv')
TVNEWS <- read_csv('/Users/brynswiston/Documents/Data Wrangling Library/DWV Assignment 8 Files/media-tvnews-dataset.csv')

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I will now map out my plan for this assignment. I will aim to have my graphs illustrate and answer the following three questions: 
#1. Which topics get less TV coverage in presidential election years? and 
#2. Does TV coverage of crime align with a shift in public opinion? and
#3. Do TV news and public policy pollsters pay attention to the same topics? 

#I will do this through the following series of steps for each question: 

#1. Go through the code book, identify and filter the variables of interests, and create the relevant data sets
#2. Rename the variables as necessary
#3. Summarize the data sets and create proportions or shares
#4. Create the graphs and adjust

#' 
## ------------------------------------------------------------------------------------------------------------------------
#Now I will filter and select the varibles of interest for the Policy Moods dataset needed to answer my questions 1 and 3. 
POLICYMOODS <- POLICYMOODS |>
  select(year, Topic, percent)|>
  filter(!is.na(percent))

#' 
## ------------------------------------------------------------------------------------------------------------------------
#Now I will do the same with TV News dataset. 
TVNEWS <- TVNEWS |>
  select(Year, Channel, Topic) |>
  filter(!is.na(Topic))

#' 
## ------------------------------------------------------------------------------------------------------------------------
#Now I want to relabel things in TVNEWS so I'm not constantly looking everything up. I don't know if this is necessary exactly (how/if the variables are encoded in the dataset, that is still a little confusing to me), but it just helps me visualize the data better. To do this, I had to learn what a "tribble" was. 
tv_labels <- tribble(
  ~Topic, ~topic_name,
  1,  "Macroeconomics",
  2,  "Civil rights",
  3,  "Health",
  4,  "Agriculture",
  5,  "Labor & employment",
  6,  "Education",
  7,  "Environment",
  8,  "Energy",
  10, "Transportation",
  12, "Law & crime",
  13, "Social welfare",
  14, "Community development & housing",
  15, "Banking, finance & commerce",
  16, "Defense",
  17, "Space, science & technology",
  18, "Foreign trade",
  19, "International affairs",
  20, "Federal government operations",
  21, "Public lands & water",
  24, "State & local government",
  26, "Weather & natural disasters",
  27, "Fires",
  28, "Culture & entertainment",
  30, "Death notices",
  31, "Campaigns",
  32, "Family issues",
  33, "Immigration",
  34, "Religion",
  99, "Other"
)

#Now I will join it with the TVNEWS dataframe so I can see everything in one place.
TVNEWS <- TVNEWS |>
  left_join(tv_labels, by = "Topic")

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I would like to do the same thing with POLICYMOODS. 
mood_labels <- tribble(
  ~majortopic, ~major_name,
  1,  "Macroeconomics",
  2,  "Civil rights",
  3,  "Health",
  4,  "Agriculture",
  5,  "Labor",
  6,  "Education",
  7,  "Environment",
  8,  "Energy",
  9,  "Immigration",
  10, "Transportation",
  12, "Law & crime",
  13, "Social welfare",
  14, "Community development",
  15, "Banking & commerce",
  16, "Defense",
  17, "Space, science & technology",
  18, "Foreign trade",
  19, "International affairs",
  20, "Government operations",
  21, "Public lands"
)
mood <- POLICYMOODS |>
  select(Year = year, Topic, Mood = percent) |>
  mutate(majortopic = Topic %/% 100) |>
  left_join(mood_labels, by = "majortopic")

#' 
## ------------------------------------------------------------------------------------------------------------------------
#Now I will focus on Question 1:Which topics get less TV coverage in presidential election years? To answer this question, I only want to look at election years. 
election_years <- seq(1968, 2020, 4)

#I will need to create a new data frame that just looks at election years and non-election years, groups by the topic, and calculates the difference between election years and non-election years. 
crowd <- TVNEWS |>
  filter(!is.na(Topic)) |>
  mutate(period = if_else(Year %in% election_years,
                          "Election year", "Other year")) |>
  count(Year, period, Topic) |>
  group_by(Year) |>
  mutate(share = 100 * n / sum(n)) |>
  ungroup() |>
  complete(nesting(Year, period), Topic, fill = list(n = 0, share = 0)) |>
  group_by(period, Topic) |>
  summarise(avg = mean(share), .groups = "drop") |>
  left_join(tv_labels, by = "Topic")

#I want to make sure that this is stored in wide format though, so I can make it a graph more easily. 
crowd_wide <- crowd |>
  filter(!is.na(topic_name), Topic != 31) |>
  pivot_wider(names_from = period, values_from = avg) |>
  mutate(diff = `Election year` - `Other year`,
         topic_name = fct_reorder(topic_name, diff))

#' 
#' ## Figure 1: Which topics get less TV coverage in presidential election years?
#' 
## ------------------------------------------------------------------------------------------------------------------------
#| fig-pos: "H"
#There are too many topics to fit well on a graph, so I will keep it to the top 12 for a more easily readable graph. I will also make sure the order they will appear in on the graph makes sense and order it by difference. 
plot_data <- crowd_wide |>
  slice_max(abs(diff), n = 12) |>
  mutate(topic_name = fct_reorder(topic_name, diff))

#Now it is time to plot. I want to do a vertical bar graph for this one, I think it fits best with the data I have. 
presyeartv <- ggplot(plot_data, aes(diff, topic_name, fill = diff > 0)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.6) +
  scale_fill_manual(values = c("TRUE" = "limegreen", "FALSE" = "firebrick")) +
  scale_y_discrete(labels = scales::label_wrap(22)) +
  labs(title = "Topics Gaining or Losing TV Coverage in Election Years,1968-2020",
       x = "Change in % of Top Stories",
       y = NULL,
       caption = "Election year minus non-election year. 12 largest changes shown.") +
  theme_minimal(base_size = 10) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 9),
        plot.margin = margin(10, 15, 10, 10))

presyeartv

#Looks good, but I want to save it as a png because I know we have show and tell in class. 
ggsave("election_year_coverage.png", presyeartv, width = 7, height = 5, dpi = 300)

#' 
## ------------------------------------------------------------------------------------------------------------------------
#Now I can move on to Question #2: Does TV coverage of crime align with a shift in public opinion? And I realized I had more than I needed, so I'm going to filter it down even more. 
mood <- mood |>
  select(Year, Topic, Mood)

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I'm specifically just looking at crime with this graph, no other topics. 
tv_crime <- TVNEWS |>
  filter(!is.na(Topic), Year > 1968) |>
  group_by(Year) |>
  summarise(tv = 100 * mean(Topic == 12))

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I'm going to need how people feel about crime, and so I picked topic 1210, which focuses mainly on public sentiments about crime by asking about attitudes toward the death penalty and toward punishment and the courts.
mood_crime <- mood |>
  filter(Topic == 1210) |>
  select(Year, mood = Mood)

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I only want the years that line up between the do graphs, because I'm going to do a stacked line graph.
crime <- inner_join(tv_crime, mood_crime, by = "Year")

#' 
#' ## Figure 2: Does TV coverage of crime align with a shift in public opinion?
#' 
## ------------------------------------------------------------------------------------------------------------------------
#| fig-pos: "H"
#Now it is time to plot. I'm going to try to do a facet wrap here. To do that, I will need to stack the two columns into one so each year gets two rows. 
crimeop <- crime |>
  pivot_longer(c(tv, mood), names_to = "series", values_to = "value") |>
  mutate(series = recode(series,
                         tv   = "TV Crime Coverage (% of Top Stories)",
                         mood = "Public Opinion (0-1, Higher = More Liberal)")) |>
  ggplot(aes(Year, value, color = series)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("TV Crime Coverage (% of Top Stories)" = "chocolate",
                                "Public Opinion (0-1, Higher = More Liberal)" = "cornflowerblue"),
                     guide = "none") +
  labs(title = "TV Crime Coverage and Public Opinion on Crime, 1969-2010",
       x = "Year", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")

crimeop

#Yay it looks good! I had to mess around with spacing and color a bit, but it looks good. 
ggsave("crime_simple.png", crimeop, width = 7, height = 5, dpi = 300)

#' 
## ------------------------------------------------------------------------------------------------------------------------
#Now I can move on to Question #3: Do TV news and public policy pollsters pay attention to the same topics? I need to build a table with one row per TV topic. Also, I need to account for the fact that immigration is the one that needs translating between the two datasets.
tvpoltable <- tv_labels |>
  mutate(majortopic = if_else(Topic == 33, 9, Topic))

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I want to count the most common TV Topics here. 
tv_topic <- TVNEWS |>
  filter(!is.na(Topic), Year > 1968) |>
  count(Topic, name = "tv_n")

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I also want to count the most common polling topics: aka Moods series per topic.
poll_topic <- mood |>
  filter(Topic != 5002) |>        
  mutate(majortopic = Topic %/% 100) |>
  distinct(majortopic, Topic) |>
  count(majortopic, name = "poll_n")

#' 
## ------------------------------------------------------------------------------------------------------------------------
#I need to calculate the shared for each so I can find the difference, because I want to make a dot graph with lines showing the difference for each topic. 
dots <- tvpoltable |>
  inner_join(tv_topic,   by = "Topic") |>
  inner_join(poll_topic, by = "majortopic") |>
  mutate(`TV news`          = 100 * tv_n   / sum(tv_n),
         `Opinion polling`  = 100 * poll_n / sum(poll_n),
         topic_name = fct_reorder(topic_name, `TV news`)) |>
  pivot_longer(c(`TV news`, `Opinion polling`),
               names_to = "source", values_to = "share")

#' 
#' ## Figure 3: Do TV news and public policy pollsters pay attention to the same topics?
#' 
## ------------------------------------------------------------------------------------------------------------------------
#| fig-pos: "H"
#| fig-height: 7
#| fig-width: 7.5
#Now I can plot!
topiccare <- ggplot(dots, aes(share, topic_name, color = source)) +
  geom_line(aes(group = topic_name), color = "navy") +
  geom_point(size = 3) +
  scale_color_manual(values = c("TV news" = "hotpink3",
                                "Opinion polling" = "orange")) +
  scale_y_discrete(labels = scales::label_wrap(22)) +
  labs(title = "Do TV News and Pollsters Cover the Same Topics?",
       x = "Share of Attention (%)", y = NULL, color = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title.position = "plot",
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

topiccare
ggsave("tv_vs_polling.png", topiccare, width = 7, height = 5, dpi = 300)
#It's important to note here that many topics (Education, Environment, Labor, Social welfare, Space and science, Public lands) have their polling dot at the same spot, around 4.7%. But this graph still provides valuable information about the gaps. 

#' 
#' 
#' 

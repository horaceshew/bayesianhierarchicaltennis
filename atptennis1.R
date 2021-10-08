# ---------------------------------------------------------------------------- #










# ----------------------------- Preliminaries -------------------------------- #
#load packages
library(tidyverse)
library(brms)
library(ggthemes)
library(tidybayes)
library(bayesplot)
library(plyr)
library(readr)
library(patchwork)
library(knitr)
library(kableExtra)
library(RColorBrewer)

#read in data
setwd("~/Desktop/2021 Summer Research/tennis")

dir = "csvfiles"
files <- list.files(path=dir, pattern="*.csv", full.names=TRUE)
files

data_tennis <- ldply(files, read_csv) %>%
  as.tibble()

# -------------------------- Pre-process Dataset ----------------------------- #

#remove Davis Cup matches
data_tennis <- data_tennis %>% 
  filter(str_detect(tourney_name, "Davis Cup", negate = TRUE))

#remove Olympic matches
data_tennis <- data_tennis %>% 
  filter(str_detect(tourney_name, "Olympics", negate = TRUE))

#determine observations in "round" column
data_tennis %>% 
  distinct(round)

# ---------------------------------- EDA ------------------------------------- #

#average winner age
data_tennis %>% 
  filter(round == "F") %>% 
  summarise(mean(winner_age))

#average winner rank
data_tennis %>% 
  filter(round == "F") %>%
  summarise(mean(winner_rank))

#youngest winner
data_tennis %>% 
  filter(round == "F") %>% 
  arrange(winner_age) %>% 
  select(winner_name, winner_age, winner_ioc, tourney_name, tourney_date)

#oldest winner
data_tennis %>% 
  filter(round == "F") %>%
  arrange(desc(winner_age)) %>% 
  select(winner_name, winner_age, winner_ioc, tourney_name, tourney_date)

#lowest ranked winner 
data_tennis %>% 
  filter(round == "F") %>%
  arrange(desc(winner_rank)) %>% 
  select(winner_name, winner_rank, winner_ioc, tourney_name, tourney_date)

#15 players with the most titles in the dataset
data_tennis %>% 
  filter(round == "F") %>%
  group_by(winner_name) %>% 
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 15)

#top 10 winners on grass court
gcourt <- data_tennis %>% 
  filter(round == "F" & surface == "Grass") %>%
  group_by(winner_name) %>% 
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(winner_name, n), y = n)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, size = 10), 
        plot.title = element_text(size = 9)) +
  labs(title = "Players with the Most Grass Court Tournaments (2009-2020)", 
       x = "", y = "Number of Wins")

#top 10 winners on hard court
hcourt <- data_tennis %>% 
  filter(round == "F" & surface == "Hard") %>%
  group_by(winner_name) %>% 
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(winner_name, n), width = 0.7, y = n)) + 
  geom_bar(stat = "identity", fill = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, size = 10), 
        plot.title = element_text(size = 9)) +
  labs(title = "Players with the Most Hard Court Tournaments (2009-2020)", 
       x = "", y = "Number of Wins")

#top 10 winners on clay court
ccourt <- data_tennis %>% 
  filter(round == "F" & surface == "Clay") %>%
  group_by(winner_name) %>% 
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(winner_name, n), width = 0.7, y = n)) + 
  geom_bar(stat = "identity", fill = "coral1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, size = 10), 
        plot.title = element_text(size = 9)) +
  labs(title = "Players with the Most Clay Court Tournaments (2009-2020)", 
       x = "", y = "Number of Wins")

#top 10 winners in masters tournaments
data_tennis %>% 
  filter(round == "F" & tourney_level == "M") %>%
  group_by(winner_name) %>% 
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(winner_name, n), width = 0.7, y = n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, size = 6), 
        plot.title = element_text(size = 9)) +
  labs(title = "Players with the Most ATP Masters Tournaments (2009-2020)", 
      x = "", y = "Number of Wins")

#use patchwork to combine plots and use ggsave
combined1 = (gcourt + hcourt + ccourt)
ggsave("surfacetournaments.png", width = 12, height = 4, units = "in", dpi = 400)

# ----------------------------- Mutate Dataset ------------------------------- #

#get number of wins for each winner
winner_data <- data_tennis %>%
  select(winner_name, tourney_id, tourney_name, tourney_level, 
         tourney_date, surface, draw_size, winner_ioc, winner_ht, 
         winner_age, winner_rank, round) %>%
  group_by(winner_name, tourney_id) %>%
  dplyr::mutate(match_wins = n()) %>%
  distinct(tourney_id, .keep_all = TRUE)
colnames(winner_data) <- gsub("winner_", "", colnames(winner_data))

#get match losers
loser_data <- data_tennis  %>%
  select(loser_name, tourney_id, tourney_name, tourney_level, 
         tourney_date, surface, draw_size, loser_name, loser_ht, 
         loser_age, loser_rank, round) %>%
  group_by(loser_name, tourney_id) %>%
  dplyr::mutate(match_wins = 0) %>% 
  distinct(tourney_id, .keep_all = TRUE)
colnames(loser_data) <- gsub("loser_", "", colnames(loser_data))
  
# combine winners and losers
model_data <- winner_data %>%
  full_join(loser_data, by = c("name", "tourney_id", "tourney_name", 
                               "tourney_level", "tourney_date", "surface", 
                               "draw_size", "ht", "age", "rank", "round")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(match_wins = match_wins.x + match_wins.y) %>%
  select(-c(match_wins.x, match_wins.y))

#create binary outcome variable based on round
model_data <- model_data %>% 
  mutate(biround = ifelse(round == "R16" |round == "RR" | round == "BR" | 
                            round == "QF" | round == "SF" | round == "F",1, 0))

#create numerical outcome variable for adjacent category logit based on round
model_data <- model_data %>%
  mutate(acatround = recode(round, "R128"= 1, "R64"= 2, "R32"= 3, "R16"= 4,
                            "QF"= 5, "RR"= 5, "BR"= 5, "SF"= 6, "F"= 7))

#distinct players, tournaments in data
unique(model_data$name)
unique(model_data$tourney_id)
  
# check that match numbers are reasonable
# should be no percentages >1 or <0
# all losers should have total matches = match wins + 1
# number of winners should equal number of tournaments = 761 (or is this not true bc of withdraws?)

model_data %>%
  transmute(
    p_win = match_wins/n_matches,
    total_minus_wins = n_matches - match_wins
  ) %>%
  summarize(
    non_probs = sum(p_win < 0 | p_win > 1),
    losers = sum(total_minus_wins == 1),
    winners = sum(total_minus_wins == 0)
  )

# ------------------------------- Fit Model ---------------------------------- #

#model using binomial outcome variable (round) with player effects
bimodelplayer <- lme4::glmer(biround ~ ht  + age + rank + (1|name), 
            data = model_data,
            family = binomial,
            nAGQ = 0)

#model using binomial outcome variable (round) with tournament effects
bimodeltournament <- lme4::glmer(biround ~ -1 + surface + 
                                   draw_size + tourney_date + (1|tourney_name), 
                             data = model_data,
                             family = binomial,
                             nAGQ = 0)


#model using binary outcome variable, 25 min to run
formula_atp_tennis <- 
  bf(biround ~ ht + age + rank + (1 | name))
prior_atp_tennis <- prior("normal(0,5)", class = "sd", group = "name")
t.start <- Sys.time()
fit_atp_tennis_b <- brm(formula = formula_atp_tennis,
                        data = model_data,
                        family = brmsfamily("bernoulli", "logit"),
                        prior = prior_atp_tennis,
                        warmup = 1000, iter = 2000,
                        cores = 4, chains = 4,
                        seed = 12345
)
Sys.time() - t.start

fit_atp_b = readRDS("atp_1.rds")

#random effects
raneffects_b <- ranef(fit_atp_b)

#fixed effects
fixeffects_b <- fixef(fit_atp_b)

#loo-cv for binary model
loo(fit_atp_b)


#adjacent category model using round as outcome variable, 15 hours to run
formula_atp_tennis <- 
  bf(acatround ~ ht + age + rank + (surface | name) +
       tourney_level + draw_size)
prior_atp_tennis <- prior("normal(0,5)", class = "sd", group = "name")
t.start <- Sys.time()
fit_atp_tennis_a <- brm(formula = formula_atp_tennis,
                        data = model_data,
                        family = brmsfamily("acat", "logit"),
                        prior = prior_atp_tennis,
                        warmup = 1000, iter = 3000,
                        cores = 4, chains = 4,
                        seed = 12345
)
Sys.time() - t.start

fit_atp_a = readRDS("atp_2.rds")


#loo-cv for acat model
loo(fit_atp_a)

# --------------------------------- Plots ------------------------------------ #

#brms plots for both models
binary_plot <- plot(fit_atp_b)
acat_plot <- plot(fit_atp_a)

#fixed effects brms plots
acat_plot[[2]]
acat_plot[[3]]

themem = theme_minimal()

#conditional effects of predictors for models
conditional_effects(fit_atp_b)
condplotc <- conditional_effects(fit_atp_a, categorical = TRUE)
plot(condplotc, theme = themem)

condplot <- conditional_effects(fit_atp_a)
plot(condplot, theme = themem)

#rank plot
rank_ce = brms::conditional_effects(fit_atp_a,
                                    effects = "rank",
                                    categorical = TRUE)

darker_colors = RColorBrewer::brewer.pal(n = 9, "YlGnBu")[3:9]

plot(rank_ce, plot = FALSE, theme = themem)[[1]] + 
  scale_color_manual(values = darker_colors) + 
  scale_fill_manual(values = darker_colors)

#random effects
raneffects_a <- ranef(fit_atp_a)
#on clay
raneffects_a$name[,,1]
#on grass
raneffects_a$name[,,2]
#on hard
raneffects_a$name[,,3]

#fixed effects
fixeffects_a <- fixef(fit_atp_a)


#exponentiate parameter effects to get odds ratio for grand slam winners on clay
coef1_c <- tibble(
                name = rownames(raneffects_a$name[,,1]),
                estimate = raneffects_a$name[,,1][,1],
                lower = raneffects_a$name[,,1][,3],
                upper = raneffects_a$name[,,1][,4]) %>%
  filter(name == "Roger Federer" | name == "Rafael Nadal" 
         | name == "Novak Djokovic" | name == "Andy Murray" | 
           name == "Stan Wawrinka" | name == "Marin Cilic" |
           name == "Dominic Thiem" ) %>%
  ggplot(aes(y = name, x = exp(estimate), 
             xmin = exp(lower), xmax = exp(upper))) +
  scale_x_log10() +
  geom_pointinterval(color = "coral1") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) +
  labs(x = "Odds Ratio", y = "Name", 
       title = "Player Effects for Grand Slam Winners on Clay")

#exponentiate parameter effects to get odds ratio for younger players on clay
coef2_c <- tibble(
  name = rownames(raneffects_a$name[,,1]),
  estimate = raneffects_a$name[,,1][,1],
  lower = raneffects_a$name[,,1][,3],
  upper = raneffects_a$name[,,1][,4]) %>%
  filter(name == "Dominic Thiem" | name == "Alexander Zverev" 
         | name == "Daniil Medvedev" | name == "Stefanos Tsitsipas"
         | name == "Andrey Rublev" | name == "Matteo Berrettini") %>%
  ggplot(aes(y = name, x = exp(estimate), xmin = exp(lower), 
             xmax = exp(upper))) +
  geom_pointinterval(color = "coral1") + 
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) +
  labs(x = "Odds Ratio", y = "Name", 
       title = "Player Effects for Younger Players on Clay")

coefplot_c = coef1_c + coef2_c
ggsave("oddsratioplotclay.png", width = 8, height = 3, units = "in", dpi = 400)

#exponentiate parameter effects to get odds ratio 
#for grand slam winners on grass
coef1_g <- tibble(
  name = rownames(raneffects_a$name[,,2]),
  estimate = raneffects_a$name[,,2][,1],
  lower = raneffects_a$name[,,2][,3],
  upper = raneffects_a$name[,,2][,4]) %>%
  filter(name == "Roger Federer" | name == "Rafael Nadal" 
         | name == "Novak Djokovic" | name == "Andy Murray" | 
           name == "Stan Wawrinka" | name == "Marin Cilic" |
           name == "Dominic Thiem" ) %>%
  ggplot(aes(y = name, x = exp(estimate), 
             xmin = exp(lower), xmax = exp(upper))) +
  scale_x_log10() +
  geom_pointinterval(color = "darkgreen") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) +
  labs(x = "Odds Ratio", y = "Name", 
       title = "Player Effects for Grand Slam Winners on Grass")

#exponentiate parameter effects to get odds ratio for younger players on grass
coef2_g <- tibble(
  name = rownames(raneffects_a$name[,,2]),
  estimate = raneffects_a$name[,,2][,1],
  lower = raneffects_a$name[,,2][,3],
  upper = raneffects_a$name[,,2][,4]) %>%
  filter(name == "Dominic Thiem" | name == "Alexander Zverev" 
         | name == "Daniil Medvedev" | name == "Stefanos Tsitsipas"
         | name == "Andrey Rublev" | name == "Matteo Berrettini") %>%
  ggplot(aes(y = name, x = exp(estimate), xmin = exp(lower), 
             xmax = exp(upper))) +
  geom_pointinterval(color = "darkgreen") + 
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) +
  labs(x = "Odds Ratio", y = "Name", 
       title = "Player Effects for Younger Players on Grass")

coefplot_g = coef1_g + coef2_g
ggsave("oddsratioplotgrass.png", width = 8, height = 3, units = "in", dpi = 400)

#exponentiate parameter effects to get odds ratio 
#for grand slam winners on hard
coef1_h <- tibble(
  name = rownames(raneffects_a$name[,,3]),
  estimate = raneffects_a$name[,,3][,1],
  lower = raneffects_a$name[,,3][,3],
  upper = raneffects_a$name[,,3][,4]) %>%
  filter(name == "Roger Federer" | name == "Rafael Nadal" 
         | name == "Novak Djokovic" | name == "Andy Murray" | 
           name == "Stan Wawrinka" | name == "Marin Cilic" |
           name == "Dominic Thiem" ) %>%
  ggplot(aes(y = name, x = exp(estimate), 
             xmin = exp(lower), xmax = exp(upper))) +
  scale_x_log10() +
  geom_pointinterval(color = "darkblue") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) +
  labs(x = "Odds Ratio", y = "Name", 
       title = "Player Effects for Grand Slam Winners on Hard Court")

#exponentiate parameter effects to get odds ratio for younger players on grass
coef2_h <- tibble(
  name = rownames(raneffects_a$name[,,3]),
  estimate = raneffects_a$name[,,3][,1],
  lower = raneffects_a$name[,,3][,3],
  upper = raneffects_a$name[,,3][,4]) %>%
  filter(name == "Dominic Thiem" | name == "Alexander Zverev" 
         | name == "Daniil Medvedev" | name == "Stefanos Tsitsipas"
         | name == "Andrey Rublev" | name == "Matteo Berrettini") %>%
  ggplot(aes(y = name, x = exp(estimate), xmin = exp(lower), 
             xmax = exp(upper))) +
  geom_pointinterval(color = "darkblue") + 
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(size = 9)) +
  labs(x = "Odds Ratio", y = "Name", 
       title = "Player Effects for Younger Players on Hard Court")

coefplot_h = coef1_h + coef2_h
ggsave("oddsratioplothard.png", width = 8, height = 3, units = "in", dpi = 400)

#distribution of odds ratio for player
playerhist <- ggplot(data = playercoeffs, aes(x = exp(r_name))) + 
  geom_histogram(bins = 20, color = "black", fill = "gray") +
  theme_minimal() +
  labs(title = "Distribution of Player Effect", x = "Odds Ratio", 
       y = "Count")

#table for random effects
rantable <- summary(fit_atp_a)$random[[1]]
knitr::kable(rantable, booktabs = TRUE, "latex") %>%
  kable_styling(latex_options="scale_down")

#table for fixed effects
fixtable <- summary(fit_atp_a)$fixed
knitr::kable(fixtable, booktabs = TRUE, "latex") %>%
  kable_styling(latex_options="scale_down")





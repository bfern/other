library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

results <- read_csv("five-a-side/data/results.csv")

results_long <- bind_rows(
  results %>%
    rename(
      attack = home,
      defence = away,
      goals = home_goals
    ) %>%
    select(attack, defence, goals),
  results %>%
    rename(
      attack = away,
      defence = home,
      goals = away_goals
    ) %>%
    select(attack, defence, goals),  
)

## Simple Poisson Model To Estimate attack and defence coefficients for each team
model <- glm(goals ~ attack + defence, results_long, family = "poisson")

## Lets make a table of all the team strengths
coefs_df <- data.frame(team_name = character(0), attack = numeric(0), defence = numeric(0))
teams <- unique(results_long$attack)
for (team in teams) {
  if (team == teams[1]) {
    attack_coef <- 0
    defence_coef <- 0
  }
  else {
    model_coef_names <- names(model$coefficients)[str_detect(names(model$coefficients), team)]
    attack_coef <- model$coefficients[model_coef_names[1]]
    defence_coef <- model$coefficients[model_coef_names[2]]
  }
  coefs_df <- coefs_df %>%
    add_row(
      team_name = team,
      attack = attack_coef,
      defence = defence_coef
    )
}

## Reposition team strengths so that 0 is average
mean_attack <- mean(coefs_df$attack)
mean_defence <- mean(coefs_df$defence)
coefs_df <- coefs_df %>%
  mutate(
    attack = attack - mean_attack,
    defence = -(defence - mean_defence)
  )

## The intercept needs recalculating
intercept <- model$coefficients[["(Intercept)"]]
intercept <- intercept + mean_attack + mean_defence

## Plot of all the team coefficients
coefs_df %>%
  ggplot(aes(x = defence, y = attack, label = team_name)) +
  geom_point() +
  geom_text() +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1))
  

## The following functions get expected results and scorelines

get_expected_goals <- function(attack_team, defence_team, team_coefs_df, model_intercept) {
  if (!attack_team %in% team_coefs_df$team_name) {
    stop(attack_team, " not in team_coefs_df. Check that the team name is spelt correctly.")
  }
  else if (!defence_team %in% team_coefs_df$team_name) {
    stop(defence_team, " not in team_coefs_df. Check that the team name is spelt correctly.")
  } else {
    exp(model_intercept +
          team_coefs_df$attack[team_coefs_df$team_name == attack_team] -
          team_coefs_df$defence[team_coefs_df$team_name == defence_team]
    )
  }
}

get_expected_scoreline <- function(team1, team2, team_coefs_df = coefs_df, model_intercept = intercept) {
  xg_team1 <- get_expected_goals(team1, team2, team_coefs_df, model_intercept)
  xg_team2 <- get_expected_goals(team2, team1, team_coefs_df, model_intercept)
  print(str_c("Expected goals for ", team1, ": ", round(xg_team1, 2)))
  print(str_c("Expected goals for ", team2, ": ", round(xg_team2, 2)))
}

get_result_probabilities <- function(team1, team2, team_coefs_df = coefs_df, model_intercept = intercept, num_sims = 10000) {
  xg_team1 <- get_expected_goals(team1, team2, team_coefs_df, model_intercept)
  xg_team2 <- get_expected_goals(team2, team1, team_coefs_df, model_intercept)
  goals_scored_team1 <- rpois(num_sims, xg_team1)
  goals_scored_team2 <- rpois(num_sims, xg_team2)
  print(str_c(team1, " Win probability: ", round(mean(goals_scored_team1 > goals_scored_team2), 3)*100, "%"))
  print(str_c("Draw probability : ", round(mean(goals_scored_team1 == goals_scored_team2), 3)*100, "%"))
  print(str_c(team2, " Win probability: ", round(mean(goals_scored_team1 < goals_scored_team2), 3)*100, "%"))
}


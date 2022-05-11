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
    dplyr::select(attack, defence, goals),
  results %>%
    rename(
      attack = away,
      defence = home,
      goals = away_goals
    ) %>%
    dplyr::select(attack, defence, goals),  
)


model <- glm(goals ~ attack + defence, results_long, family = "poisson")

coefs_df <- data.frame(team_name = character(0), attack = numeric(0), defence = numeric(0))

teams <- unique(results_long$attack)

intercept <- model$coefficients[["(Intercept)"]]

ave_coef <- log(mean(results_long$goals))

for (team in teams) {
  if (team == "Chook Chook Albion") {
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

mean_attack <- mean(coefs_df$attack)
mean_defence <- mean(coefs_df$defence)

coefs_df <- coefs_df %>%
  mutate(
    attack = attack - mean_attack,
    defence = -(defence - mean_defence)
  )

intercept <- intercept + mean_attack + mean_defence

coefs_df %>%
  ggplot(aes(x = defence, y = attack, label = team_name)) +
  geom_point() +
  geom_text() +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1))
  


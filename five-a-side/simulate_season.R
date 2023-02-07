source("five-a-side/script.R")
set.seed(1)

teams <- c(
  "Prosecco Bois", "New Levels FC", "Fishermans Friends", "Chook Chook Albion", "Legacy Football",
  "Stevos Steiners", "Contrarian FC", "Penalty Clause", "Wolves of Warren St"
)

current_table <- tibble(
  position = 1:9,
  team_name = teams,
  points = c(12, 9, 9, 9, 9, 4, 3, 3, 1),
  goal_difference = c(18, 10, 10, 6, -3, -5, -9, -14, -13)
)

num_sims <- 10000

fixtures_df <- read_csv("five-a-side/data/fixtures.csv") %>%
  left_join(coefs_df, by = c("home" = "team_name")) %>%
  rename(home_attack = attack, home_defence = defence) %>%
  left_join(coefs_df, by = c("away" = "team_name")) %>%
  rename(away_attack = attack, away_defence = defence) %>%
  mutate(
    goals_for_home_param = exp(intercept + home_attack - away_defence),
    goals_for_away_param = exp(intercept - home_defence + away_attack)
  )
  
simulated_home_goals_matrix <- sapply(fixtures_df$goals_for_home_param, function(x) rpois(num_sims, x))
simulated_away_goals_matrix <- sapply(fixtures_df$goals_for_away_param, function(x) rpois(num_sims, x))
simulated_goal_difference_matrix <- simulated_home_goals_matrix - simulated_away_goals_matrix
simulated_home_points_matrix <- (sign(simulated_goal_difference_matrix)+1)^(log(3, 2))
simulated_away_points_matrix <- (1-sign(simulated_goal_difference_matrix))^(log(3, 2))

final_league_positions <- list()
for (team in teams) {
  final_league_positions[team] <- c()
}
for (i in 1:num_sims) {
  simulated_table <- current_table
  for (j in 1:length(teams)) {
    team <- teams[j]
    home_indexes <- which(fixtures_df$home == team)
    away_indexes <- which(fixtures_df$away == team)
    additional_points <- sum(simulated_home_points_matrix[i, home_indexes]) +
      sum(simulated_away_points_matrix[i, away_indexes])
    additional_goal_difference <- sum(simulated_goal_difference_matrix[i, home_indexes]) -
      sum(simulated_goal_difference_matrix[i, away_indexes])
    simulated_table$points[j] <- simulated_table$points[j] + additional_points
    simulated_table$goal_difference[j] <- simulated_table$goal_difference[j] + additional_goal_difference
  }
  simulated_table$random_number <- runif(9)
  simulated_table <- simulated_table %>%
    arrange(desc(points), desc(goal_difference), desc(random_number)) %>%
    mutate(position = 1:n())
  for (j in 1:nrow(simulated_table)) {
    final_league_positions[[simulated_table$team_name[j]]] <- c(final_league_positions[[simulated_table$team_name[j]]], j)
  }
}

final_league_positions_df <- tibble(team = character(0), position = numeric(0))
for (team_name in names(final_league_positions)) {
  final_league_positions_df <- bind_rows(final_league_positions_df, tibble(team = team_name, position = final_league_positions[[team_name]]))
}

simulation_grid <- final_league_positions_df %>%
  group_by(team, position) %>%
  summarise(prop = n()/10000) %>%
  pivot_wider(names_from = position, values_from = prop)

simulation_grid[is.na(simulation_grid)] <- 0

print(simulation_grid)

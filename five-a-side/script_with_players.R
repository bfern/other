### To do this we need to make a matrix that corresponds to what we want

head(results_long)

players <- read_csv("five-a-side/data/players.csv")
players$X1 <- NULL
players <- t(players)

obs_matrix <- matrix(0, nrow = nrow(results_long), ncol = 33)

teams <- setdiff(unique(results_long$attack), c("Fishermans Friends", "Chook Chook Albion"))

num_times_seen_fishermans_friends_attack <- 0
num_times_seen_fishermans_friends_defence <- 0
for (i in 1:nrow(results_long)) {
  
  ## intercept term
  
  obs_matrix[i,1] <- 1
  
  
  ## now do attack rating
  
  attack_team <- results_long$attack[i]
  
  if (!attack_team %in% c("Fishermans Friends", "Chook Chook Albion")) {
    team_no <- which(attack_team == teams)
    obs_matrix[i, team_no+1] <- 1
  }
  
  if (attack_team == "Fishermans Friends") {
    num_times_seen_fishermans_friends_attack <- num_times_seen_fishermans_friends_attack + 1
    players_for_match <- which(players[num_times_seen_fishermans_friends_attack,] == 1)
    for (player_index in players_for_match) {
      obs_matrix[i, player_index+9] <- 0.2
    }
  }
  
  ## now do defence rating
  
  defence_team <- results_long$defence[i]
  
  if (!defence_team %in% c("Fishermans Friends", "Chook Chook Albion")) {
    team_no <- which(attack_team == teams)
    obs_matrix[i, team_no+17] <- 1
  }
  
  if (defence_team == "Fishermans Friends") {
    num_times_seen_fishermans_friends_defence <- num_times_seen_fishermans_friends_defence + 1
    players_for_match <- which(players[num_times_seen_fishermans_friends_defence,] == 1)
    for (player_index in players_for_match) {
      obs_matrix[i, player_index+25] <- 0.2
    }    
  }
  
  
}


model <- glm(results_long$goals ~ 0 + obs_matrix, family = "poisson")

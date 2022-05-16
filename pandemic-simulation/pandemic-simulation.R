population_size <- 10000
reproduction_rate <- 1.4
num_active_cases <- 10
num_cases_by_week <- num_active_cases

population_ids <- 1:population_size
uninfected_members <- population_ids
current_infected_members <- sample(uninfected_members, num_active_cases)

while (num_active_cases > 0) {
  num_people_each_person_infects <- rpois(num_active_cases, reproduction_rate)
  current_infected_members <- c()
  for (val in num_people_each_person_infects) {
    infected_people <- intersect(sample(population_ids, val), uninfected_members)
    current_infected_members <- c(current_infected_members, infected_people)
    uninfected_members <- setdiff(uninfected_members, infected_people)
  }
  num_active_cases <- length(unique(current_infected_members))
  num_cases_by_week <- c(num_cases_by_week, num_active_cases)
}

plot(1:length(num_cases_by_week), num_cases_by_week)
print(sum(num_cases_by_week))
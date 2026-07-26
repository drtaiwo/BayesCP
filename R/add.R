tournament_summary <- matches |>
  dplyr::group_by(tournament_year) |>
  dplyr::summarise(
    matches = dplyr::n(),
    teams = dplyr::n_distinct(c(team_i, team_j)),
    total_goals = sum(goals_i + goals_j, na.rm = TRUE),
    goals_per_match = mean(goals_i + goals_j, na.rm = TRUE),
    median_goals = median(goals_i + goals_j, na.rm = TRUE),
    scoreless_draws = sum(goals_i == 0 & goals_j == 0, na.rm = TRUE),
    draws = sum(goals_i == goals_j, na.rm = TRUE),
    draw_percentage = 100 * mean(goals_i == goals_j, na.rm = TRUE),
    extra_time_matches = sum(after_extra_time, na.rm = TRUE),
    penalty_shootouts = sum(penalty_shootout, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    goals_per_match = round(goals_per_match, 3),
    median_goals = round(median_goals, 2),
    draw_percentage = round(draw_percentage, 2)
  )

print(tournament_summary)



exists("matches")


head(matches[, c(
  "tournament_year",
  "team_i",
  "team_j",
  "goals_i",
  "goals_j"
)])

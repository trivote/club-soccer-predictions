# Script for pulling and combining FB Ref match logs and Transfermarkt values

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transfermarkt Team Values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get player values from transfermarkt
player_values_raw <-
  worldfootballR::tm_player_market_values(
    country_name = "England",
    start_year = 2019:2022
  )

# wrangle to team total values per season
squad_values <-
  player_values_raw |>
  dplyr::mutate(
    comp_name = dplyr::case_when(country == "England" ~ "Premier League"),
    team = stringr::str_remove(squad, c(" FC|AFC "))
  ) |>
  dplyr::select(
    comp_name,
    team,
    season_start_year,
    player_market_value_euro
  ) |>
  dplyr::rename(
    season = season_start_year,
    league = comp_name
  ) |>
  tidyr::drop_na() |>
  dplyr::summarise(
    value = sum(player_market_value_euro),
    .by = c(team, season)
  ) |>
  dplyr::arrange(desc(value))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Team Match Logs ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

match_urls <-
  worldfootballR::fb_match_urls(
    country = "ENG",
    gender = "M",
    season_end_year = 2020:2023,
    tier = "1st"
  )

get_match_logs <- function(url) {
  worldfootballR::fb_advanced_match_stats(
    match_url = url,
    stat_type = "summary",
    team_or_player = "player"
  )
}

match_logs_raw <-
  purrr::pmap(list(match_urls), get_match_logs) |>
  dplyr::bind_rows()

match_logs <-
  match_logs_raw |>
  janitor::clean_names(
    replace = c(
      "_Expected" = "",
      "PK" = "pk",
      "xG" = "xg",
      "xAG" = "xag"
    )
  ) |>
  tidyr::pivot_wider(
    names_from = home_away,
    values_from = npxg
  ) |>
  dplyr::rename(
    date = match_date,
    home_npxg = Home,
    away_npxg = Away
  ) |>
  tidyr::fill(home_npxg, .direction = "down") |>
  tidyr::fill(away_npxg, .direction = "up") |>
  dplyr::select(
    date,
    dplyr::starts_with(c("home", "away")) &
      dplyr::ends_with(c("_team", "_score", "_xg", "_npxg"))
  ) |>
  dplyr::distinct() |>
  dplyr::rename(
    home_goals = home_score,
    away_goals = away_score
  ) |>
  dplyr::relocate("away_team", .after = "home_team") |>
  dplyr::mutate(
    season = dplyr::case_when(
      date <= "2020-07-26" ~ 2019,
      dplyr::between(date, "2020-07-26", "2021-05-23") ~ 2020,
      dplyr::between(date, "2021-05-23", "2022-05-22") ~ 2021,
      dplyr::between(date, "2022-05-22", "2023-05-28") ~ 2022,
    )
  ) |>
  dplyr::relocate("season", .after = "date")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combined Team Dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

team_data <-
  match_logs |>
  dplyr::full_join(
    squad_values,
    by = dplyr::join_by(
      "home_team" == "team",
      season
    )
  ) |>
  dplyr::rename(
    home_value = value
  ) |>
  dplyr::full_join(
    squad_values,
    by = dplyr::join_by(
      "away_team" == "team",
      season
    )
  ) |>
  dplyr::rename(
    away_value = value
  )

readr::write_csv(team_data, here::here("data", "team_data.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Player Shot Logs ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

match_urls <-
  worldfootballR::fb_match_urls(
    country = "ENG",
    gender = "M",
    season_end_year = 2020:2023,
    tier = "1st"
  )

get_shot_logs <- function(url) {
  worldfootballR::fb_match_shooting(
    match_url = url
  )
}

shot_logs_raw <-
  purrr::pmap(list(match_urls), get_shot_logs) |>
  dplyr::bind_rows()

shot_logs <-
  shot_logs_raw |>
  janitor::clean_names(
    replace = c(
      "_Expected" = "",
      "PK" = "pk",
      "xG" = "xg",
      "PS" = "ps",
      "xAG" = "xag"
    )
  ) |>
  dplyr::rename(team = squad) |>
  dplyr::mutate(
    team = stringr::str_replace_all(team, "Utd", "United"),
    team = dplyr::case_when(
      team == "West Ham" ~ "West Ham United",
      team == "Brighton" ~ "Brighton & Hove Albion",
      team == "Tottenham" ~ "Tottenham Hotspur",
      team == "Nott'ham Forest" ~ "Nottingham Forest",
      team == "Wolves" ~ "Wolverhampton Wanderers",
      team == "West Brom" ~ "West Bromwich Albion",
      .default = team
    )
  ) |>
  dplyr::full_join(
    match_logs |> dplyr::select(date, team, opponent),
    by = c("date", "team")
  ) |>
  dplyr::relocate(opponent, .after = team)

readr::write_csv(shots, here::here("data", "player_data.csv"))

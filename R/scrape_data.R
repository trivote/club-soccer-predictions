# Script for pulling and combining FB Ref match logs and Transfermarkt values

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Squad Values ----
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
    .by = c(team, league, season)
  ) |>
  dplyr::arrange(desc(value))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Squad Match Logs ----
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
  tidyr::drop_na(home_goals) |>
  dplyr::mutate(
    opponent = dplyr::case_when(
      home_away == "Home" ~ away_team,
      home_away == "Away" ~ home_team
    )
  ) |>
  dplyr::select(team, home_away, opponent, gls, xg, npxg)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combined Dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

team_data <-
  match_logs |>
  dplyr::full_join(squad_values, by = "team", relationship = "many-to-many")

readr::write_csv(team_data, here::here("data", "team_data.csv"))
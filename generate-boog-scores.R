#!/usr/bin/env Rscript

library(tidyverse)

first_season_for_logit <- 1945

print("Loading names and active players...")

chadwick_names <- read_csv("data/all-people.csv") |>
    mutate(name = if_else(is.na(name_suffix), paste(name_first, name_last), paste(name_first, name_last, name_suffix))) |>
    select(key_bbref, key_fangraphs, key_mlbam, key_person, name) |>
    filter(!is.na(key_fangraphs) | !is.na(key_mlbam) | !is.na(key_bbref))

active_batters <- read_csv("data/active-batters.csv") |> 
    left_join(chadwick_names, by = join_by(key_bref == key_bbref)) |> 
    select(key_person)

active_pitchers <- read_csv("data/active-pitchers.csv") |> 
    left_join(chadwick_names, by = join_by(key_bref == key_bbref)) |> 
    select(key_person)

active_players <- bind_rows(active_batters, active_pitchers) |> distinct(key_person) |> mutate(active = 1) |> filter(!is.na(key_person))

print("Loading seasonal data...")

starting_pitching_data <- read_csv("data/starting-pitcher-season-values.csv") |> 
    mutate(HBP = replace_na(HBP, 0), BF = if_else(is.na(TBF), 3 * round(IP) + H + BB + HBP, TBF), role = "start", WAR = (fWAR + rWAR) / 2) |>
    select(League, Name, Season, Team, Age, PlayerId, MLBAMID, WAR, BF, IP, role)

relief_pitching_data <- read_csv("data/relief-pitcher-season-values.csv") |>
    mutate(HBP = replace_na(HBP, 0), BF = if_else(is.na(TBF), 3 * round(IP) + H + BB + HBP, TBF), role = "relief", WAR = (fWAR + rWAR) / 2) |>
    select(League, Name, Season, Team, Age, PlayerId, MLBAMID, WAR, BF, IP, role)

pitching_data <- bind_rows(starting_pitching_data, relief_pitching_data) |>
    left_join(chadwick_names, by = join_by(PlayerId == key_fangraphs), na_matches = "never") |>
    left_join(chadwick_names, by = join_by(MLBAMID == key_mlbam), na_matches = "never") |>
    mutate(key_person = if_else(is.na(key_person.x), key_person.y, key_person.x)) |>
    mutate(name = if_else(is.na(key_person.x), if_else(is.na(key_person.y), Name, name.y), name.x)) |>
    rename(league = League, season = Season, team = Team, age = Age, fangraphs_id = PlayerId, mlbam_id = MLBAMID) |>
    select(!Name) |>
    select(!key_bbref.x:name.y) |>
    mutate(WAA = WAR - (round(IP) * (sum(WAR) / sum(round(IP)))), .by = c(league, season)) |>
    select(!league) |> select(!WAR) |>
    relocate(name) |>
    summarize(WAA = sum(WAA), BF = sum(BF), .by = c(name, season, age, fangraphs_id, mlbam_id, key_person, role)) |>
    pivot_wider(names_from = role, names_glue = "WAA_{role}", values_from = WAA)
    

batting_data <- read_csv("data/batter-season-values.csv") |> 
    select(Name, Season, Team, Age, PlayerId, MLBAMID, WAA, PA) |>
    rename(WAA_bat = WAA) |>
    left_join(chadwick_names, by = join_by(PlayerId == key_fangraphs), na_matches = "never") |>
    left_join(chadwick_names, by = join_by(MLBAMID == key_mlbam), na_matches = "never") |>
    mutate(key_person = if_else(is.na(key_person.x), key_person.y, key_person.x)) |>
    mutate(name = if_else(is.na(key_person.x), if_else(is.na(key_person.y), Name, name.y), name.x)) |>
    rename(season = Season, team = Team, age = Age, fangraphs_id = PlayerId, mlbam_id = MLBAMID) |>
    select(!Name) |>
    select(!key_bbref.x:name.y) |>
    relocate(name)

hall_of_famers <- read_csv("data/hall-of-famers.csv") |>
    left_join(chadwick_names, by = join_by(key_bref == key_bbref), na_matches = "never") |>
    mutate(hof = 1, key_person = replace_na(key_person, Name), bbwaa = if_else(BBWAA, 1, 0), .by = Name) |>
    select(key_person, hof, bbwaa)

combined_data <- bind_rows(pitching_data, batting_data) |> 
    filter(!is.na(age)) |>
    mutate(key_person = if_else(is.na(key_person), as.character(fangraphs_id), key_person)) |> 
    select(name, season, age, WAA_bat, WAA_start, WAA_relief, BF, PA, key_person)

curr_season <- max(combined_data$season)

print("Combining data sets and summarizing BOOG scores..")

boog_seasons <- combined_data |>
    mutate(
        started = any(!is.na(WAA_start)),
        relieved = any(!is.na(WAA_relief)),
        BF = sum(BF, na.rm = TRUE),
        PA = sum(PA, na.rm = TRUE),
        WAA_bat = sum(WAA_bat, na.rm = TRUE),
        WAA_start = sum(WAA_start, na.rm = TRUE),
        WAA_relief = sum(WAA_relief, na.rm = TRUE),
        season_BOOG = max(WAA_bat + WAA_start + WAA_relief, 0),
        .by = c(key_person, season)
    ) |> filter(sum(season_BOOG) > 0, .by = key_person) |>
    slice(1, .by = c(key_person, season)) |> 
    left_join(hall_of_famers, by = join_by(key_person)) |>
    left_join(active_players, by = join_by(key_person)) |>
    mutate(
        active = replace_na(active, 0), 
        hof = replace_na(hof, 0), 
        bbwaa = replace_na(bbwaa, 0),
        BOOG_bat = case_when(
            PA >= BF & WAA_bat > 0 ~ WAA_bat,
            .default = 0
        ),
        BOOG_pitch = case_when(
            PA >= BF & WAA_start + WAA_relief < 0 ~ 0,
            PA >= BF ~ WAA_start + WAA_relief,
            .default = season_BOOG
        ),
        BOOG_start = case_when(
            !started ~ 0, 
            PA >= BF & WAA_start < 0 ~ 0,
            PA >= BF ~ WAA_start,
            WAA_start + WAA_bat < 0 ~ 0,
            .default = WAA_start + WAA_bat
        ),
        BOOG_relief = case_when(
            !relieved ~ 0,
            started & WAA_relief < 0 ~ 0,
            started ~ WAA_relief,
            PA >= BF & WAA_relief < 0 ~ 0,
            PA >= BF ~ WAA_relief,
            WAA_relief + WAA_bat < 0 ~ 0,
            .default = WAA_relief + WAA_bat
        ),
    ) |> select(!WAA_bat:PA) |> select(!started:relieved) |>
    group_by(key_person) |>
    mutate(
        delta = min(season) - min(age), 
        max_season = if_else(active == 1, curr_season, max(season)),
        pitcher = if_else(sum(BOOG_pitch) > sum(BOOG_bat), 1, 0)
    ) |> select(!BOOG_bat:BOOG_pitch) |>
    complete(age = full_seq(age, 1)) |>
    arrange(age) |>
    fill(name, hof, bbwaa, active, delta, max_season, pitcher, .direction = "downup") |>
    ungroup() |> 
    mutate(
        season = if_else(is.na(season), age + delta, season)
    ) |> select(!delta) |>
    arrange(season) |>
    mutate(
        season_BOOG = replace_na(season_BOOG, 0),
        BOOG_start = replace_na(BOOG_start, 0),
        BOOG_relief = replace_na(BOOG_relief, 0)
    ) |> mutate(
        cumsum_BOOG = cumsum(season_BOOG),
        cumsum_BOOG_start = cumsum(BOOG_start),
        cumsum_BOOG_relief = cumsum(BOOG_relief),
        career_BOOG = sum(season_BOOG),
        .by = key_person
    ) |> mutate(
        hof_rate = 0.0,
        bbwaa_rate = 0.0,
        starter = if_else(pitcher & cumsum_BOOG_start >= cumsum_BOOG_relief, 1, 0)
    ) |> relocate(pitcher, .after = season) |>
    relocate(starter, .after = pitcher)

print("Generating HOF data")

hof_projection_data <- boog_seasons |> 
    mutate(starter = if_else(season != max(season), NA, starter), .by = key_person) |>
    group_by(pitcher) |>
    complete(key_person, age) |>
    group_by(key_person) |>
    arrange(age) |>
    fill(cumsum_BOOG) |>
    fill(hof, bbwaa, starter, .direction = "downup") |>
    ungroup() |>
    mutate(season_BOOG = replace_na(season_BOOG, 0), cumsum_BOOG = replace_na(cumsum_BOOG, 0))

median_hof_batter <- hof_projection_data |> filter(hof == 1, pitcher == 0) |>
    summarize(median_season_BOOG = median(season_BOOG), median_cumsum_BOOG = median(cumsum_BOOG), .by = c(age, pitcher)) |>
    filter(median_cumsum_BOOG > 0) |>
    rename(season_BOOG = median_season_BOOG, cumsum_BOOG = median_cumsum_BOOG) |>
    mutate(name = "Median HOF Batter", key_person = name, starter = 0)

median_bbwaa_batter <- hof_projection_data |> filter(bbwaa == 1, pitcher == 0) |>
    summarize(median_season_BOOG = median(season_BOOG), median_cumsum_BOOG = median(cumsum_BOOG), .by = c(age, pitcher)) |>
    filter(median_cumsum_BOOG > 0) |>
    rename(season_BOOG = median_season_BOOG, cumsum_BOOG = median_cumsum_BOOG) |>
    mutate(name = "Median BBWAA Batter", key_person = name, starter = 0)

median_hof_pitcher <- hof_projection_data |> filter(hof == 1, pitcher == 1) |>
    summarize(median_season_BOOG = median(season_BOOG), median_cumsum_BOOG = median(cumsum_BOOG), .by = c(age, starter)) |>
    filter(median_cumsum_BOOG > 0) |>
    rename(season_BOOG = median_season_BOOG, cumsum_BOOG = median_cumsum_BOOG) |>
    mutate(name = if_else(starter == 1, "Median HOF SP", "Median HOF RP"), key_person = name, pitcher = 1)

median_bbwaa_pitcher <- hof_projection_data |> filter(bbwaa == 1, pitcher == 1) |>
    summarize(median_season_BOOG = median(season_BOOG), median_cumsum_BOOG = median(cumsum_BOOG), .by = c(age, starter)) |>
    filter(median_cumsum_BOOG > 0) |>
    rename(season_BOOG = median_season_BOOG, cumsum_BOOG = median_cumsum_BOOG) |>
    mutate(name = if_else(starter == 1, "Median BBWAA SP", "Median BBWAA RP"), key_person = name, pitcher = 1)

hof_batter_ct <- nrow(boog_seasons |> filter(pitcher == 0, hof == 1) |> distinct(key_person))
hof_sp_ct <- nrow(boog_seasons |> filter(season == max_season, pitcher == 1, starter == 1, hof == 1) |> distinct(key_person))
hof_rp_ct <- nrow(boog_seasons |> filter(season == max_season, pitcher == 1, starter == 0, hof == 1) |> distinct(key_person))
bbwaa_batter_ct <- nrow(boog_seasons |> filter(pitcher == 0, bbwaa == 1) |> distinct(key_person))
bbwaa_sp_ct <- nrow(boog_seasons |> filter(season == max_season, pitcher == 1, starter == 1, bbwaa == 1) |> distinct(key_person))
bbwaa_rp_ct <- nrow(boog_seasons |> filter(season == max_season, pitcher == 1, starter == 0, bbwaa == 1) |> distinct(key_person))

min_player_ct <-  2 * max(c(hof_batter_ct, hof_sp_ct, hof_rp_ct, bbwaa_batter_ct, bbwaa_sp_ct, bbwaa_rp_ct))

marginal_rows <- tibble(
    key_person = character(),
    name = character(),
    age = numeric(),
    cumsum_BOOG = numeric(),
    pitcher = numeric(),
    starter = numeric()
)

for (player_age in (hof_projection_data |> distinct(pitcher, age) |> count(age) |> filter(n == 2))$age) {
    if (nrow(hof_projection_data |> filter(age == player_age)) >= min_player_ct) {
        marginal_rows <- marginal_rows |> add_row(
            key_person = "Marginal HOF Batter",
            name = "Marginal HOF Batter",
            age = player_age,
            cumsum_BOOG = min((hof_projection_data |> filter(pitcher == 0, age == player_age) |> slice_max(cumsum_BOOG, n = hof_batter_ct))$cumsum_BOOG),
            pitcher = 0,
            starter = 0,
        ) |>
        add_row(
            key_person = "Marginal HOF SP",
            name = "Marginal HOF SP",
            age = player_age,
            cumsum_BOOG = min((hof_projection_data |> filter(pitcher == 1, starter == 1, age == player_age) |> slice_max(cumsum_BOOG, n = hof_sp_ct))$cumsum_BOOG),
            pitcher = 1,
            starter = 1
        ) |>
        add_row(
            key_person = "Marginal HOF RP",
            name = "Marginal HOF RP",
            age = player_age,
            cumsum_BOOG = min((hof_projection_data |> filter(pitcher == 1, starter == 0, age == player_age) |> slice_max(cumsum_BOOG, n = hof_rp_ct))$cumsum_BOOG),
            pitcher = 1,
            starter = 0
        ) |>
        add_row(
            key_person = "Marginal BBWAA Batter",
            name = "Marginal BBWAA Batter",
            age = player_age,
            cumsum_BOOG = min((hof_projection_data |> filter(pitcher == 0, age == player_age) |> slice_max(cumsum_BOOG, n = bbwaa_batter_ct))$cumsum_BOOG),
            pitcher = 0,
            starter = 0
        ) |>
        add_row(
            key_person = "Marginal BBWAA SP",
            name = "Marginal BBWAA SP",
            age = player_age,
            cumsum_BOOG = min((hof_projection_data |> filter(pitcher == 1, starter == 1, age == player_age) |> slice_max(cumsum_BOOG, n = bbwaa_sp_ct))$cumsum_BOOG),
            pitcher = 1,
            starter = 1
        ) |>
        add_row(
            key_person = "Marginal BBWAA RP",
            name = "Marginal BBWAA RP",
            age = player_age,
            cumsum_BOOG = min((hof_projection_data |> filter(pitcher == 1, starter == 0, age == player_age) |> slice_max(cumsum_BOOG, n = bbwaa_rp_ct))$cumsum_BOOG),
            pitcher = 1,
            starter = 0
        )
    }
}

hof_pace <- bind_rows(median_hof_batter, median_hof_pitcher, median_bbwaa_batter, median_bbwaa_pitcher, marginal_rows) |> mutate(hof_rate = 0.0, bbwaa_rate = 0.0)

print("Calculating logistic regressions...")

logit_young_batter <- boog_seasons |> filter(cumsum_BOOG > 0, age <= 21, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 0, .by = key_person) |> 
    select(hof, bbwaa, cumsum_BOOG)

logit_final_batter <- boog_seasons |> filter(cumsum_BOOG > 0, season == max_season, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 0, .by = key_person) |>
    select(hof, bbwaa, cumsum_BOOG)

logit_young_sp <- boog_seasons |> filter(cumsum_BOOG > 0, age <= 21, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 1, starter == 1, .by = key_person) |> 
    select(hof, bbwaa, cumsum_BOOG)

logit_young_rp <- boog_seasons |> filter(cumsum_BOOG > 0, age <= 21, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof) & pitcher == 1, starter == 0, .by = key_person) |> 
    select(hof, bbwaa, cumsum_BOOG)

logit_final_sp <- boog_seasons |> filter(cumsum_BOOG > 0, season == max_season, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 1, starter == 1, .by = key_person) |>
    select(hof, bbwaa, cumsum_BOOG)

logit_final_rp <- boog_seasons |> filter(cumsum_BOOG > 0, season == max_season, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 1, starter == 0, .by = key_person) |>
    select(hof, bbwaa, cumsum_BOOG)

hof_young_batter_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_young_batter |> select(!bbwaa)))
bbwaa_young_batter_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_young_batter |> select(!hof)))
hof_final_batter_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_final_batter |> select(!bbwaa)))
bbwaa_final_batter_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_final_batter |> select(!hof)))

hof_young_sp_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_young_sp |> select(!bbwaa)))
hof_young_rp_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_young_rp |> select(!bbwaa)))
bbwaa_young_sp_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_young_sp |> select(!hof)))
bbwaa_young_rp_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_young_rp |> select(!hof)))
hof_final_sp_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_final_sp |> select(!bbwaa)))
hof_final_rp_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_final_rp |> select(!bbwaa)))
bbwaa_final_sp_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_final_sp |> select(!hof)))
bbwaa_final_rp_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_final_rp |> select(!hof)))

boog_seasons <- boog_seasons |> mutate(
    hof_in_progress_rate = case_when(
        pitcher == 0 ~ predict(hof_young_batter_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response"),
        cumsum_BOOG_start + cumsum_BOOG_relief > 0 ~ (cumsum_BOOG_start * predict(hof_young_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response") + 
            cumsum_BOOG_relief * predict(hof_young_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response")) / (cumsum_BOOG_start + cumsum_BOOG_relief),
        .default = 0
    ),
    bbwaa_in_progress_rate = case_when(
        pitcher == 0 ~ predict(bbwaa_young_batter_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response"),
        cumsum_BOOG_start + cumsum_BOOG_relief > 0  ~ (cumsum_BOOG_start * predict(bbwaa_young_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response") + 
            cumsum_BOOG_relief * predict(bbwaa_young_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response")) / (cumsum_BOOG_start + cumsum_BOOG_relief),
        .default = 0
    ),
    hof_final_rate = case_when(
        pitcher == 0 ~ predict(hof_final_batter_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response"),
        cumsum_BOOG_start + cumsum_BOOG_relief > 0 ~ (cumsum_BOOG_start * predict(hof_final_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response") +
            cumsum_BOOG_relief * predict(hof_final_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response")) / (cumsum_BOOG_start + cumsum_BOOG_relief),
        .default = 0
    ),
    bbwaa_final_rate = case_when(
        pitcher == 0 ~ predict(bbwaa_final_batter_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response"),
        cumsum_BOOG_start + cumsum_BOOG_relief > 0 ~ (cumsum_BOOG_start * predict(bbwaa_final_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response") +
            cumsum_BOOG_relief * predict(bbwaa_final_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG)), type="response")) / (cumsum_BOOG_start + cumsum_BOOG_relief),
        .default = 0
    ),
    hof_in_progress_starter_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_start == 0 ~ 0,
        .default = predict(hof_young_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_start) |> rename(cumsum_BOOG = cumsum_BOOG_start)), type="response") 
    ),
    bbwaa_in_progress_starter_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_start == 0 ~ 0,
        .default = predict(bbwaa_young_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_start) |> rename(cumsum_BOOG = cumsum_BOOG_start)), type="response") 
    ),
    hof_final_starter_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_start == 0 ~ 0,
        .default = predict(hof_final_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_start) |> rename(cumsum_BOOG = cumsum_BOOG_start)), type="response") 
    ),
    bbwaa_final_starter_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_start == 0 ~ 0,
        .default = predict(bbwaa_final_sp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_start) |> rename(cumsum_BOOG = cumsum_BOOG_start)), type="response") 
    ),
    hof_in_progress_relief_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_relief == 0 ~ 0,
        .default = predict(hof_young_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_relief) |> rename(cumsum_BOOG = cumsum_BOOG_relief)), type="response") 
    ),
    bbwaa_in_progress_relief_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_relief == 0 ~ 0,
        .default = predict(bbwaa_young_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_relief) |> rename(cumsum_BOOG = cumsum_BOOG_relief)), type="response") 
    ),
    hof_final_relief_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_relief == 0 ~ 0,
        .default = predict(hof_final_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_relief) |> rename(cumsum_BOOG = cumsum_BOOG_relief)), type="response") 
    ),
    bbwaa_final_relief_rate = case_when(
        pitcher == 0 ~ 0,
        cumsum_BOOG_relief == 0 ~ 0,
        .default = predict(bbwaa_final_rp_model, newdata = (boog_seasons |> select(age, cumsum_BOOG_relief) |> rename(cumsum_BOOG = cumsum_BOOG_relief)), type="response") 
    ),
    hof_in_progress_pitcher_rate = case_when(
        pitcher == 0 ~ 0,
        hof_in_progress_rate > hof_in_progress_starter_rate & hof_in_progress_rate > hof_in_progress_relief_rate ~ hof_in_progress_rate,
        hof_in_progress_starter_rate > hof_in_progress_relief_rate ~ hof_in_progress_starter_rate,
        .default = hof_in_progress_relief_rate
    ),
    bbwaa_in_progress_pitcher_rate = case_when(
        pitcher == 0 ~ 0,
        bbwaa_in_progress_rate > bbwaa_in_progress_starter_rate & bbwaa_in_progress_rate > bbwaa_in_progress_relief_rate ~ bbwaa_in_progress_rate,
        bbwaa_in_progress_starter_rate > bbwaa_in_progress_relief_rate ~ bbwaa_in_progress_starter_rate,
        .default = bbwaa_in_progress_relief_rate
    ),
    hof_final_pitcher_rate = case_when(
        pitcher == 0 ~ 0,
        hof_final_rate > hof_final_starter_rate & hof_final_rate > hof_final_relief_rate ~ hof_final_rate,
        hof_final_starter_rate > hof_final_relief_rate ~ hof_final_starter_rate,
        .default = hof_final_relief_rate
    ),
    bbwaa_final_pitcher_rate = case_when(
        pitcher == 0 ~ 0,
        bbwaa_final_rate > bbwaa_final_starter_rate & bbwaa_final_rate > bbwaa_final_relief_rate ~ bbwaa_final_rate,
        bbwaa_final_starter_rate > bbwaa_final_relief_rate ~ bbwaa_final_starter_rate,
        .default = bbwaa_final_relief_rate
    ),
    hof_rate = case_when(
        cumsum_BOOG == 0 ~ hof_rate,
        pitcher == 0 & (age > 36 | (season == max_season & active == 0)) ~ hof_final_rate,
        age > 36 | (season == max_season & active == 0) ~ hof_final_pitcher_rate,
        pitcher == 0 & age <= 21 ~ hof_in_progress_rate,
        age <= 21 ~ hof_in_progress_pitcher_rate,
        .default = hof_rate
    ),
    bbwaa_rate = case_when(
        cumsum_BOOG == 0 ~ bbwaa_rate,
        pitcher == 0 & (age > 36 | (season == max_season & active == 0)) ~ bbwaa_final_rate,
        age > 36 | (season == max_season & active == 0) ~ bbwaa_final_pitcher_rate,
        pitcher == 0 & age <= 21 ~ bbwaa_in_progress_rate,
        age <= 21 ~ bbwaa_in_progress_pitcher_rate,
        .default = bbwaa_rate
    )
)

hof_pace <- hof_pace |> mutate(
    hof_in_progress_rate = case_when(
        pitcher == 0 ~ predict(hof_young_batter_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 1 ~ predict(hof_young_sp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 0 ~ predict(hof_young_rp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
    ),
    bbwaa_in_progress_rate = case_when(
        pitcher == 0 ~ predict(bbwaa_young_batter_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 1 ~ predict(bbwaa_young_sp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 0 ~ predict(bbwaa_young_rp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
    ),
    hof_final_rate = case_when(
        pitcher == 0 ~ predict(hof_final_batter_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 1 ~ predict(hof_final_sp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 0 ~ predict(hof_final_rp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
    ),
    bbwaa_final_rate = case_when(
        pitcher == 0 ~ predict(bbwaa_final_batter_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 1 ~ predict(bbwaa_final_sp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
        pitcher == 1 & starter == 0 ~ predict(bbwaa_final_rp_model, newdata = (hof_pace |> select(age, cumsum_BOOG)), type="response"),
    ),
    hof_rate = case_when(
        cumsum_BOOG == 0 ~ hof_rate,
        age > 36 ~ hof_final_rate,
        age <= 21 ~ hof_in_progress_rate,
        .default = hof_rate
    ),
    bbwaa_rate = case_when(
        cumsum_BOOG == 0 ~ bbwaa_rate,
        age > 36 ~ bbwaa_final_rate,
        age <= 21 ~ bbwaa_in_progress_rate,
        .default = bbwaa_rate
    )
)

for (curr_age in 22:36) {
    logit_batter <- boog_seasons |> filter(cumsum_BOOG > 0, age == curr_age, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 0, .by = key_person) |>
    select(hof, bbwaa, cumsum_BOOG)

    logit_sp <- boog_seasons |> filter(cumsum_BOOG > 0, age == curr_age, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 1, starter == 1, .by = key_person) |>
    select(hof, bbwaa, cumsum_BOOG)

    logit_rp <- boog_seasons |> filter(cumsum_BOOG > 0, age == curr_age, min(season) >= first_season_for_logit, (max_season <= (curr_season - 15) | hof), pitcher == 1, starter == 0, .by = key_person) |>
    select(hof, bbwaa, cumsum_BOOG)

    hof_batter_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_batter |> select(!bbwaa)))
    bbwaa_batter_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_batter |> select(!hof)))
    hof_sp_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_sp |> select(!bbwaa)))
    hof_rp_model <- glm(hof ~., family = binomial(link = "logit"), data = (logit_rp |> select(!bbwaa)))
    bbwaa_sp_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_sp |> select(!hof)))
    bbwaa_rp_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (logit_rp |> select(!hof)))

    boog_seasons <- boog_seasons |> mutate(
        hof_in_progress_rate = case_when(
            age != curr_age ~ hof_in_progress_rate,
            pitcher == 0 ~ predict(hof_batter_model, newdata = (boog_seasons |> select(cumsum_BOOG)), type="response"),
            pitcher == 1 ~ (cumsum_BOOG_start * predict(hof_sp_model, newdata = (boog_seasons |> select(cumsum_BOOG)), type="response") + 
                cumsum_BOOG_relief * predict(hof_rp_model, newdata = (boog_seasons |> select(cumsum_BOOG)), type="response")) / (cumsum_BOOG_start + cumsum_BOOG_relief)
        ),
        bbwaa_in_progress_rate = case_when(
            age != curr_age ~ bbwaa_in_progress_rate,
            pitcher == 0 ~ predict(bbwaa_batter_model, newdata = (boog_seasons |> select(cumsum_BOOG)), type="response"),
            pitcher == 1 ~ (cumsum_BOOG_start * predict(bbwaa_sp_model, newdata = (boog_seasons |> select(cumsum_BOOG)), type="response") +
                cumsum_BOOG_relief * predict(bbwaa_rp_model, newdata = (boog_seasons |> select(cumsum_BOOG)), type="response")) / (cumsum_BOOG_start + cumsum_BOOG_relief)
        ),
        hof_in_progress_starter_rate = case_when(
            pitcher == 0 ~ 0,
            cumsum_BOOG_start == 0 ~ 0,
            .default = predict(hof_sp_model, newdata = (boog_seasons |> select(cumsum_BOOG_start) |> rename(cumsum_BOOG = cumsum_BOOG_start)), type="response") 
        ),
        bbwaa_in_progress_starter_rate = case_when(
            pitcher == 0 ~ 0,
            cumsum_BOOG_start == 0 ~ 0,
            .default = predict(bbwaa_sp_model, newdata = (boog_seasons |> select(cumsum_BOOG_start) |> rename(cumsum_BOOG = cumsum_BOOG_start)), type="response") 
        ),
        hof_in_progress_relief_rate = case_when(
            pitcher == 0 ~ 0,
            cumsum_BOOG_relief == 0 ~ 0,
            .default = predict(hof_rp_model, newdata = (boog_seasons |> select(cumsum_BOOG_relief) |> rename(cumsum_BOOG = cumsum_BOOG_relief)), type="response") 
        ),
        bbwaa_in_progress_relief_rate = case_when(
            pitcher == 0 ~ 0,
            cumsum_BOOG_relief == 0 ~ 0,
            .default = predict(bbwaa_rp_model, newdata = (boog_seasons |> select(cumsum_BOOG_relief) |> rename(cumsum_BOOG = cumsum_BOOG_relief)), type="response") 
        ),
        hof_in_progress_pitcher_rate = case_when(
            pitcher == 0 ~ 0,
            hof_in_progress_rate > hof_in_progress_starter_rate & hof_in_progress_rate > hof_in_progress_relief_rate ~ hof_in_progress_rate,
            hof_in_progress_starter_rate > hof_in_progress_relief_rate ~ hof_in_progress_starter_rate,
            .default = hof_in_progress_relief_rate
        ),
        bbwaa_in_progress_pitcher_rate = case_when(
            pitcher == 0 ~ 0,
            bbwaa_in_progress_rate > bbwaa_in_progress_starter_rate & bbwaa_in_progress_rate > bbwaa_in_progress_relief_rate ~ bbwaa_in_progress_rate,
            bbwaa_in_progress_starter_rate > bbwaa_in_progress_relief_rate ~ bbwaa_in_progress_starter_rate,
            .default = bbwaa_in_progress_relief_rate
        ),
        hof_rate = case_when(
            age != curr_age ~ hof_rate,
            cumsum_BOOG == 0 ~ 0,
            pitcher == 0 & season == max_season & active == 0 ~ hof_final_rate,
            season == max_season & active == 0 ~ hof_final_pitcher_rate,
            pitcher == 0 & hof_in_progress_rate > hof_final_rate ~ hof_in_progress_rate,
            pitcher == 1 & hof_in_progress_pitcher_rate > hof_final_pitcher_rate ~ hof_in_progress_pitcher_rate,
            pitcher == 1 ~ hof_final_pitcher_rate,
            .default = hof_final_rate
        ),
        bbwaa_rate = case_when(
            age != curr_age ~ bbwaa_rate,
            cumsum_BOOG == 0 ~ 0,
            pitcher == 0 & season == max_season & active == 0 ~ bbwaa_final_rate,
            season == max_season & active == 0 ~ bbwaa_final_pitcher_rate,
            pitcher == 0 & bbwaa_in_progress_rate > bbwaa_final_rate ~ bbwaa_in_progress_rate,
            pitcher == 1 & bbwaa_in_progress_pitcher_rate > bbwaa_final_pitcher_rate ~ bbwaa_in_progress_pitcher_rate,
            pitcher == 1 ~ bbwaa_final_pitcher_rate,
            .default = bbwaa_final_rate
        )
    )

    hof_pace <- hof_pace |> mutate(
        hof_in_progress_rate = case_when(
            age != curr_age ~ hof_in_progress_rate,
            pitcher == 0 ~ predict(hof_batter_model, newdata = (hof_pace |> select(cumsum_BOOG)), type="response"),
            pitcher == 1 & starter == 1 ~ predict(hof_sp_model, newdata = (hof_pace |> select(cumsum_BOOG)), type="response"),
            pitcher == 1 & starter == 0 ~ predict(hof_rp_model, newdata = (hof_pace |> select(cumsum_BOOG)), type="response"),
        ),
        bbwaa_in_progress_rate = case_when(
            age != curr_age ~ bbwaa_in_progress_rate,
            pitcher == 0 ~ predict(bbwaa_batter_model, newdata = (hof_pace |> select(cumsum_BOOG)), type="response"),
            pitcher == 1 & starter == 1 ~ predict(bbwaa_sp_model, newdata = (hof_pace |> select(cumsum_BOOG)), type="response"),
            pitcher == 1 & starter == 0 ~ predict(bbwaa_rp_model, newdata = (hof_pace |> select(cumsum_BOOG)), type="response"),
        ),
        hof_rate = case_when(
            age != curr_age ~ hof_rate,
            cumsum_BOOG == 0 ~ 0,
            hof_in_progress_rate > hof_final_rate ~ hof_in_progress_rate,
            .default = hof_final_rate
        ),
        bbwaa_rate = case_when(
            age != curr_age ~ bbwaa_rate,
            cumsum_BOOG == 0 ~ 0,
            bbwaa_in_progress_rate > bbwaa_final_rate ~ bbwaa_in_progress_rate,
            .default = bbwaa_final_rate
        )
    )
}

print("Writing output files")

boog_seasons <- boog_seasons |> select(!hof_in_progress_rate:bbwaa_final_pitcher_rate)
hof_pace <- hof_pace |> select(!pitcher) |> select(!starter) |> select(!hof_in_progress_rate:bbwaa_final_rate)


spreadsheet_entries <- boog_seasons |> 
    filter(hof | career_BOOG >= 10 | (pitcher == 1 & starter == 0 & career_BOOG >= 5) | (active == 1 & career_BOOG >= 0.05)) |>
    filter(n() > 1, .by = key_person) |>
    select(!hof:max_season) |> select(!cumsum_BOOG_start:career_BOOG) |> select(!season:starter)

spreadsheet_entries <- bind_rows(spreadsheet_entries, hof_pace |> filter(cumsum_BOOG > 0))

names_to_check <- (spreadsheet_entries |> distinct(key_person, name) |> count(name) |> filter(n > 1))$name

print("Please check names:")
print(names_to_check)

write_csv(spreadsheet_entries |> arrange(name, key_person, age), "output/boog-progression.csv", na = "")

active_boog_scores <- boog_seasons |> filter(active == 1) |> 
    arrange(season) |> 
    group_by(key_person) |>
    mutate(
        hof_rate_delta = hof_rate - replace_na(lag(hof_rate, default = 0), 0),
        bbwaa_rate_delta = bbwaa_rate - replace_na(lag(bbwaa_rate, default = 0), 0)
    ) |> 
    ungroup() |>
    filter(season == curr_season) |>
    select(!key_person) |> select(!season) |> select(!hof:cumsum_BOOG_relief) |>
    relocate(name) |> relocate(pitcher, .after = age) |> relocate(starter, .after = pitcher) |>
    arrange(desc(career_BOOG))

write_csv(active_boog_scores, "output/active-player-boogs.csv")

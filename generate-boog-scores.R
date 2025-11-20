#!/usr/bin/env Rscript

library(tidyverse)

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

pitching_data <- read_csv("data/pitcher-season-values.csv") |> 
    mutate(HBP = replace_na(HBP, 0), BF = if_else(is.na(TBF), 3 * round(IP) + H + BB + HBP, TBF)) |>
    select(League, Name, Season, Team, Age, PlayerId, MLBAMID, WAR, BF, IP) |>
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
    mutate(pitching = 1)
    

batting_data <- read_csv("data/batter-season-values.csv") |> 
    select(Name, Season, Team, Age, PlayerId, MLBAMID, WAA, PA) |>
    left_join(chadwick_names, by = join_by(PlayerId == key_fangraphs), na_matches = "never") |>
    left_join(chadwick_names, by = join_by(MLBAMID == key_mlbam), na_matches = "never") |>
    mutate(key_person = if_else(is.na(key_person.x), key_person.y, key_person.x)) |>
    mutate(name = if_else(is.na(key_person.x), if_else(is.na(key_person.y), Name, name.y), name.x)) |>
    rename(season = Season, team = Team, age = Age, fangraphs_id = PlayerId, mlbam_id = MLBAMID) |>
    select(!Name) |>
    select(!key_bbref.x:name.y) |>
    relocate(name) |>
    mutate(pitching = 0)

hall_of_famers <- read_csv("data/hall-of-famers.csv") |>
    left_join(chadwick_names, by = join_by(key_bref == key_bbref), na_matches = "never") |>
    mutate(hof = 1, key_person = replace_na(key_person, Name), bbwaa = if_else(BBWAA, 1, 0), .by = Name) |>
    select(key_person, hof, bbwaa)

combined_data <- bind_rows(pitching_data, batting_data) |> 
    filter(!is.na(age)) |>
    mutate(key_person = if_else(is.na(key_person), as.character(fangraphs_id), key_person)) |>
    select(name, season, age, WAA, BF, PA, key_person, pitching)
    

curr_season <- max(combined_data$season)

print("Combining data sets and summarizing BOOG scores..")

boog_seasons <- combined_data |> 
    mutate(
        batters_faced = if_else(pitching == 1, sum(BF), NA),
        plate_appearances = if_else(pitching == 0, sum(PA), NA),
        .by = c(key_person, pitching)
    ) |> group_by(key_person) |>
    fill(batters_faced, plate_appearances, .direction = "downup") |>
    ungroup() |>
    mutate(
        batters_faced = replace_na(batters_faced, 0),
        plate_appearances = replace_na(plate_appearances, 0),
        pitcher = if_else(batters_faced > plate_appearances, 1, 0)
    ) |> 
    mutate(season_BOOG = max(sum(WAA), 0), .by = c(key_person, season)) |>
    filter(sum(season_BOOG) > 0, .by = key_person) |>
    select(!WAA) |> select(!pitching:plate_appearances) |> select(!BF:PA) |>
    slice(1, .by = c(key_person, season)) |> 
    left_join(hall_of_famers, by = join_by(key_person)) |>
    left_join(active_players, by = join_by(key_person)) |>
    mutate(
        active = replace_na(active, 0), 
        hof = replace_na(hof, 0), 
        bbwaa = replace_na(bbwaa, 0)
    ) |> group_by(key_person) |>
    mutate(
        delta = min(season) - min(age), 
        max_season = if_else(active == 1, curr_season, max(season))
    ) |>
    complete(age = full_seq(age, 1)) |>
    arrange(age) |>
    fill(name, pitcher, delta, hof, bbwaa, max_season, active, .direction = "downup") |>
    mutate(season_BOOG = replace_na(season_BOOG, 0), season = if_else(is.na(season), age + delta, season)) |>
    select(!delta) |>
    arrange(season) |>
    mutate(career_to_date_BOOG = cumsum(season_BOOG), career_BOOG = sum(season_BOOG)) |>
    ungroup() |> mutate(hof_rate = 0.0, bbwaa_rate = 0.0)

print("Generating HOF data")

hof_projection_data <- boog_seasons |>
    group_by(pitcher) |>
    complete(key_person, age) |>
    group_by(key_person) |>
    arrange(age) |>
    fill(career_to_date_BOOG) |>
    fill(hof, bbwaa, .direction = "downup") |>
    ungroup() |>
    mutate(season_BOOG = replace_na(season_BOOG, 0), career_to_date_BOOG = replace_na(career_to_date_BOOG, 0))

median_hof <- hof_projection_data |> filter(hof == 1) |> 
    summarize(median_season_BOOG = median(season_BOOG), median_career_to_date_BOOG = median(career_to_date_BOOG), .by = c(age, pitcher)) |>
    filter(median_career_to_date_BOOG > 0) |>
    rename(season_BOOG = median_season_BOOG, career_to_date_BOOG = median_career_to_date_BOOG) |>
    mutate(name = if_else(pitcher == 1, "Median HOF Pitcher", "Median HOF Batter"), key_person = name)

median_bbwaa <- hof_projection_data |> filter(bbwaa == 1) |> 
    summarize(median_season_BOOG = median(season_BOOG), median_career_to_date_BOOG = median(career_to_date_BOOG), .by = c(age, pitcher)) |>
    filter(median_career_to_date_BOOG > 0) |>
    rename(season_BOOG = median_season_BOOG, career_to_date_BOOG = median_career_to_date_BOOG) |>
    mutate(name = if_else(pitcher == 1, "Median BBWAA Pitcher", "Median BBWAA Batter"), key_person = name)

hof_batter_ct <- nrow(boog_seasons |> filter(pitcher == 0 & hof == 1) |> distinct(key_person))
hof_pitcher_ct <- nrow(boog_seasons |> filter(pitcher == 1 & hof == 1) |> distinct(key_person))
bbwaa_batter_ct <- nrow(boog_seasons |> filter(pitcher == 0 & bbwaa == 1) |> distinct(key_person))
bbwaa_pitcher_ct <- nrow(boog_seasons |> filter(pitcher == 1 & bbwaa == 1) |> distinct(key_person))

min_player_ct <-  2 * max(c(hof_batter_ct, hof_pitcher_ct, bbwaa_batter_ct, bbwaa_pitcher_ct))

marginal_rows <- tibble(
    key_person = character(),
    name = character(),
    age = numeric(),
    career_to_date_BOOG = numeric(),
    pitcher = numeric()
)

for (player_age in (hof_projection_data |> distinct(pitcher, age) |> count(age) |> filter(n == 2))$age) {
    if (nrow(hof_projection_data |> filter(age == player_age)) >= min_player_ct) {
        marginal_rows <- marginal_rows |> add_row(
            key_person = "Marginal HOF Batter",
            name = "Marginal HOF Batter",
            age = player_age,
            career_to_date_BOOG = min((hof_projection_data |> filter(pitcher == 0 & age == player_age) |> slice_max(career_to_date_BOOG, n = hof_batter_ct))$career_to_date_BOOG),
            pitcher = 0
        ) |>
        add_row(
            key_person = "Marginal HOF Pitcher",
            name = "Marginal HOF Pitcher",
            age = player_age,
            career_to_date_BOOG = min((hof_projection_data |> filter(pitcher == 1 & age == player_age) |> slice_max(career_to_date_BOOG, n = hof_pitcher_ct))$career_to_date_BOOG),
            pitcher = 1
        ) |>
        add_row(
            key_person = "Marginal BBWAA Batter",
            name = "Marginal BBWAA Batter",
            age = player_age,
            career_to_date_BOOG = min((hof_projection_data |> filter(pitcher == 0 & age == player_age) |> slice_max(career_to_date_BOOG, n = bbwaa_batter_ct))$career_to_date_BOOG),
            pitcher = 0
        ) |>
        add_row(
            key_person = "Marginal BBWAA Pitcher",
            name = "Marginal BBWAA Pitcher",
            age = player_age,
            career_to_date_BOOG = min((hof_projection_data |> filter(pitcher == 1 & age == player_age) |> slice_max(career_to_date_BOOG, n = bbwaa_pitcher_ct))$career_to_date_BOOG),
            pitcher = 1
        )
    }
}

hof_pace <- bind_rows(median_hof, median_bbwaa, marginal_rows) |> mutate(hof_rate = 0.0, bbwaa_rate = 0.0)

print("Calculating logistic regressions...")

logit_young <- boog_seasons |> filter(career_to_date_BOOG > 0 & age <= 21 & season >= 1950 & max_season <= (curr_season - 15)) |> 
    select(pitcher, hof, bbwaa, career_to_date_BOOG)

logit_final <- boog_seasons |> filter(career_to_date_BOOG > 0 & season == max_season & season >= 1950 & max_season <= (curr_season - 15)) |>
    select(pitcher, hof, bbwaa, career_to_date_BOOG)

logit_young$pitcher <- as.factor(logit_young$pitcher)
logit_final$pitcher <- as.factor(logit_final$pitcher)

young_row_ct <- nrow(logit_young)
young_split_idx <- round(young_row_ct * 0.9)
young_rows <- sample(young_row_ct)
logit_young <- logit_young[young_rows, ]
train_young <- logit_young[1:young_split_idx, ]
test_young <- logit_young[(young_split_idx+1):nrow(logit_young), ]

final_row_ct <- nrow(logit_final)
final_split_idx <- round(final_row_ct * 0.9)
final_rows <- sample(final_row_ct)
logit_final <- logit_final[final_rows, ]
train_final <- logit_final[1:final_split_idx, ]
test_final <- logit_final[(final_split_idx + 1):nrow(logit_final), ]

hof_young_model <- glm(hof ~., family = binomial(link = "logit"), data = (train_young |> select(!bbwaa)))
bbwaa_young_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (train_young |> select(!hof)))
hof_final_model <- glm(hof ~., family = binomial(link = "logit"), data = (train_final |> select(!bbwaa)))
bbwaa_final_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (train_final |> select(!hof)))

boog_seasons <- boog_seasons |> mutate(
    hof_in_progress_rate = predict(hof_young_model, newdata = (boog_seasons |> select(pitcher, age, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    bbwaa_in_progress_rate = predict(bbwaa_young_model, newdata = (boog_seasons |> select(pitcher, age, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    hof_final_rate = predict(hof_final_model, newdata = (boog_seasons |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    bbwaa_final_rate = predict(bbwaa_final_model, newdata = (boog_seasons |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    hof_rate = case_when(
        age > 37 ~ hof_final_rate,
        age > 21 ~ hof_rate,
        career_to_date_BOOG == 0 ~ 0,
        (season == max_season & active == 0) ~ hof_final_rate,
        hof_in_progress_rate > hof_final_rate ~ hof_in_progress_rate,
        .default = hof_final_rate
    ),
    bbwaa_rate = case_when(
        age > 37 ~ bbwaa_final_rate,
        age > 21 ~ bbwaa_rate,
        career_to_date_BOOG == 0 ~ 0,
        (season == max_season & active == 0) ~ bbwaa_final_rate,
        bbwaa_in_progress_rate > bbwaa_final_rate ~ bbwaa_in_progress_rate,
        .default = bbwaa_final_rate
    )
)

hof_pace <- hof_pace |> mutate(
    hof_in_progress_rate = predict(hof_young_model, newdata = (hof_pace |> select(pitcher, age, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    bbwaa_in_progress_rate = predict(bbwaa_young_model, newdata = (hof_pace |> select(pitcher, age, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    hof_final_rate = predict(hof_final_model, newdata = (hof_pace |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    bbwaa_final_rate = predict(bbwaa_final_model, newdata = (hof_pace |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
    hof_rate = case_when(
        age > 37 ~ hof_final_rate,
        age > 21 ~ hof_rate,
        career_to_date_BOOG == 0 ~ 0,
        hof_in_progress_rate > hof_final_rate ~ hof_in_progress_rate,
        .default = hof_final_rate
    ),
    bbwaa_rate = case_when(
        age > 37 ~ bbwaa_final_rate,
        age > 21 ~ bbwaa_rate,
        career_to_date_BOOG == 0 ~ 0,
        bbwaa_in_progress_rate > bbwaa_final_rate ~ bbwaa_in_progress_rate,
        .default = bbwaa_final_rate
    )
)

for (curr_age in 22:37) {
    logit <- boog_seasons |> filter(career_to_date_BOOG > 0 & age == curr_age & season >= 1950 & max_season <= (curr_season - 15)) |>
    select(pitcher, hof, bbwaa, career_to_date_BOOG)

    logit$pitcher <- as.factor(logit$pitcher)

    row_ct <- nrow(logit)
    split_idx <- round(row_ct * 0.9)
    rows <- sample(row_ct)
    logit <- logit[rows , ]
    train <- logit[1:split_idx, ]
    test <- logit[(split_idx + 1):nrow(logit), ]

    hof_model <- glm(hof ~., family = binomial(link = "logit"), data = (train |> select(!bbwaa)))
    bbwaa_model <- glm(bbwaa ~., family = binomial(link = "logit"), data = (train |> select(!hof)))

    boog_seasons <- boog_seasons |> mutate(
        hof_in_progress_rate = predict(hof_model, newdata = (boog_seasons |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
        bbwaa_in_progress_rate = predict(bbwaa_model, newdata = (boog_seasons |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
        hof_rate = case_when(
            age != curr_age ~ hof_rate,
            career_to_date_BOOG == 0 ~ 0,
            (season == max_season & active == 0) ~ hof_final_rate,
            hof_in_progress_rate > hof_final_rate ~ hof_in_progress_rate,
            .default = hof_final_rate
        ),
        bbwaa_rate = case_when(
            age != curr_age ~ bbwaa_rate,
            career_to_date_BOOG == 0 ~ 0,
            (season == max_season & active == 0) ~ bbwaa_final_rate,
            bbwaa_in_progress_rate > bbwaa_final_rate ~ bbwaa_in_progress_rate,
            .default = bbwaa_final_rate
        )
    )

    hof_pace <- hof_pace |> mutate(
        hof_in_progress_rate = predict(hof_model, newdata = (hof_pace |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
        bbwaa_in_progress_rate = predict(bbwaa_model, newdata = (hof_pace |> select(pitcher, career_to_date_BOOG) |> mutate(pitcher = relevel(as.factor(pitcher), 1, 0))), type="response"),
        hof_rate = case_when(
            age != curr_age ~ hof_rate,
            career_to_date_BOOG == 0 ~ 0,
            hof_in_progress_rate > hof_final_rate ~ hof_in_progress_rate,
            .default = hof_final_rate
        ),
        bbwaa_rate = case_when(
            age != curr_age ~ bbwaa_rate,
            career_to_date_BOOG == 0 ~ 0,
            bbwaa_in_progress_rate > bbwaa_final_rate ~ bbwaa_in_progress_rate,
            .default = bbwaa_final_rate
        )
    )
}

print("Writing output files")

hof_pace <- hof_pace |> select(!pitcher) |> select(!hof_in_progress_rate:bbwaa_final_rate)


spreadsheet_entries <- boog_seasons |> 
    filter(hof | career_BOOG >= 10) |> 
    select(!hof:max_season) |> select(!career_BOOG) |> select(!season) |> select(!pitcher) |> select(!hof_in_progress_rate:bbwaa_final_rate)

spreadsheet_entries <- bind_rows(spreadsheet_entries, hof_pace |> filter(career_to_date_BOOG > 0))

names_to_check <- (spreadsheet_entries |> distinct(key_person, name) |> count(name) |> filter(n > 1))$name

print("Please check names:")
print(names_to_check)

write_csv(spreadsheet_entries |> arrange(name, key_person, age), "output/boog-progression.csv", na = "")

active_boog_scores <- boog_seasons |> filter(active == 1) |> 
    arrange(season) |> 
    group_by(key_person) |>
    mutate(
        hof_rate_delta = hof_rate - replace_na(lag(hof_rate, default = 0), 0),
        bbwaa_rate_delta = bbwaa_rate - replace_na(lag(bbwaa_rate, default = 0), 0),
        hof_compiling = (hof_rate == hof_final_rate),
        bbwaa_compiling = (bbwaa_rate == bbwaa_final_rate)
    ) |> 
    ungroup() |>
    filter(season == curr_season) |>
    select(!key_person) |> select(!season) |> select(!hof:career_to_date_BOOG) |> select(!hof_in_progress_rate:bbwaa_final_rate) |>
    relocate(name) |> relocate(pitcher, .after = age)

write_csv(active_boog_scores, "output/active-player-boogs.csv")

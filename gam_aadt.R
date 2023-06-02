{
  library(tidyverse)
  library(broom)
  library(sf)
  library(sfnetworks)
  library(igraph)
  #library(tidygraph)
  library(mgcv)
  library(gratia)
}

# To get fresh AADT
#source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")


# Read directed links ----
# From Kibana CSVs
tl_files <- list.files(pattern = "directed_tl*")

directed_links <-
  purrr::map(
    tl_files,
    ~ readr::read_csv2(file = .x, col_types = readr::cols(location.county_ids = "c"))
  ) |>
  purrr::list_rbind() |>
  dplyr::select(
    id = '_id',
    #functional_class_high = functional_road_class_info.highest,
    functional_class_low = functional_road_class_info.lowest,
    with_metering = is_traffic_with_metering_direction,
    length,
    county_ids = location.county_ids,
    #ferry = is_ferry_traffic_link,
    #directions = lanes_and_directions_info.direction_types,
    road_category,
    urban_ratio,
    #lanes_max = lane_info.max_num_lanes,
    lanes_min = lane_info.min_num_lanes,
    trp_id = primary_trp,
    speed_high = speed_limit_info.highest,
    speed_low = speed_limit_info.lowest,
    #blocked,
    #public_transport_only = lane_info.has_only_public_transport_lanes,
    #is_invalid,
    #invalid_reason
  )

directed_links_tidy <-
  directed_links |>
  dplyr::mutate(
    trp_id = stringr::str_replace(trp_id, "^-$", NA_character_),
    # Because of weirdness from Kibana:
    dplyr::across(
      #.cols = c(lanes_max, lanes_min, speed_high, speed_low),
      .cols = c(lanes_min, speed_high, speed_low),
      .fns = ~ stringr::str_replace(.x, ",0", "") |> as.numeric()
    ),
    county_ids = stringr::str_replace_all(county_ids, ",0", ""),
    log_length_km = base::log(1 + length / 1e3)
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(functional_class_low, lanes_min, speed_high),
      .fns = ~ base::as.factor(.)
    ),
    #year = stats::relevel(year, "2022"),
    lanes_min = stats::relevel(lanes_min, "2"),
    speed_high = stats::relevel(speed_high, "80"),
    functional_class_low =
      forcats::fct_recode(
        functional_class_low,
        "6" = "7",
        "6" = "8",
        "6" = "9"
      ),
    speed_high = forcats::fct_recode(speed_high, "30" = "20")
  ) |>
  tibble::rowid_to_column("row_id") |>
  dplyr::filter(
    road_category %in% c("EUROPAVEG", "RIKSVEG", "FYLKESVEG")
  ) |>
  dplyr::mutate(
    functional_class_low =
      dplyr::case_when(
        functional_class_low %in% c("4", "5", "6") & road_category %in% c("EUROPAVEG", "RIKSVEG") ~ "3",
        TRUE ~ functional_class_low
      ) |> forcats::as_factor() |>
      stats::relevel("3")
  )

remove(directed_links)

directed_links_combinations <-
  directed_links_tidy |>
  dplyr::filter(
    dplyr::if_all(
      .cols = c(functional_class_low, lanes_min, speed_high),
      .fns = ~ !base::is.na(.)
    )
  ) |>
  dplyr::group_by(
    lanes_min,
    functional_class_low,
    speed_high
  ) |>
  dplyr::summarise(
    n = n(),
    .groups = "drop"
  )

# AADT ----
# trps_on_links <-
#   base::unique(directed_links_tidy$trp_id) |>
#   purrr::discard(is.na)
#
# aadts_1 <-
#   get_aadt_by_direction_for_trp_list(trps_on_links[1:10]) |>
#  dplyr::filter(!is.na(adt))
#
# aadt_all <-
#   dplyr::bind_rows(
#     aadts,
#     aadts_2,
#     aadts_3,
#     aadts_4,
#     aadts_5
#   ) |>
#   dplyr::distinct()
#
# readr::write_rds(
#  aadt_all,
#  file = "aadt_all.rds"
# )

aadt_heading <-
  readr::read_rds(
    file = "aadt_heading.rds"
  )


# Links and AADT ----
# All links with latest AADT
latest_aadt_and_link <-
  aadt_heading |>
  tibble::as_tibble() |>
  dplyr::slice_max(year) |>
  dplyr::select(
    trp_id,
    year,
    with_metering,
    adt,
    se_mean
  ) |>
  dplyr::right_join(
    directed_links_tidy,
    by = c("trp_id", "with_metering")
  ) |>
  dplyr::mutate(
    year = year |> forcats::as_factor() |> stats::relevel("2022"),
    adt =
      dplyr::case_when(
        adt == 0 ~ NA_integer_,
        TRUE ~ adt
      )
  )

# How representative are the links with AADT?
aadt_and_link_combinations <-
  latest_aadt_and_link |>
  dplyr::filter(
    dplyr::if_all(
      .cols = c(functional_class_low, lanes_min, speed_high, adt),
      .fns = ~ !base::is.na(.)
    )
  ) |>
  dplyr::group_by(
    lanes_min,
    functional_class_low,
    speed_high
  ) |>
  dplyr::summarise(
    n_with_aadt = n(),
    .groups = "drop"
  )

compare_link_and_aadt_distribution <-
  directed_links_combinations |>
  dplyr::left_join(
    aadt_and_link_combinations,
    by = dplyr::join_by(functional_class_low, lanes_min, speed_high)
  ) |>
  dplyr::mutate(
    n_with_aadt =
      dplyr::case_when(
        is.na(n_with_aadt) ~ 0,
        TRUE ~ n_with_aadt
      ),
    n_with_aadt_ratio = (n_with_aadt / (n_with_aadt + n)) |> round(2)
  )


# Split training and validation set ----
# Need to combine all AADT with links first to check if
# validation set will be a representative sample of the model variables.
# Representative in comparison to:
# - all links? Yes, but without depleting training set.
# - links in training set? NO, because training set will be expanded.

# Picking some AADTs for 2022 for later to compare prediction
aadt_heading_for_validation <-
  aadt_heading |>
  dplyr::filter(
    year == 2022,
    se_mean == 0,
    adt > 100
  )








chosen_trps <-
  aadt_heading_for_validation |>
  dplyr::select(
    trp_id
  ) |>
  dplyr::distinct() |>
  dplyr::slice_sample(n = 500)

aadt_heading_for_validation_chosen <-
  aadt_heading_for_validation |>
  dplyr::filter(
    trp_id %in% chosen_trps$trp_id
  )

# DO NOT OVERWRITE
# readr::write_rds(
#   aadt_heading_for_validation_chosen,
#   "aadt_test_set.rds"
# )

aadt_heading_for_validation_chosen <-
  readr::read_rds(
    "aadt_test_set.rds"
  )





# Removing test set from training set
aadt_heading_training <-
  aadt_heading |>
  dplyr::filter(
    !(trp_id %in% aadt_heading_for_validation_chosen$trp_id)
  )


aadt_and_link <-
  aadt_heading_training |>
  tibble::as_tibble() |>
  dplyr::filter(
    year > 2010
  ) |>
  dplyr::select(
    trp_id,
    year,
    with_metering,
    adt,
    se_mean
  ) |>
  dplyr::right_join(
    directed_links_tidy,
    by = c("trp_id", "with_metering")
  ) |>
  dplyr::mutate(
    year = year |> forcats::as_factor() |> stats::relevel("2022"),
    adt =
      dplyr::case_when(
        adt == 0 ~ NA_integer_,
        TRUE ~ adt
      )
  )

summary(aadt_and_link$functional_class_low)
summary(aadt_and_link$speed_high)
table(aadt_and_link$road_category, aadt_and_link$functional_class_low)
summary(aadt_and_link$log_length_km)
summary(aadt_and_link$adt)

# Compare links in training and test set
aadt_and_link_training <-
  aadt_and_link |>
  dplyr::filter(
    !is.na(adt)
  ) |>
  dplyr::slice_max(year)


aadt_and_link_test <-
  aadt_heading_for_validation_chosen |>
  tibble::as_tibble() |>
  dplyr::select(
    trp_id,
    year,
    with_metering,
    adt,
    se_mean
  ) |>
  dplyr::left_join(
    directed_links_tidy,
    by = c("trp_id", "with_metering")
  ) |>
  dplyr::mutate(
    year = year |> forcats::as_factor() |> stats::relevel("2022"),
    adt =
      dplyr::case_when(
        adt == 0 ~ NA_integer_,
        TRUE ~ adt
      )
  )

comparison_lanes <-
  dplyr::bind_rows(
    summary(aadt_and_link_training$lanes_min),
    summary(aadt_and_link_test$lanes_min)
  )


# Modelling ----
simpel_model <-
  mgcv::gam(
    adt ~ 1 + year + lanes_min + functional_class_low + speed_high + log_length_km * urban_ratio,
    family = Gamma(link = "log"),
    data = aadt_and_link,
    method = "REML"
  )

# TODO: why no more smooth terms? They can only be used for numerical variables!
# For s-functions:
# Determine number of basis functions: k = ?
# Determine smoothing parameter: s = ?
# Add categorical variables as "by" in s-functions?

# To compare models that are based on the same data set, use:
# AIC
# GCV/UBRE
# R adj

# simpel_model_alt_1 <-
#   mgcv::gam(
#     adt ~ 1 + year + lanes_min + functional_class_low + speed_high + log_length_km * urban_ratio,
#     family = Gamma(link = "log"),
#     data = aadt_and_link,
#     method = "REML"
#   )

## Checking results ----
simpel_model_results <-
  dplyr::bind_rows(
    broom::glance(simpel_model),
    #broom::glance(simpel_model_alt_1)
  )

plot(
  simpel_model,
  pages = 1,
  all.terms = TRUE,
  shade = TRUE,
  residuals = TRUE,
  seWithMean = TRUE,
  shift = stats::coef(simpel_model)[1]
)

gratia::appraise(simpel_model)

mgcv::summary.gam(simpel_model)
mgcv::gam.check(simpel_model)
# Model must have convergence!
# EDF should not be close to k'.
# QQ-plot should be close to straight line.
# Histogram should have bell shape.
# Residuals should be evenly spread around zero.
# Response plot should cluster around the 1-to-1 line.


# Look at variables with smoother functions:
gratia::draw(simpel_model, residuals = TRUE)


# Predicting ----
# The data frame with all links, to hold the predicted aadt
directed_links_predicted_aadt <-
  directed_links_tidy |>
  dplyr::mutate(
    year = "2022"
  )

# Predict
directed_links_predicted_aadt$predicted_aadt <-
  mgcv::predict.gam(
    simpel_model,
    directed_links_predicted_aadt,
    type = "response"
  ) |>
  base::floor()

predicted_na <-
  directed_links_predicted_aadt |>
  dplyr::filter(
    is.na(predicted_aadt)
  )

# TODO: Predict variance from predicted AADT
# See if true AADT is within CI of predicted AADT

# Predict for all links, but keep the true AADT and their true SE instead of the predicted.(?)


# Choosing some of the links without TRP
links_to_predict <-
  links_with_relevant_data |>
  dplyr::filter(
    is.na(trp_id)
  ) |>
  dplyr::filter(
    dplyr::if_all(
      .cols = c(functional_class_low, lanes_min, speed_high),
      .fns = ~ !base::is.na(.)
    )
  ) |>
  dplyr::slice(1:100) |>
  dplyr::mutate(
    year = as.factor("2022")
  )


# Comparing predictions to test set ----
# Two different strategies:
# 1. Leaving som TRP-AADT out of the modeling, and predicting them.
# 2. Comparing predicted AADT on non-TRP links with manual AADT.

# 1
aadt_compared <-
  aadt_heading_for_validation_chosen |>
  dplyr::select(
    trp_id,
    with_metering,
    adt_true = adt
  ) |>
  dplyr::left_join(
    directed_links_predicted_aadt,
    by = dplyr::join_by(trp_id, with_metering)
  ) |>
  dplyr::select(
    id,
    trp_id,
    with_metering,
    county_ids,
    road_category,
    lanes_min,
    functional_class_low,
    speed_high,
    log_length_km,
    urban_ratio,
    adt_true,
    predicted_aadt
  ) |>
  dplyr::mutate(
    diff = predicted_aadt - adt_true
  )

sum_absolute_diff <- sum(aadt_compared$diff)

{
  library(tidyverse)
  #library(jsonlite)
  library(sf)
  library(sfnetworks)
  library(igraph)
  #library(tidygraph)
  library(mgcv)
  library(gratia)
}

source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")

# Read ----
# Geojson files from ADM
links <- sf::st_read("traffic-links-2022.geojson")
nodes <- sf::st_read("traffic-link-nodes-2021.geojson")

# CSV from Kibana
links_info <-
  readr::read_csv2("links_info.csv") |>
  dplyr::select(
    nvdb_id,
    functional_class_high = functional_road_class_info.highest,
    functional_class_low = functional_road_class_info.lowest,
    ferry = is_ferry_traffic_link,
    directions = lanes_and_directions_info.direction_types,
    road_category = location.road_category,
    urban_ratio,
    lanes_max = lanes_and_directions_info.max_num_lanes,
    lanes_min = lanes_and_directions_info.min_num_lanes,
    trp = primary_trp,
    speed_high = speed_limit_info.highest,
    speed_low = speed_limit_info.lowest,
    is_degenerate,
    blocked,
    public_transport_only = lanes_and_directions_info.has_only_public_transport_lanes
  ) |>
  dplyr::mutate(
    trp_id = stringr::str_replace(trp, "^-$", NA_character_),
    # Because of weirdness from Kibana:
    dplyr::across(
      .cols = c(directions, lanes_max, lanes_min, speed_high, speed_low),
      .fns = ~ stringr::str_replace(.x, ",0", "") |> as.numeric()
    )
  )

links_tidy <-
  links |>
  dplyr::select(
    nvdb_id = nvdbId,
    length,
    start_node = startPositionAccordingToMetering,
    end_node = endPositionAccordingToMetering
  ) |>
  dplyr::left_join(
    links_info,
    by = "nvdb_id"
  ) |>
  dplyr::mutate(
    log_length_km = base::log(1 + length / 1e3)
  ) |>
  tibble::as_tibble()

# links_subset <-
#   links |>
#   slice(1:40) |>
#   dplyr::select(
#     nvdb_id = nvdbId,
#     location,
#     length,
#     trp = primaryTrp,
#     start_node = startNodeAccordingToMetering,
#     end_node = endNodeAccordingToMetering
#   ) |>
#   dplyr::mutate(
#     location_json = purrr::map(location, ~ jsonlite::fromJSON(.) |> tibble::as_tibble())
#   ) |>
#   tidyr::unnest(cols = "location_json")
#
# links_trp <-
#   links_subset |>
#   sf::st_drop_geometry() |>
#   dplyr::select(
#     nvdb_id,
#     trp
#   ) |>
#   dplyr::filter(
#     !is.na(trp)
#   ) |>
#   dplyr::mutate(
#     json = purrr::map(trp, ~ jsonlite::fromJSON(.) |> tibble::as_tibble())
#   ) |>
#   tidyr::unnest(cols = "json") |>
#   dplyr::select(
#     nvdb_id,
#     trp_id = id
#   )


#
# Links in chosen area ----
# smola <-
#   hent_kommune_v3(1573) |>
#   sf::st_transform("wgs84")

hitra <-
  hent_kommune_v3(5056) |>
  sf::st_transform("wgs84")

links_chosen <-
  links_tidy[sf::st_intersects(links_tidy, hitra$polygon) %>% lengths > 0,]

links_chosen |>
  ggplot() +
  geom_sf(aes(col = length))

# Smøla
# Repair links without nodes
# union_hopen_veiholmen <-
#   links_chosen |>
#   dplyr::filter(
#     nvdb_id %in% c("1014386164", "1014386165")
#   ) |>
#   dplyr::summarise(
#     nvdb_id = max(nvdb_id),
#     length = sum(length),
#     start_node = min(start_node, na.rm = TRUE),
#     end_node = min(end_node, na.rm = TRUE),
#     functional_class_high = max(functional_class_high),
#     functional_class_low = min(functional_class_low),
#     ferry = any(ferry),
#     directions = min(directions),
#     road_category = min(road_category),
#     urban_ratio = mean(urban_ratio),
#     lanes_max = max(lanes_max),
#     lanes_min = min(lanes_min),
#     trp = max(trp),
#     speed_high = max(speed_high),
#     speed_low = min(speed_low)
#   )

links_chosen_tidy <-
  links_chosen |>
  # dplyr::filter(
  #   !is.na(start_node),
  #   !is.na(end_node)
  # ) |>
  #dplyr::bind_rows(
  #  union_hopen_veiholmen
  #) |>
  dplyr::mutate(
    with_metering = 1,
    link_id = dplyr::row_number()
  ) |>
  dplyr::relocate(
    link_id
  ) |>
  # Before building graph, all multilines should be merged to lines
  # But there are too many weird links that stay multiline
  # TODO: use most recent links (hopefully fewer errors)
  sf::st_line_merge() |>
  sf::st_cast("LINESTRING") |>
  # Removing problematic links
  dplyr::filter(
    !(nvdb_id %in% c(
      "1014584167",
      "1014584152",
      "1014443825",
      "1014444138",
      "1014444137",
      "1014584106",
      "1014444324",
      "1014444142"
    ))
  )

links_chosen_narrow <-
  links_chosen_tidy |>
  dplyr::select(
    nvdb_id
  )


#
# Nodes ----
node_ids <-
  c(
    links_chosen_tidy$start_node,
    links_chosen_tidy$end_node
  ) |>
  base::unique() |>
  base::sort()

# Reset node_ids in link subset
node_id_reset <-
  tibble::tibble(
    id = node_ids
  ) |>
  dplyr::mutate(
    node_id = dplyr::row_number()
  )

nodes_chosen <-
  nodes |>
  dplyr::filter(
    id %in% node_ids
  ) |>
  dplyr::left_join(
    node_id_reset,
    by = "id"
  ) |>
  dplyr::arrange(node_id) |>
  dplyr::relocate(node_id)

# TODO: many missing nodes!

## Plot ----
links_chosen_tidy |>
  ggplot() +
  geom_sf(aes(col = link_id)) +
  geom_sf(data = nodes_chosen)


# TRP heading ----
trp_direction <-
  get_trps_with_direction() %>%
  dplyr::select(
    trp_id,
    from_according_to_metering,
    to_according_to_metering
  ) %>%
  dplyr::distinct()

trp_with_metering <-
  trp_direction %>%
  dplyr::select(
    trp_id,
    heading = to_according_to_metering
  ) %>%
  dplyr::mutate(
    with_metering = 1
  )

trp_against_metering <-
  trp_direction %>%
  dplyr::select(
    trp_id,
    heading = from_according_to_metering
  ) %>%
  dplyr::mutate(
    with_metering = 0
  )

trp_heading <-
  dplyr::bind_rows(
    trp_with_metering,
    trp_against_metering
  ) %>%
  dplyr::arrange(
    trp_id
  )


# AADT ----
trps <-
  links_chosen_tidy$trp |>
  purrr::discard(is.na)

aadt <-
  get_aadt_by_direction_for_trp_list(trps)

aadt_heading <-
  aadt |>
  dplyr::left_join(
    trp_heading,
    by = c("trp_id", "heading")
  ) |>
  dplyr::select(
    trp_id,
    with_metering,
    year,
    adt,
    se_mean
  )

#Directed links ----
links_one_way <-
  links_chosen_tidy_subset |>
  dplyr::filter(
    directions != 3
  )

links_two_way <-
  links_chosen_tidy_subset |>
  dplyr::filter(
    directions == 3
  )

links_two_way_against <-
  links_two_way |>
  dplyr::mutate(
    with_metering = 0,
    start_node = links_two_way$end_node,
    end_node = links_two_way$start_node
  )

# TODO: Handle one way links with direction against metering, i.e. directions == 2


links_directed <-
  dplyr::bind_rows(
    links_one_way,
    links_two_way,
    links_two_way_against
  ) |>
  dplyr::arrange(
    link_id
  ) |>
  dplyr::mutate(
    length_km = length / 1e3,
    length_normalized = length_km / base::max(length_km),
    speed = dplyr::case_when(
      ferry == TRUE ~ 10,
      TRUE ~ speed_high
    ),
    travel_time_minutes = length_km / speed * 60,
    travel_time_normalized = travel_time_minutes / base::max(travel_time_minutes)
    #from = as.numeric(start_node),
    #to = as.numeric(end_node)
  ) |>
  dplyr::left_join(
    node_id_reset,
    by = c("start_node" = "id")
  )  |>
  dplyr::left_join(
    node_id_reset,
    by = c("end_node" = "id"),
    suffix = c("", "_end")
  ) |>
  dplyr::select(
    -length,
    -trp,
    -start_node,
    -end_node
  ) |>
  dplyr::rename(
    from = node_id,
    to = node_id_end
  ) |>
  dplyr::relocate(
    from, to
  )

# Variables used further:
# ferry_speed_10 = speed,
# pred_lane = lanes_min,

links_directed_nodes <-
  links_directed |>
  sf::st_drop_geometry() |>
  dplyr::select(
    from,
    to
  )

## AADT per directed link ----
links_directed_id_aadt <-
  links_directed |>
  sf::st_drop_geometry() |>
  dplyr::select(
    link_id,
    with_metering,
    trp_id
  ) |>
  dplyr::left_join(
    aadt_heading,
    by = c("trp_id", "with_metering")
  )



# Directed graph ----

# Build directed graph object from nodes and spatially implicit edges
# Implicit because there are gaps (roundabouts) which will give:
# Error: Edge boundaries do not match their corresponding nodes
# Therefore: edges_as_lines = FALSE,

# The edges must contain columns 'from' and 'to'

directed_graph <-
  sfnetworks::sfnetwork(
    nodes = nodes_chosen,
    edges = links_directed,
    directed = TRUE,
    node_key = "node_id",
    edges_as_lines = FALSE,
    force = FALSE
  )

plot(directed_graph)

ggplot2::autoplot(directed_graph)

# Verify that directed graph contains one single component
directed_graph |> igraph::count_components(mode="weak")

cc <- directed_graph |> igraph::components(mode="weak")

rev(table(cc$csize))

## Compute edge betweenness centrality ----
directed_graph_bc <-
  directed_graph %>%
  tidygraph::activate("edges") |>
  dplyr::mutate(
    bc_travel_time =
      tidygraph::centrality_edge_betweenness(
        weights = travel_time_minutes,
        directed = TRUE,
        cutoff = NULL)
    )

# Extract link metainfo, now including BC
# TODO: And reattaching geometry
edges_main <-
  directed_graph_bc |>
  tidygraph::activate("edges") |>
  tibble::as_tibble()




# graph_test <-
#   tidygraph::tbl_graph(
#     nodes = nodes_chosen,
#     node_key = "node_id",
#     edges = links_directed_nodes
#   )
#
# test <- igraph::graph(c(links_directed_nodes$from, links_directed_nodes$to))




# GAM on larger area ----
## Link info and AADT ----
links_with_relevant_data <-
  links_tidy |>
  sf::st_drop_geometry() |>
  dplyr::select(
    nvdb_id,
    functional_class_low,
    lanes_min,
    speed_high,
    log_length_km,
    urban_ratio,
    public_transport_only,
    ferry,
    trp_id
  )

trps_on_links <-
  base::unique(links_with_relevant_data$trp_id) |>
  purrr::discard(is.na)

#aadts <-
#  get_aadt_for_trp_list(trps_on_links) |>
#  dplyr::filter(!is.na(adt))

#readr::write_rds(
#  aadts,
#  file = "aadts_test.rds"
#)

aadts <-
  readr::read_rds(
    file = "aadts_test.rds"
  )


aadt_and_link <-
  aadts |>
  tibble::as_tibble() |>
  dplyr::filter(
    year > 2010
  ) |>
  dplyr::select(
    trp_id,
    year,
    adt
  ) |>
  dplyr::left_join(
    links_with_relevant_data,
    by = "trp_id"
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(year, functional_class_low, lanes_min, speed_high),
      .fns = ~ base::as.factor(.)
    ),
    year = stats::relevel(year, "2022"),
    lanes_min = stats::relevel(lanes_min, "2")
  ) |>
  tibble::rowid_to_column("id")

# Removing some AADTs for 2022 for later to compare prediction
aadt_to_compare <-
  aadt_and_link |>
  dplyr::filter(
    year == "2022"
  ) |>
  dplyr::slice_sample(n = 15)

aadt_and_link_for_modelling <-
  aadt_and_link |>
  dplyr::filter(
    !(id %in% aadt_to_compare$id)
  )


## Modelling ----
simpel_model <-
  mgcv::gam(
    adt ~ 1 + year + lanes_min + functional_class_low + speed_high + s(log_length_km, k = 8),
    family = Gamma(link = "log"),
    data = aadt_and_link_for_modelling,
    method = "REML"
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

# Look at variables with smoother functions:
gratia::draw(simpel_model, residuals = TRUE)


## Predicting ----
# Predict the ones left out
aadt_to_compare$predicted_aadt <-
  mgcv::predict.gam(
    simpel_model,
    aadt_to_compare,
    type = "response"
  ) |>
  base::floor()

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


## Balancing ----


## Comparing ----
# Two different strategies:
# 1. Leaving som TRP-AADT out of the modeling, and predicting them.
# 2. Comparing predicted AADT on non-TRP links with manual AADT.

# 1
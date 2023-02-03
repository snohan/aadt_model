# Create a geopackage file containing directed links with AADT

# NRs framgangsmåte ----
# Lag vegnettsgraf, steg 1 i ÅDT-modulen
#
# N1 Først isoleres hovedkomponenten i vegnettet
#    gjennom en nettverksanalyse på
#    samlingen av trafikklenker og kryss (NR-kode).
#
# N2 Deretter retningsdekomponeres lenkene i hovedkomponenten (TØI-kode).
#    Dette gir en sammenhengende vegnettsgraf med retningsoppløsning.
#
# N3 Fra vegnettsgrafen beregnes sentralitetsmålet betweenness centrality
#    til bruk i den statistiske modellen (NR-kode).
#    Sentralitetsverdier tilordnes hver enkelt lenke i den rettede grafen.
#
# N4 Preliminær ÅDT fra tilgjengelige trafikkregistreringspunkter
#    (kontinuerlige og periodiske) kobles på sine respektive rettede lenker
#    der hvor slike punkter finnes (Knowit-kode).
#
# N5 For å få med nettutlagt ÅDT fra transportmodellkjøringen for 2018,
#    må det dessuten opprettes en kobling mellom den nye vegnettsgrafen
#    og lenkene i vegnettsgrafen fra det inneværende prosjektet (ny kode).
#    Senere versjoner kan benytte nettutlagt ÅDT fra denne første koblingen.
#
# N6 Den nye vegnettsgrafen med tilhørende attributter sammenstilles
#    til en GeoPackagefil.


# Problemer Norge ----
#
# P1 TØI laget en liste med lenker som skulle fjernes.
#    ID-ene er endret, så disse kan ikke
#    fjernes slik de er angitt nå. Er dette fortsatt nødvendig?
#    Finnes det andre lenker som burde plukkes vekk?
#
# P2 Grafen for hele Norge er veldig usammenhengende med sine over
#    2 000 komponenter.
#    Vi burde sjekke alle løse vegnettsdeler -
#    skulle de vært en del av hovedkomponenten?
#    Det er tre komponenter som må kobles på.
#
# P3 Transportmodell-ÅDT er bare koblet på for Region Vest tidligere.
#    Nå skulle vi gjort det for hele landet.


# Tilpasset framgangsmåte Region vest ----
#
# 1. Bruk Trionas gpkg-fil for hele Norge og isoler hovedkomponenten.
#    Hekt på deler som skal være med.
#
#      Kode hentet fra:
#      prepros_generer_urettet_hovedkomponent.R
#
# 2. Begrens området til fylkene Rogaland og Vestland.
#
#      Kode hentet fra:
#      2_datasett_region_vest.R
#
# 3. Lag rettet graf.
#
#      Kode hentet fra:
#      3_rettede_lenker_metrering.R
#      4_rettede_lenker_trafikk.R
#
# 4. Koble på ÅDT fra transportmodell fra graf_dir_maincom_20210505.gpkg.
#
# 5. Koble på ÅDT fra Trafikkdata-API.
#
# 6. Beregn sentralitetsparametere.
#
#      Kode hentet fra:
#      prepros_grafanalyse_vegnett_rettet.r
#
# 7. Skriv til ny GPKG-fil.


# Setup ----
library(tidyverse)
library(sf)
library(sfnetworks)
library(igraph)
library(tidygraph)
library(ggplot2)
library(DBI)
library(RSQLite)
library(tmap)
library(tictoc)

source("nr_prep_links/nr/prepros_manipuler_trafikklenker_hardkodet.r")

## The Triona file
geopackage_file <-
  #'C:/Users/snohan/Desktop/Trafikklenker_20220116.gpkg'
  'C:/Users/snohan/Desktop/Trafikklenker_20220315.gpkg'

# Trafikklenker som skal fjernes iht til TØIs kartlegging
file_remove_links_toi <-
  'nr_prep_links/nr/Data/ID_trafikklenker_fjernes.csv'


# 1. Undirected road net Norway ----
## Read ----
#geopackage_layers <- sf::st_layers(geopackage_file)

edges <-
  sf::st_read(
    geopackage_file,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"TrafikkLenker\""
  ) %>%
  sf::st_zm()

nodes <-
  sf::st_read(
    geopackage_file,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"Kryss\""
  ) %>%
  sf::st_zm()

road_net_info <-
  sf::st_read(
    geopackage_file,
    layer = 'Trafikklenker_stedfesting'
  )

# edges_without_geometry <-
#   edges %>%
#   sf::st_drop_geometry()


## Clean ----
## Targeted data cleansing of individual traffic links and nodes
links_remove_toi <-
  read.csv(
    file_remove_links_toi,
    stringsAsFactors = FALSE
  )

# The link IDs have changed in the original Triona file,
# so this won't have any effect
# TODO: Are these links still a problem?
# TODO: Are there others we should remove?
# TODO: Remove problematic links automatically?

# Removing cyclic paths, uncoupled nodes, problematic paths
# Connecting uncoupled graph parts that should be part of main graph
data_cleansed <-
  manipuler_trafikklenker_hardkodet(
    nn = nodes,
    ee = edges,
    links_remove = links_remove_toi
  )

nodes <- data_cleansed$nodes
edges <- data_cleansed$edges


## Build undirected graph ----
# Add indices for start and end nodes of each edge wrt nodes object
edges$from <-
  base::match(
    edges$START_NODE_OID,
    nodes$FEATURE_OID
  )

edges$to <-
  base::match(
    edges$END_NODE_OID,
    nodes$FEATURE_OID
  )

# Build undirected graph object from nodes and spatially implicit edges
net_undir <-
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = edges,
    directed = FALSE,
    node_key = "FEATURE_OID",
    edges_as_lines = FALSE,
    force = FALSE
  )


## Graph components ----
# Decompose graph and consider the distribution of nodes
# among the various components

# Identify connected components
net_undir %>%
  igraph::count_components(mode="weak")

cc <-
  net_undir %>%
  igraph::components(mode="weak")

# Look at component size distribution
rev(table(cc$csize))

# Isolate main vs and all other
net_main <-
  igraph::induced_subgraph(
    net_undir,
    igraph::V(net_undir)[components(net_undir)$membership ==
                              which.max(components(net_undir)$csize)]) %>%
  sfnetworks::as_sfnetwork()

net_all_but_main <-
  igraph::induced_subgraph(
    net_undir,
    igraph::V(net_undir)[components(net_undir)$membership !=
                              which.max(components(net_undir)$csize)]) %>%
  sfnetworks::as_sfnetwork()


## Add geometry ----
# Extract nodes and edges from components, and add geometries.

### Main component ----
nodes_main <- net_main %>%
  sfnetworks::activate("nodes") %>%
  st_as_sf()

edges_main <- net_main %>%
  sfnetworks::activate("edges") %>%
  as_tibble()

# Transfer geometries from original edges based on matching IDs
idx <-
  base::match(
    as.character(edges_main$ID),
    as.character(edges$ID)
  )

edges_main <-
  sf::st_set_geometry(
    edges_main,
    edges$GEOMETRY[idx]
  )

# Road net info
road_net_info_main <-
  road_net_info %>%
  dplyr::filter(
    FEATURE_OID %in% edges_main$ID
  )


### All but main  ----
nodes_all_but_main <-
  net_all_but_main %>%
  sfnetworks::activate("nodes") %>%
  sf::st_as_sf()

edges_all_but_main <-
  net_all_but_main %>%
  sfnetworks::activate("edges") %>%
  tibble::as_tibble()

# Transfer geometries from original edges object based on matching IDs
idx_2 <-
  base::match(
    as.character(edges_all_but_main$ID),
    as.character(edges$ID)
  )

edges_all_but_main <-
  sf::st_set_geometry(
    edges_all_but_main,
    edges$GEOMETRY[idx_2]
  )

# No need for road net info for these


## Write ----
### Main component ----
file_main <-
  'nr_gpkg/trafikklenker_undir_norge.gpkg'

sf::st_write(
  nodes_main,
  dsn = file_main,
  layer = 'nodes_main'
)

sf::st_write(
  edges_main,
  dsn = file_main,
  layer = 'edges_main'
)

sf::st_write(
  road_net_info_main,
  dsn = file_main,
  layer = 'road_net_info'
)


### All but main ----
file_all_but_main <-
  'nr_gpkg/trafikklenker_undir_norge_all_but_main.gpkg'

sf::st_write(
  nodes_all_but_main,
  dsn = file_all_but_main,
  layer = 'nodes_all_but_main'
)

sf::st_write(
  edges_all_but_main,
  dsn = file_all_but_main,
  layer = 'edges_all_but_main'
)

# Do not need road net info for these


# 2. Undirected road net Region vest ----
# Polygoner for Vestland og Rogaland fylker er hentet herfra:
# https://kartkatalog.geonorge.no/metadata/administrative-enheter-kommuner/041f1e6e-bdbc-4091-b48f-8a5990f3cc5b

path_vestland <-
  'nr_prep_links/Basisdata_46_Vestland_25833_Kommuner_GEOJSON.geojson'

path_rogaland <-
  'nr_prep_links/Basisdata_11_Rogaland_25833_Kommuner_GEOJSON.geojson'

#sf::st_layers(path_vestland)

vestland <-
  sf::st_read(
    path_vestland,
    layer = 'administrative_enheter.kommune'
  )

rogaland <-
  sf::st_read(
    path_rogaland,
    layer = 'administrative_enheter.kommune'
  )

region_vest_kommuner <-
  dplyr::bind_rows(vestland, rogaland)

region_vest <-
  region_vest_kommuner %>%
  sf::st_union()

# Overlapping edges
ind <-
  sf::st_intersects(
    edges_main,
    region_vest
  )

ind_rv <-
  lengths(ind) == 1

edges_rv <- edges_main[ind_rv, ]

# Road net info
road_net_info_rv <-
  road_net_info_main %>%
  dplyr::filter(
    FEATURE_OID %in% edges_rv$ID
  )

# Nodes
# nodes_rv_covered_or_not <-
#   sf::st_covered_by(
#     nodes_main,
#     region_vest
#   )
#
# nodes_rv_covered <-
#   which(lengths(nodes_rv_covered_or_not) == 1)

nodes_rv_connected_ids <-
  unique(
    c(
      edges_rv$START_NODE_OID,
      edges_rv$END_NODE_OID
    )
  )

nodes_rv_connected <-
  which(
    nodes_main$FEATURE_OID %in% nodes_rv_connected_ids)

nodes_rv <- nodes_main[nodes_rv_connected, ]


## Urban area ratio ----
# TODO: add urban ratio here as the duplication of two way links
# will not affect this ratio


## Write ----
# TODO: functonize write to gpkg
file_rv <-
  'nr_gpkg/trafikklenker_undir_rv.gpkg'

sf::st_write(
  nodes_rv,
  dsn = file_rv,
  layer = 'nodes_rv'
)

sf::st_write(
  edges_rv,
  dsn = file_rv,
  layer = 'edges_rv'
)

sf::st_write(
  road_net_info_rv,
  dsn = file_rv,
  layer = 'road_net_info'
)

## Read back in ----
edges_rv <-
  sf::st_read(
    file_rv,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"edges_rv\""
  )

nodes_rv <-
  sf::st_read(
    file_rv,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"nodes_rv\""
  )

road_net_info_rv <-
  sf::st_read(
    file_rv,
    layer = 'road_net_info'
  )


# 3. Directed road net ----
# Look at how many links with different directions
edges_rv %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::count(DIRECTION)

## Removing links without direction ----
# Links with direction = 0 are short connection links that must be removed,
# but the remaining links must be reconnected

# Zero-links, their nodes and road net element IDs
edges_rv_direction_0 <-
  edges_rv %>%
  dplyr::filter(
    DIRECTION == 0
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    ID,
    from,
    to,
    START_NODE_OID,
    END_NODE_OID
  ) %>%
  dplyr::left_join(
    road_net_info_rv,
    by = c("ID" = "FEATURE_OID")
  )

# Need to find the neighbouring links that is located on the far side of the
# zero-link seen from the end node of the wanted link topology.
# Assuming this is identified by links on the same road net element,
# and including other links connected to the same node.

# Neighbours on the start node
edges_rv_direction_0_neighbours_1 <-
  edges_rv %>%
  dplyr::filter(
    START_NODE_OID %in% c(edges_rv_direction_0$START_NODE_OID,
                          edges_rv_direction_0$END_NODE_OID)
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    ID,
    from,
    to,
    START_NODE_OID,
    END_NODE_OID
  )

# Neighbours on the end node
edges_rv_direction_0_neighbours_2 <-
  edges_rv %>%
  dplyr::filter(
    END_NODE_OID %in% c(edges_rv_direction_0$START_NODE_OID,
                         edges_rv_direction_0$END_NODE_OID)
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    ID,
    from,
    to,
    START_NODE_OID,
    END_NODE_OID
  )

# All neighbours with road net info
edges_rv_direction_0_neighbours <-
  dplyr::bind_rows(
    edges_rv_direction_0_neighbours_1,
    edges_rv_direction_0_neighbours_2
  ) %>%
  dplyr::distinct() %>%
  # Need to remove the zero-links as they are included again here
  dplyr::filter(
    !(ID %in% edges_rv_direction_0$ID)
  ) %>%
  dplyr::left_join(
    road_net_info_rv,
    by = c("ID" = "FEATURE_OID")
  )

# Need to identify the node-IDs which are going to be replaced
# For each zero-link, identify the neighbouring link on the same
# road net element
edges_rv_direction_0_and_neighbour <-
  edges_rv_direction_0 %>%
  dplyr::left_join(
    edges_rv_direction_0_neighbours,
    by = "ELEMENT_ID",
    suffix = c("_zero", "_neighbour")
  )

# If there is no match, the change must be done manually
edges_rv_direction_0_and_neighbour_no_match <-
  edges_rv_direction_0_and_neighbour %>%
  dplyr::filter(
    is.na(ID_neighbour)
  )

# Link ID 1014557512 must change its START_NODE_ID from 820818 to 2897991
# and "to" node to 8698
edges_rv$START_NODE_OID[edges_rv$ID == "1014557512"] <- 2897991
edges_rv$to[edges_rv$ID == "1014557512"] <- 8698


# The others can be done away with programmatically
# Identify the node that is going to be swapped
# (there might be more than one link having them)
edges_rv_direction_0_and_neighbour_with_match <-
  edges_rv_direction_0_and_neighbour %>%
  dplyr::filter(
    !is.na(ID_neighbour)
  ) %>%
  dplyr::mutate(
    node_id_to_be_swapped =
      dplyr::case_when(
        START_NODE_OID_zero == START_NODE_OID_neighbour ~ START_NODE_OID_zero,
        START_NODE_OID_zero == END_NODE_OID_neighbour ~ START_NODE_OID_zero,
        END_NODE_OID_zero == START_NODE_OID_neighbour ~ END_NODE_OID_zero,
        END_NODE_OID_zero == END_NODE_OID_neighbour ~ END_NODE_OID_zero
      ),
    node_id_new_value =
      dplyr::case_when(
        START_NODE_OID_zero != node_id_to_be_swapped ~ START_NODE_OID_zero,
        TRUE ~ END_NODE_OID_zero
      )
  )

# A named vector for recoding
node_swaps <-
  structure(
    edges_rv_direction_0_and_neighbour_with_match$node_id_new_value,
    names = edges_rv_direction_0_and_neighbour_with_match$node_id_to_be_swapped
  )


# Delete swapped nodes
# Delete road net info for zero links

# Delete zero-links and swap nodes to reconnect graph
edges_rv_clean <-
  edges_rv %>%
  dplyr::filter(
    DIRECTION != 0
  ) %>%
  dplyr::mutate(
    START_NODE_OID =
      dplyr::recode(
        START_NODE_OID,
        !!!node_swaps
      ),
    END_NODE_OID =
      dplyr::recode(
        END_NODE_OID,
        !!!node_swaps
      )
  )

nodes_dir_rv <-
  nodes_rv %>%
  dplyr::filter(
    !(FEATURE_OID %in%
        edges_rv_direction_0_and_neighbour_with_match$node_id_to_be_swapped)
  )

road_net_info_dir_rv <-
  road_net_info_rv %>%
  dplyr::filter(
    !(FEATURE_OID %in% edges_rv_direction_0$ID)
  )


## Duplicating two way links ----
# legg til metreringsretning-dummy
edges_rv_dir <-
  edges_rv_clean %>%
  dplyr::mutate(
    med_metrering = 1
  )

# dupliserer lenker med toveistrafikk
trafikklenker_enveis <-
  filter(edges_rv_dir, DIRECTION < 3)

trafikklenker_toveis <-
  filter(edges_rv_dir, DIRECTION == 3)

trafikklenker_toveis_mot <- trafikklenker_toveis # lag en kopi

identical(trafikklenker_toveis_mot, trafikklenker_toveis)
# TRUE

# snu retning
trafikklenker_toveis_mot <- trafikklenker_toveis_mot %>%
  mutate(
    # snu start- og sluttnode
    START_NODE_OID = trafikklenker_toveis$END_NODE_OID,
    END_NODE_OID = trafikklenker_toveis$START_NODE_OID,
    # angi at lenkeretning er mot metreringsretning ved å sette dummy = 0
    med_metrering = 0,
    # snu vegreferansen
    ROADREF_START = trafikklenker_toveis$ROADREF_END,
    ROADREF_END  = trafikklenker_toveis$ROADREF_START,
    # snu from og to
    from = trafikklenker_toveis$to,
    to = trafikklenker_toveis$from
  )

identical(trafikklenker_toveis_mot, trafikklenker_toveis)
# FALSE

edges_rv_dir_complete <-
  dplyr::bind_rows(
    trafikklenker_enveis,
    trafikklenker_toveis,
    trafikklenker_toveis_mot
  ) %>%
  dplyr::arrange(ID)


# enveislenker med kjøreretning mot lenkeretning (= metreringsretning)
enveis <-
  edges_rv_dir_complete %>%
  dplyr::filter(DIRECTION == 2)

# resten av lenkene
ikke_enveis <-
  edges_rv_dir_complete %>%
  dplyr::filter(DIRECTION != 2)

# snu retning
enveis <- enveis %>%
  mutate(
    # snu start- og sluttnode
    START_NODE_OID = enveis$END_NODE_OID,
    END_NODE_OID = enveis$START_NODE_OID,
    # angi at lenkeretning er mot metreringsretning ved å sette dummy = 0
    med_metrering = 0,
    # snu vegreferansen
    ROADREF_START = enveis$ROADREF_END,
    ROADREF_END  = enveis$ROADREF_START,
    # snu from og to
    from = enveis$to,
    to = enveis$from
  )

# slå sammen lenker
edges_rv_dir_complete_with_all_turned <-
  dplyr::bind_rows(
    ikke_enveis,
    enveis
  ) %>%
  dplyr::arrange(ID) %>%
  tibble::rownames_to_column(var = "lenke_nr")

#rm(trafikklenker_enveis, trafikklenker_toveis, trafikklenker_toveis_mot)

table(edges_rv_dir_complete_with_all_turned$med_metrering)


## Write ----
# file_rv_dir <-
#   'nr_gpkg/trafikklenker_dir_rv.gpkg'
#
# sf::st_write(
#   nodes_dir_rv,
#   dsn = file_rv_dir,
#   layer = 'nodes_rv'
# )
#
# sf::st_write(
#   edges_rv_dir_complete_with_all_turned,
#   append = FALSE,
#   dsn = file_rv_dir,
#   layer = 'edges_rv'
# )
#
# sf::st_write(
#   road_net_info_dir_rv,
#   dsn = file_rv_dir,
#   layer = 'road_net_info'
# )


## Read back in ----
# edges_dir_rv <-
#   sf::st_read(
#     file_rv_dir,
#     # as_tibble = TRUE,
#     query = "SELECT * FROM \"edges_rv\""
#   )
#
# nodes_dir_rv <-
#   sf::st_read(
#     file_rv_dir,
#     # as_tibble = TRUE,
#     query = "SELECT * FROM \"nodes_rv\""
#   )
#
# road_net_info_dir_rv <-
#   sf::st_read(
#     file_rv_dir,
#     layer = 'road_net_info'
#   )
#
# look_at_edges <-
#   edges_dir_rv %>%
#   sf::st_drop_geometry()


# 4. Transport model AADT ----

## Read ----
# Using the values already connected to road net in former part of the project.
former_transport_model_enhanced_gpkg <-
  'nr_gpkg/graf_dir_maincomp_20210505.gpkg'

#sf::st_layers(former_transport_model_enhanced_gpkg)
edges_dir_rv_former <-
  sf::st_read(
    former_transport_model_enhanced_gpkg,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"edges_main_dir\""
  )

edges_dir_rv_former_small <-
  edges_dir_rv_former %>%
  dplyr::select(
    ID,
    med_metrering,
    ROADREF_START,
    ROADREF_END,
    MUNICIPALITY,
    AADT_ALLE_MIN,
    AADT_ALLE_MAX,
    AADT_ALLE_MEAN
  )

rm(edges_dir_rv_former)
#look_at_former <- head(edges_dir_rv_former)


## Matching geometry ----
# Use just neceasry columns here
# Join transport model columns back on edges after joining on geometry
# and aggregating AADT values if overlap with more than one link
edges_dir_rv_small <-
  #edges_dir_rv %>%
  edges_rv_dir_complete_with_all_turned %>%
  dplyr::select(
    lenke_nr,
    ROADREF_START,
    ROADREF_END,
    med_metrering
  )

# Problem: links that share just a point on their common node is joined on
# geometry.
# Check to see if many links cover different roads or if this can be used as
# a filter for removing just point overlap.
# compare_start_and_end_road_no <-
#   edges_dir_rv %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(
#     ID,
#     ROADREF_START,
#     ROADREF_END
#   ) %>%
#   dplyr::mutate(
#     start = stringr::str_extract(ROADREF_START, "[:upper:]{1}V[:digit:]*"),
#     end = stringr::str_extract(ROADREF_END, "[:upper:]{1}V[:digit:]*"),
#     same_road = start == end
#   ) %>%
#   dplyr::filter(
#     same_road == FALSE
#   )
# 19 have different road at start and end. This is just 0.2 %. Ok to ignore.

edges_with_overlapping_geometry <-
  edges_dir_rv_small %>%
  sf::st_join(edges_dir_rv_former_small) %>%
  dplyr::filter(
    med_metrering.x == med_metrering.y
  ) %>%
  # Problem: links that share just a point on their common node is joined
  # Solution: keep only links that start on the same road
  dplyr::mutate(
    start.x1 =
      stringr::str_extract(
        ROADREF_START.x,
        "[:upper:]{1}V[:digit:]*"
      ) %>%
      stringr::str_replace(
        "V",
        ""
      ),
    end.x =
      stringr::str_extract(
        ROADREF_END.x,
        "[:upper:]{1}V[:digit:]*"
      ) %>%
      stringr::str_replace(
        "V",
        ""
      ),
    start.x =
      dplyr::case_when(
        is.na(start.x1) ~ end.x,
        TRUE ~ start.x1
      ),
    start.y =
      stringr::str_extract(
        ROADREF_START.y,
        "^[:upper:]{1}[:digit:]*"
      )
  ) %>%
  dplyr::filter(
    start.x == start.y
  ) %>%
  dplyr::select(
    lenke_nr,
    med_metrering = med_metrering.x,
    starts_with("AADT")
  ) %>%
  sf::st_drop_geometry() %>%
  # Set 0 values to NA to avoid them messing up the aggregations
  dplyr::mutate(
    dplyr::across(
      .cols = starts_with("AADT"),
      .fns = ~ dplyr::na_if(., 0)
    )
  ) %>%
  dplyr::group_by(lenke_nr) %>%
  dplyr::summarise(
    AADT_ALLE_MIN = base::min(AADT_ALLE_MIN, na.rm = TRUE),
    AADT_ALLE_MAX = base::max(AADT_ALLE_MAX, na.rm = TRUE),
    AADT_ALLE_MEAN = base::mean(AADT_ALLE_MEAN, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = starts_with("AADT"),
      .fns = ~ dplyr::na_if(., Inf)
    ),
    dplyr::across(
      .cols = starts_with("AADT"),
      .fns = ~ dplyr::na_if(., -Inf)
    ),
    dplyr::across(
      .cols = starts_with("AADT"),
      .fns = ~ ifelse(is.nan(.), NA, .)
    )
  )

#length(unique(edges_with_overlapping_geometry$lenke_nr))

# join back
edges_dir_rv_tm <-
  #edges_dir_rv %>%
  edges_rv_dir_complete_with_all_turned %>%
  dplyr::left_join(
    edges_with_overlapping_geometry,
    by = "lenke_nr"
  )

# look_at_edges_dir_rv_tm <-
#   edges_dir_rv_tm %>%
#   sf::st_drop_geometry()


# 5. Traffic registration ADT ----
# How is it to be saved in GPKG?
# In its own layer, with link ID and med_metrering as keys
# aadt_dir_rv_former <-
#   sf::st_read(
#     former_transport_model_enhanced_gpkg,
#     # as_tibble = TRUE,
#     query = "SELECT * FROM \"aadt\""
#   )
#names(aadt_dir_rv_former)
# "FEATURE_OID"    "aadt_prelim"    "aadt_prelim_sd"
# "direction"      "size" vehicle_class      "year"
# "type" registration_frequency


# AADT per direction in RV
# TODO: AADT per direction and three classes in RV
# both continuous and periodic (factor curve values) (not radar)
# connect them to correct link and direction by using road link info

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

## TRPs ----
trp <- get_points()

distinct_trps <- trp %>%
  split_road_system_reference() %>%
  dplyr::select(
    trp_id,
    traffic_type,
    county_name,
    registration_frequency,
    road_network_link,
    road_network_position
  ) %>%
  dplyr::distinct(trp_id, .keep_all = T)

trp_id_and_road_link <-
  distinct_trps %>%
  dplyr::select(
    trp_id,
    road_network_link,
    road_network_position,
    registration_frequency
  ) %>%
  dplyr::mutate(
    road_network_link = as.character(road_network_link)
  )


## TRP on traffic link ----
# Resulting table must have:
# traffic link ID
# med_metrering
# trp_id

link_id_and_trp_id <-
  road_net_info_dir_rv %>%
  dplyr::left_join(
    trp_id_and_road_link,
    by = c("ELEMENT_ID" = "road_network_link")
  ) %>%
  dplyr::mutate(
    on_link =
      road_network_position > START_MEASURE &
      road_network_position < END_MEASURE |
      road_network_position < START_MEASURE &
      road_network_position > END_MEASURE
  ) %>%
  dplyr::filter(
    on_link == TRUE
  ) %>%
  dplyr::select(
    ID = FEATURE_OID,
    trp_id,
    registration_frequency
  )
# ok with multiple trps on same link


## TRP heading ----
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
    med_metrering = 1
  )

trp_against_metering <-
  trp_direction %>%
  dplyr::select(
    trp_id,
    heading = from_according_to_metering
  ) %>%
  dplyr::mutate(
    med_metrering = 0
  )

trp_heading <-
  dplyr::bind_rows(
    trp_with_metering,
    trp_against_metering
  ) %>%
  dplyr::arrange(
    trp_id
  )


## AADT ----
trp_rv <-
  distinct_trps %>%
  dplyr::filter(
    county_name %in% c("Rogaland", "Vestland"),
    traffic_type == "VEHICLE"
  )

# aadt_rv <-
#   get_aadt_by_direction_for_trp_list(
#     trp_rv$trp_id
#   ) %>%
#   dplyr::filter(
#     year > 2013
#   )
#
# saveRDS(
#   aadt_rv,
#   file = "nr_prep_links/aadt_rv.rds"
# )

aadt_rv <-
  readRDS(
    file = "nr_prep_links/aadt_rv.rds"
  )

# Per Feb 2022 factor curve AADTs aren't in the API, fetched from Kibana:
aadt_by_factor_curve <-
  readr::read_csv2(
    "periodic_data/periodic_aadt_by_factor_curve.csv"
  ) %>%
  dplyr::mutate(
    se_mean = (confidence_interval.upper - confidence_interval.lower) / 4
  ) %>%
  dplyr::select(
    trp_id = traffic_registration_point_id,
    year,
    adt_factor_curve = adt,
    se_mean,
    curve
  ) %>%
  dplyr::filter(
    year < 2022
  )

# using factor curve aadt divided by 2 when it is available
periodic_aadt_compare_factor_curve_and_naive <-
  aadt_rv %>%
  dplyr::left_join(
    aadt_by_factor_curve,
    by = c("trp_id", "year")
  ) %>%
  dplyr::mutate(
    adt_factor_curve_halfed = floor(adt_factor_curve / 2),
    adt_diff = adt - adt_factor_curve_halfed
  ) %>%
  dplyr::select(
    trp_id,
    heading,
    year,
    adt,
    standard_deviation,
    se_mean = se_mean.y,
    adt_factor_curve_halfed,
    adt_diff
  ) %>%
  dplyr::filter(
    !is.na(adt_factor_curve_halfed)
  ) %>%
  dplyr::select(
    -adt,
    -adt_diff,
    adt = adt_factor_curve_halfed
  )

aadt_rv_tidy <-
  aadt_rv %>%
  dplyr::filter(
    total.coverage.percentage > 50 |
      is.na(total.coverage.percentage),
    adt > 25
  ) %>%
  dplyr::select(
    trp_id,
    heading,
    year,
    adt,
    standard_deviation,
    se_mean
  ) %>%
  dplyr::anti_join(
    periodic_aadt_compare_factor_curve_and_naive,
    by = c("trp_id", "year")
  ) %>%
  dplyr::bind_rows(
    periodic_aadt_compare_factor_curve_and_naive
  ) %>%
  # remove errors
  dplyr::filter(
    !(trp_id == "51214V805707" & adt < 1000)
  )

traffic_link_id_and_aadt_rv <-
  link_id_and_trp_id %>%
  dplyr::left_join(
    trp_heading,
    by = "trp_id"
  ) %>%
  dplyr::left_join(
    aadt_rv_tidy,
    by = c("trp_id", "heading")
  ) %>%
  dplyr::filter(
    !is.na(year)
  ) %>%
  dplyr::select(
    #trp_id,
    FEATURE_OID = ID,
    aadt_prelim = adt,
    #aadt_prelim_sd = standard_deviation,
    aadt_prelim_sd = se_mean,
    direction = med_metrering,
    year,
    type = registration_frequency
  ) %>%
  dplyr::mutate(
    size = "ALL",
    type =
      dplyr::case_when(
        type == "CONTINUOUS" ~ "K",
        type == "PERIODIC" ~ "P"
      )
  )

# aadts_without_match <-
#   aadt_rv_tidy %>%
#   dplyr::filter(
#     !(trp_id %in% traffic_link_id_and_aadt_rv$trp_id)
#   ) %>%
#   dplyr::select(
#     trp_id
#   ) %>%
#   dplyr::distinct()
#
# links_with_more_than_one_trp_aadt <-
#   traffic_link_id_and_aadt_rv %>%
#   dplyr::select(
#     trp_id,
#     FEATURE_OID
#   ) %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(
#     FEATURE_OID
#   ) %>%
#   dplyr::summarise(
#     n_trp = n()
#   ) %>%
#   dplyr::filter(
#     n_trp > 1
#   )
#
# multiple_aadt <-
#   traffic_link_id_and_aadt_rv %>%
#   dplyr::filter(
#     FEATURE_OID %in% links_with_more_than_one_trp_aadt$FEATURE_OID
#   )
#
# links_with_duplicate_headings <-
#   aadt_rv_tidy %>%
#   dplyr::select(
#     trp_id,
#     heading,
#     year
#   ) %>%
#   dplyr::group_by(
#     trp_id,
#     heading,
#     year
#   ) %>%
#   dplyr::summarise(
#     n_aadt = n()
#   ) %>%
#   dplyr::filter(
#     n_aadt > 1
#   )


# 6. Graph centrality parameters ----
## Targeted data cleansing ----
# Adds columns to edges:
# ferry speed
# travel time (normalized)
# link length (normalized)
# aadt cleasning already dealt with above
# no alteration of the nodes here

edges <-
  edges_dir_rv_tm %>%
  dplyr::mutate(
    ferry_speed_5 = MAKS_SPEED,
    ferry_speed_10 = MAKS_SPEED,
    ferry_speed_15 = MAKS_SPEED,
    pred_lane = MIN_LANE,
    MD = as.numeric(sf::st_length(geom))/1e3
  )

#head(edges)

## Euclidean lengths of each traffic link
#     Two measures:
#       i) absolute metric distance [km]
#      ii) normalized to [0,1] using longest path as normalizing factor

max.path <- max(edges$MD)

edges <-
  edges %>%
  dplyr::mutate(
    MD_norm = MD/max.path
  )


## Identify traffic links with missing MIN/MAKS_SPEED (assumed ferry ledges)
#    - Assign alternative plausibly low speeds
#      to account for waiting times on top of the passage itself
#    - For later use with centrality calculations
idx <- is.na(edges$MAKS_SPEED) | edges$MAKS_SPEED==0
edges$ferry_speed_5[idx] <- 5
edges$ferry_speed_10[idx] <- 10
edges$ferry_speed_15[idx] <- 15

# Valid factor levels: 30,40,50,60,70,80,90
idx <- edges$ferry_speed_5 < 30
edges$ferry_speed_5[idx] <- 30
idx <- edges$ferry_speed_10 < 30
edges$ferry_speed_10[idx] <- 30
idx <- edges$ferry_speed_15 < 30
edges$ferry_speed_15[idx] <- 30


## Assign (minimum) travel time to each traffic link
#     Two measures:
#       i) absolute travel time in minutes assuming travel speed=MAKS_SPEED
#      ii) normalized to [0,1] using longest travel time as normalizing factor
edges <-
  edges %>%
  dplyr::mutate(
    Tt_5 = MD/ferry_speed_5*60,
    Tt_10 = MD/ferry_speed_10*60,
    Tt_15 = MD/ferry_speed_15*60
  )

max.time <- max(edges$Tt_10)

edges <-
  edges %>%
  dplyr::mutate(
    Tt_norm = Tt_10/max.time
  )


## MIN_VEGKL_9338
#  Traffic links data Region vest 2021:
#  No traffic count stations alongside links in categories 6,7,8,9;
#  those are thus included in category 5 to enable prediction from the model
#   - Valid factor levels all categories: 0,1,2,3,4,5
#   - ROADREF_CATEGORY == 'E' or 'R': Map levels 4,5(,6) to level 3
idx <- edges$MIN_VEGKL_9338 %in% c(6:9)
edges$MIN_VEGKL_9338[idx] <- 5

idx <- edges$ROADREF_CATEGORY %in% c('E','R') & edges$MIN_VEGKL_9338 >= 4
edges$MIN_VEGKL_9338[idx] <- 3


## pred_lane
#  Traffic links data Region vest 2021:
#  No traffic count stations alongside links with more than 6 lanes;
#  such links are thus included in category 6 to enable
#  prediction from the model
#   - Valid factor levels: 1:6
idx <- edges$pred_lane > 6
edges$pred_lane[idx] <- 6



## Build directed graph object ----
# First, add indices for start and end nodes of each edge wrt nodes object
edges$from <-
  base::match(
    edges$START_NODE_OID,
    nodes_dir_rv$FEATURE_OID
  )

edges$to <-
  base::match(
    edges$END_NODE_OID,
    nodes_dir_rv$FEATURE_OID
  )

# Build directed graph object from nodes and spatially implicit edges
# The edges must contain columns 'from' and 'to'
net_RV_dir <-
  sfnetworks::sfnetwork(
    nodes = nodes_dir_rv,
    edges = edges,
    directed = TRUE,
    node_key = "FEATURE_OID",
    edges_as_lines = FALSE,
    force = FALSE
  )

# Verify that directed graph contains one single component
net_RV_dir %>% count_components(mode="weak")

cc <- net_RV_dir %>% components(mode="weak")

rev(table(cc$csize))


## Compute edge betweenness centrality ----
net_main_bc <-
  net_RV_dir %>%
  activate("edges") %>%
  mutate(
    #bc_noweights = centrality_edge_betweenness(weights=NULL,directed=TRUE,cutoff=NULL),
    bc_MD =
      centrality_edge_betweenness(
        weights=MD,
        directed=TRUE,
        cutoff=NULL),
    bc_Tt_5 =
      centrality_edge_betweenness(
        weights=Tt_5,
        directed=TRUE,
        cutoff=NULL),
    bc_Tt_10 =
      centrality_edge_betweenness(
        weights=Tt_10,
        directed=TRUE,
        cutoff=NULL),
    bc_Tt_15 =
      centrality_edge_betweenness(
        weights=Tt_15,
        directed=TRUE,
        cutoff=NULL))
# bc_MDnorm_weights = centrality_edge_betweenness(weights=MD_norm,directed=TRUE,cutoff=NULL),
# bc_Ttnorm_weights = centrality_edge_betweenness(weights=Tt_norm,directed=TRUE,cutoff=NULL),
# bc_MDTt_weights = centrality_edge_betweenness(weights=MD_norm*Tt_norm,directed=TRUE,cutoff=NULL),
# bc_MDplusTt_weights = centrality_edge_betweenness(weights=MD_norm+Tt_norm,directed=TRUE,cutoff=NULL))


# Add geometries to edges ----
# nodes_main <- net_main_bc %>%
#   activate("nodes") %>%
#   st_as_sf()

edges_main <- net_main_bc %>%
  activate("edges") %>%
  as_tibble()

## Transfer geometries from original edges object based on matching IDs
idx <- match(as.character(edges_main$ID),as.character(edges$ID))
edges_main <- st_set_geometry(edges_main,edges$geom[idx])


# 7. Write final file ----
final_file_rv_dir <-
  'nr_gpkg/trafikklenker_dir_rv_2021_5.gpkg'

sf::st_write(
  nodes_dir_rv,
  dsn = final_file_rv_dir,
  layer = 'nodes'
)

sf::st_write(
  edges_main,
  dsn = final_file_rv_dir,
  layer = 'edges'
)

# sf::st_write(
#   road_net_info_rv_clean,
#   dsn = final_file_rv_dir,
#   layer = 'road_net_info'
# )

sf::st_write(
  traffic_link_id_and_aadt_rv,
  append = FALSE,
  dsn = final_file_rv_dir,
  layer = 'aadt'
)


# 8. Urban area ratio ----
# TODO: move this part to directed graph when
# starting from the very beginning again

# Using urban areas defined by SSB https://www.ssb.no/natur-og-miljo/geodata
urban_areas_norway <-
  sf::st_read(
    "nr_gpkg/tettsted2021/tettsted2021.shp"
  ) %>%
  sf::st_set_crs(25833)
#st_crs(region_vest) == st_crs(urban_areas_norway)

urban_areas_rv <-
  urban_areas_norway[st_intersects(
    urban_areas_norway,
    region_vest) %>% lengths > 0,] %>%
  dplyr::select(
    TETTNR
  )

#head(urban_areas_rv)

# Write urban areas to file and check in ArcGIS
# sf::st_write(
#   urban_areas_rv,
#   dsn = "nr_gpkg/urban_areas_rv.gpkg",
#   layer = 'urban_areas_rv'
# )
# OK!

# The edges
edges_dir_rv <-
  edges_main
  # sf::st_read(
  #   final_file_rv_dir,
  #   query = "SELECT * FROM \"edges\""
  # )
names(edges_dir_rv)

edge_id_and_total_length <-
  edges_dir_rv %>%
  dplyr::select(
    lenke_nr,
    total_length = MD
  ) %>%
  sf::st_drop_geometry()

# Reduce processing time by first filtering links
# keeping just those which overlap urban areas
edge_ids <-
  edges_dir_rv %>%
  dplyr::select(
    lenke_nr
  )

edges_with_some_urban_part <-
  edge_ids[st_intersects(
    edge_ids,
    urban_areas_rv) %>% lengths > 0,]

# Making all urban areas one multipolygon
urban_areas_rv_unified <-
  sf::st_union(
    urban_areas_rv
  )

tictoc::tic()
edges_non_urban_part <-
  edges_with_some_urban_part %>%
  sf::st_difference(
    urban_areas_rv_unified
  )
tictoc::toc()
# Time spent: 815 s, 837 s, 734 s, 787 s
# Still takes too much time
# Reduce by first finding which urban areas each link intersects with,
# and then find st_difference for just those?
# TODO: Do not include those which are completely within an urban area

# Calculate non urban length
edges_non_urban_part_ratio <-
  edges_non_urban_part %>%
  dplyr::mutate(
    non_urban_length = as.numeric(sf::st_length(geometry)) / 1e3
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::inner_join(
    edge_id_and_total_length,
    by = "lenke_nr"
  ) %>%
  dplyr::mutate(
    urban_ratio = non_urban_length / total_length
  ) %>%
  dplyr::select(
    lenke_nr,
    urban_ratio
  )

edges_completely_inside_urban_areas <-
  edges_with_some_urban_part %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    !(lenke_nr %in% edges_non_urban_part$lenke_nr)
  ) %>%
  dplyr::mutate(
    urban_ratio = 1
  )

edges_completely_outside_urban_areas <-
  edge_ids %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(
    !(lenke_nr %in% edges_with_some_urban_part$lenke_nr)
  ) %>%
  dplyr::mutate(
    urban_ratio = 0
  )


edge_ids_and_urban_ratio <-
  dplyr::bind_rows(
    edges_non_urban_part_ratio,
    edges_completely_inside_urban_areas,
    edges_completely_outside_urban_areas
  ) %>%
  dplyr::rename(
    T_andel = urban_ratio
  )

## Join back with edges ----
edges_dir_rv_urban_ratio <-
  edges_dir_rv %>%
  dplyr::left_join(
    edge_ids_and_urban_ratio,
    by = "lenke_nr"
  )

## Write ----
sf::st_write(
  edges_dir_rv_urban_ratio,
  append = FALSE,
  dsn = final_file_rv_dir,
  layer = 'edges'
)


# 9. Municipality ----

## Read back in ----
final_file_rv_dir <-
  'nr_gpkg/trafikklenker_dir_rv_2021_5.gpkg'

edges_rv <-
  #edges_dir_rv_urban_ratio
  sf::st_read(
    final_file_rv_dir,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"edges\""
  )

nodes_rv <-
  #nodes_dir_rv
  sf::st_read(
    final_file_rv_dir,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"nodes\""
  )

aadt_rv <-
  sf::st_read(
    final_file_rv_dir,
    query = "SELECT * FROM \"aadt\""
  )


source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")


## Sola ----
sola <-
  hent_kommune_v3(1124)

edges_sola <-
  edges_rv[st_intersects(
    edges_rv,
    sola) %>% lengths > 0,]


### Write ----
final_file_sola <-
  'nr_gpkg/trafikklenker_sola_2021.gpkg'

sf::st_write(
  nodes_rv,
  dsn = final_file_sola,
  layer = 'nodes'
)

sf::st_write(
  edges_sola,
  dsn = final_file_sola,
  layer = 'edges'
)

sf::st_write(
  #aadt_rv,
  traffic_link_id_and_aadt_rv,
  append = FALSE,
  dsn = final_file_sola,
  layer = 'aadt'
)

### Read back in ----
edges_sola <-
  #edges_dir_rv_urban_ratio
  sf::st_read(
    final_file_sola,
    # as_tibble = TRUE,
    query = "SELECT * FROM \"edges\""
  )

## Nord-Jaeren ----
sola <-
  hent_kommune_v3(1124)

stavanger <-
  hent_kommune_v3(1103)

sandnes <-
  hent_kommune_v3(1108)

randaberg <-
  hent_kommune_v3(1127)

nj <-
  dplyr::bind_rows(
    sola,
    stavanger,
    sandnes,
    randaberg
  ) %>%
  sf::st_union()

edges_nj <-
  edges_rv[sf::st_intersects(
    edges_rv,
    nj) %>% lengths > 0,]


### Write nj ----
final_file_nj <-
  'nr_gpkg/trafikklenker_nj_2021.gpkg'

sf::st_write(
  nodes_rv,
  dsn = final_file_nj,
  layer = 'nodes'
)

sf::st_write(
  edges_nj,
  dsn = final_file_nj,
  layer = 'edges'
)

sf::st_write(
  aadt_rv,
  #traffic_link_id_and_aadt_rv,
  append = FALSE,
  dsn = final_file_nj,
  layer = 'aadt'
)



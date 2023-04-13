library(dplyr)
library(DiagrammeR)

optim_fit <- lapply(optim_fit, ungroup)

stocks <- optim_fit$res.by.year %>%
  filter(year == max(year)) %>%
  mutate(size = total.biomass/sum(total.biomass),
         type = "stock") %>%
  rename("name" = "stock") %>%
  dplyr::select(name, type, size)

fleets <- optim_fit$fleet.info %>%
  filter(year == max(year)) %>%
  mutate(size = amount/sum(amount),
         type = "fleet") %>%
  rename("name" = "fleet") %>%
  dplyr::select(name, type, size)


predation <- optim_fit$suitability %>%
  dplyr::ungroup() %>%
  dplyr::filter(suit > 0) %>%
  dplyr::select(stock, fleet) %>%
  dplyr::distinct()




graph <- create_graph()

## ####

graph %>%
  add_nodes_from_table(
    table = bind_rows(stocks, fleets) %>% rename("value" = "size"),
    label_col = name,
    type_col = type
  ) %>%
  add_edges_from_table(
    table = predation,
    from_col = stock,
    to_col = fleet,
    from_to_map = label
  ) %>%
  set_node_attrs(
    node_attr = 'fontcolor',
    values = 'black'
  ) %>%
  select_nodes(type == "stock") %>%

  set_node_attrs_ws(
    node_attr = fillcolor,
    value = "orange") %>%
  set_node_attrs_ws(
    node_attr = shape,
    value = "rectangle") %>%
  set_node_attrs_ws(
    node_attr = y,
    value = 4) %>%
  clear_selection() %>%
  render_graph("tree")

#
#   get_global_graph_attr_info()
#
#   select_nodes(type == "stock") %>%

  # render_graph(layout = "tree")

## ####
# nodes <-
#   create_nodes(
#     nodes = 1:4,
#     label = FALSE,
#     type = "lower",
#     style = "filled",
#     color = "aqua",
#     shape = c("circle", "circle",
#               "rectangle", "rectangle"),
#     data = c(3.5, 2.6, 9.4, 2.7))
#
# graph <- create_graph(nodes_df = nodes)
#
# Create a simple NDF
nodes <-
  create_node_df(
    n = 4,
    type = "a",
    label = c(2384, 3942, 8362, 2194),
    style = "filled",
    color = "aqua",
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    value = c(3.5, 2.6, 9.4, 2.7))

# Create a simple EDF
edges <-
  create_edge_df(
    from = c(1, 1, 3, 1),
    to = c(2, 3, 4, 4),
    rel = "related")

# Create the graph object,
# incorporating the NDF and
# the EDF, and, providing some
# global attributes
graph <-
  create_graph(
    nodes_df = nodes,
    edges_df = edges,
    graph_attrs = "layout = neato",
    node_attrs = "fontname = Helvetica",
    edge_attrs = "color = gray20")

# Use the %>% operator between
# `create_graph()` and `render_graph()`
create_graph(
  nodes_df = nodes,
  edges_df = edges,
  graph_attrs = "layout = neato",
  node_attrs = "fontname = Helvetica",
  edge_attrs = "color = gray20") %>%
  render_graph

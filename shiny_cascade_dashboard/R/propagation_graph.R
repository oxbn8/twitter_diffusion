# Propagation graph helpers for Cascade Shape Dashboard (Shiny).
# Used by app_modular.R. Original app.R keeps inline logic unchanged.

#' Coerce values to numeric (edge / time fields).
coerce_numeric_edges <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

#' Load per-tweet edge lists for propagation graphs.
#'
#' Priority: cascade_edges.csv, then data_model.Rdata (`data` object).
#'
#' @return Named list keyed by tweet_id; each element has parent_id, retweeter_id, time_elapsed.
load_propagation_edge_data <- function(
    cascade_edges_csv,
    data_model_path = NA_character_,
    read_csv = function(path) utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
) {
  edge_data <- NULL

  if (file.exists(cascade_edges_csv)) {
    edge_df <- read_csv(cascade_edges_csv)
    edge_df$tweet_id <- as.character(edge_df$tweet_id)
    edge_df$parent_id <- as.character(edge_df$parent_id)
    edge_df$retweeter_id <- as.character(edge_df$retweeter_id)
    edge_df$time_elapsed <- coerce_numeric_edges(edge_df$time_elapsed)
    return(split(edge_df, edge_df$tweet_id))
  }

  if (!is.na(data_model_path) && nzchar(data_model_path) && file.exists(data_model_path)) {
    env <- new.env(parent = emptyenv())
    load(data_model_path, envir = env)
    if (!exists("data", envir = env)) {
      return(NULL)
    }
    raw_data <- get("data", envir = env)
    raw_data$tweet_id <- as.character(raw_data$tweet_id)
    raw_data$retweeter_id <- as.character(raw_data$retweeter_id)
    raw_data$parent_id <- as.character(raw_data$parent_id)
    if ("retweeted" %in% names(raw_data)) {
      rv <- sort(unique(stats::na.omit(coerce_numeric_edges(raw_data$retweeted))))
      if (length(rv) > 0 && all(rv %in% c(0, 1))) {
        raw_data <- raw_data[coerce_numeric_edges(raw_data$retweeted) == 1, , drop = FALSE]
      }
    }
    raw_data <- raw_data[
      !is.na(raw_data$tweet_id) &
        !is.na(raw_data$retweeter_id) &
        !is.na(raw_data$parent_id) &
        raw_data$retweeter_id != raw_data$parent_id,
      c("tweet_id", "parent_id", "retweeter_id", "time_elapsed"),
      drop = FALSE
    ]
    raw_data$time_elapsed <- coerce_numeric_edges(raw_data$time_elapsed)
    return(split(raw_data, raw_data$tweet_id))
  }

  NULL
}

#' Build directed igraph for one cascade (propagation graph).
#'
#' @param edge_data Named list from load_propagation_edge_data().
#' @param tweet_id Character tweet id.
#' @return List with `graph` (igraph) and `edges` (data.frame), or NULL.
build_propagation_graph_for_tweet <- function(edge_data, tweet_id) {
  if (is.null(edge_data) || !tweet_id %in% names(edge_data)) {
    return(NULL)
  }
  df <- edge_data[[tweet_id]]
  df <- unique(df[, c("parent_id", "retweeter_id", "time_elapsed"), drop = FALSE])
  names(df) <- c("from", "to", "time_elapsed")
  df <- df[df$from != df$to & !is.na(df$from) & !is.na(df$to), , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for propagation graphs.")
  }
  g <- igraph::graph_from_data_frame(df[, c("from", "to")], directed = TRUE)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  list(graph = g, edges = df)
}

#' Plot propagation graph (base graphics).
#'
#' @param obj Result of build_propagation_graph_for_tweet().
#' @param main Plot title.
plot_propagation_graph <- function(
    obj,
    main = "Propagation graph",
    root_color = "#f59e0b",
    vertex_color = "#2563eb",
    edge_color = "#94a3b8"
) {
  if (is.null(obj)) {
    return(invisible(FALSE))
  }
  g <- obj$graph
  roots <- igraph::V(g)$name[igraph::degree(g, mode = "in") == 0]
  if (length(roots) == 0) {
    roots <- igraph::V(g)$name[1]
  }
  lay <- igraph::layout_as_tree(g, root = roots, circular = FALSE)
  cols <- ifelse(igraph::V(g)$name %in% roots, root_color, vertex_color)
  graphics::par(mar = c(0, 0, 3, 0))
  plot(
    g,
    layout = lay,
    vertex.size = pmax(5, 12 - log1p(igraph::vcount(g))),
    vertex.label = NA,
    vertex.color = cols,
    edge.arrow.size = 0.25,
    edge.color = edge_color,
    main = main
  )
  invisible(TRUE)
}

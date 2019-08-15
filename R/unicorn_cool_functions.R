#' Creating A Sankey Diagram From Group by Object
#'
#' @param x A group_by object with both categorical variable and at least one numeric variable
#' @param cols A string, defines the categorical variables to use
#' @param num_col A string, define a the numeric variable to use for the sankey diagram
#' @param rcolorbrewer_name A RcolorBrewer palette
#' @return A plotly object
#' @export
#'
#'

sankey_ly <- function(x, cols, num_col, rcolorbrewer_name="Set2") {
  `%>%` <- magrittr::`%>%`
  index <- NULL
  map <- function(x, cols) {
    color_indx <- unique_cat <- map_df <- NULL
    j = 1
    for (i in cols) {
      unique_cat <- c(unique_cat, base::unique(x[, i]))
      color_indx <- c(color_indx, rep(j, nrow(base::unique(x[, i]))))
      j = j+1
    }
    map_df <- base::data.frame(
      cat = base::unlist(unique_cat),
      index = 0:(length(base::unlist(unique_cat)) - 1),
      color_indx = unlist(color_indx),
      stringsAsFactors = FALSE
    )
    return(map_df)
  }
  map <- map(x, cols)
  df <- lapply(1:(base::length(cols) - 1), function(i) {
    df <- NULL
    df <- x %>%
      dplyr::group_by_(s = cols[i], t = cols[i + 1]) %>%
      dplyr::summarise_(.dots = stats::setNames(paste("sum(", num_col, ",na.rm = TRUE)", sep = ""), "total")) %>%
      dplyr::left_join(map %>% dplyr::select(s = cat, source = index)) %>%
      dplyr::left_join(map %>% dplyr::select(t = cat, target = index))
    return(df)
  }) %>% dplyr::bind_rows()

  # map <- map %>% dplyr::left_join(colors)

  p <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    valueformat = ".0f",
    # valuesuffix = "TWh",
    node = list(
      label = map$cat,
      color = RColorBrewer::brewer.pal(n = max(map$color_indx), name = rcolorbrewer_name)[map$color_indx],
      pad = 15,
      thickness = 30,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    link = list(
      source = df$source,
      target = df$target,
      value = df$total
    )
  )
  return(p)
}




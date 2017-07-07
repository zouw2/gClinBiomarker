#' @export
geom_text_table <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", location = 'top', ..., parse = FALSE,
                            nudge_x = 0, nudge_y = 0, check_overlap = FALSE, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, subset) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(data = subset %||% data, mapping = mapping, stat = StatTextDodge, geom = GeomTextTable,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(parse = parse, check_overlap = check_overlap,
                  na.rm = na.rm, location = location, ...))
}

#' @export
GeomTextTable <- ggproto("GeomTextTable", Geom,
  required_aes = c("x", "y", "label"),

  setup_data = function(data, params) {
    if (params$location %||% 'top' == 'top')
      data$y <- (max(data$ymax %||% data$y) - min(data$ymin %||% data$y)) * 1.25 + min(data$ymin %||% data$y)
    else if (params$location == 'bottom')
      data$y <- max(data$ymax %||% data$y) - (max(data$ymax %||% data$y) - min(data$ymin %||% data$y)) * 1.25
    data
  },

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.8
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {

    if (parse) data$label <- parse(text = as.character(lab))
    data$group[data$group == -1] <- 1

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
    if (is.character(data$hjust)) data$hjust <- ggplot2:::compute_just(data$hjust, data$x)

    lineheight.npc <- grid:::convertHeight(unit(data$lineheight * 1.0 * data$size * .pt, "bigpts"), "npc", TRUE)
    data$y <- grid:::convertHeight(unit(0.98, "npc") - lineheight.npc * unit((data$group - 1), "npc"), "npc", TRUE)
    data$vjust <- 1

    grid:::textGrob(
      data$label,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = grid:::gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)

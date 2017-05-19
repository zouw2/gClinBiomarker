geom_text_table <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., parse = FALSE, nudge_x = 0,
                            nudge_y = 0, check_overlap = FALSE, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(data = data, mapping = mapping, stat = stat, geom = GeomTextTable,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(parse = parse, check_overlap = check_overlap,
                  na.rm = na.rm, ...))
}

GeomTextTable <- ggproto("GeomTextTable", Geom,
  required_aes = c("x", "y", "label"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {

    lab <- data$label
    if (parse) lab <- parse(text = as.character(lab))

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) data$vjust <- compute_just(data$vjust, data$y)
    if (is.character(data$hjust)) data$hjust <- compute_just(data$hjust, data$x)

    data$group[data$group == -1] <- 1
    # data$y <- data$y + unit(data$vjust * data$group, "lines")
    data$vjust <- (data$group - max(data$group) * 0.5) * data$lineheight +
      (2 * (0.5 - data$vjust)) * max(data$group) * 0.5 * data$lineheight

    grid::textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = grid::gpar(
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

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

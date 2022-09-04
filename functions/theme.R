#' @export
theme_piers <- function() {
  box::use(ggplot2[...])
  thm <- theme_minimal(base_family = "serif") %+replace%
    theme(text = element_text(colour = "#373737", size = 14),
          plot.title = element_text(size = "20", hjust = 0, vjust = 3.2, face = "bold"),
          plot.subtitle = element_text(hjust = 0, vjust = 3.5),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, size = 10),
          legend.position = "right",
          axis.text = element_text(face = "plain"),
          panel.grid.major = element_line(linetype = "dashed"),
          panel.grid.minor = element_blank(),
          plot.margin = margin(11, 15, 5, 10),
          axis.line.x = element_line(colour = "#8e8e8e"),
          axis.ticks = element_line(colour = "#8e8e8e"))

  return(thm)
}

#' @export
pal_piers <- function(alpha) {
  piers_palette <- c(
    "midnightBlue" = "#393A76",
    "redNCS" = "#BE0E34",
    "seaGreen" = "#129158",
    "orange" = "#cc700e",
    "pewterBlue" = "#88A2AA",
    "voiletRed" = "#F0569B",
    "uranianBlue" = "#C2EAFF"
  )
  if (alpha > 1 | alpha <= 0)
    stop("alpha must be in (0, 1]")
  raw_cols = piers_palette
  raw_cols_rgb = grDevices::col2rgb(raw_cols)
  alpha_cols = grDevices::rgb(raw_cols_rgb[1, ], raw_cols_rgb[2, ],
                              raw_cols_rgb[3, ], alpha = alpha * 255, names = names(raw_cols),
                              maxColorValue = 255)

  scales::manual_pal(unname(alpha_cols))
}


#' @export
scale_fill_piers <- function(alpha = 1, ...) {
  ggplot2::discrete_scale("fill", "piers", pal_piers(alpha), ...)
}


#' @export
scale_colour_piers <- function(alpha = 1, ...) {
  ggplot2::discrete_scale("color", "piers", pal_piers(alpha), ...)
}


#' @export
pal_ons <- function(alpha) {
  ons_palette <- c(
    "DarkBlue" = "#12436D",
    "Orange" = "#F46A25",
    "Turquoise" = "#28A197",
    "DarkPink" = "#801650",
    "darkGrey" = "#3D3D3D",
    "lightPurple" = "#A285D1"
  )
  if (alpha > 1 | alpha <= 0)
    stop("alpha must be in (0, 1]")
  raw_cols = ons_palette
  raw_cols_rgb = grDevices::col2rgb(raw_cols)
  alpha_cols = grDevices::rgb(raw_cols_rgb[1, ], raw_cols_rgb[2, ],
                              raw_cols_rgb[3, ], alpha = alpha * 255, names = names(raw_cols),
                              maxColorValue = 255)

  scales::manual_pal(unname(alpha_cols))
}

#' @export
scale_fill_ons <- function(alpha = 1, ...) {
  ggplot2::discrete_scale("fill", "ons", pal_ons(alpha), ...)
}


#' @export
scale_colour_ons <- function(alpha = 1, ...) {
  ggplot2::discrete_scale("color", "ons", pal_ons(alpha), ...)
}





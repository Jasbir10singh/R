#----------------------------------------------------------------------- LIBRARY
#  In order to run the following code, we need the %>% operator.
library(dplyr)

#--------------------------------------------------------- DEFINING THE NEW GEOM
#  Defines a new ggproto subclass:
GeomHurricane <- ggplot2::ggproto(`_class`     = "GeomHurricane",
                                  `_inherit`   = ggplot2::Geom,
                                  required_aes = c("x", "y",
                                                   "r_ne", "r_nw",
                                                   "r_se", "r_sw",
                                                   "fill", "colour"),
                                  default_aes = ggplot2::aes(scale_radii = 1,
                                                             alpha       = 0.7,
                                                             linetype    = 1,
                                                             size        = 0.7),
                                  draw_key    = ggplot2::draw_key_polygon,
                                  #  In contrast of the "draw_panel", "draw_group" creates a grob per group
                                  #  (i.e., it is like an implicit loop for each group that creates a grob in
                                  #  each iteration); however, I decided to use the former because I realize
                                  #  that I can generate various grobs in just one call through the "id"
                                  #  argument of the polygonGrob function.
                                  draw_panel  = function(data, panel_scales, coord){
                                    #  Length of the data (we assume 3 rows per observation).
                                    n <- nrow(data)
                                    #  Number of points used to generate each quadrant.
                                    n_smooth <- 91
                                    #  Stores the scale of the wind radius.
                                    radius_scale <- data[1, c("scale_radii")]
                                    data <- data %>%
                                      dplyr::select(colour:y, scale_radii,
                                                    alpha, linetype, size,
                                                    #  These are automatic created variables.
                                                    PANEL, group,
                                                    #  The order matters for these variables.
                                                    r_ne, r_se, r_sw, r_nw) %>%
                                      tidyr::gather(quadrant, radius_nm, r_ne:r_nw)
                                    data <- data[rep(1:(n * 4), each = n_smooth), ] %>%
                                      #  Because the function "destPoint" deems that the distance is
                                      #  given in meters, we need to convert the radius from nautical
                                      #  miles to meters.  A nautical mile is defined as 1,852 meters:
                                      #  https://en.wikipedia.org/wiki/Nautical_mile
                                      dplyr::mutate(radius_m = radius_nm * 1852 * scale_radii)
                                    data <- data %>%
                                      dplyr::bind_cols(
                                        #  Creates the points (lon and lat) that define the polygons for each
                                        #  quadrant, starting from northeast to northwest (i.e., clockwise):
                                        as.data.frame(geosphere::destPoint(as.matrix(data[ ,c("x", "y")]),
                                                                           #  ne quadrant
                                                                           c(rep(seq(  0,  90, length.out = n_smooth), n),
                                                                             #  se quadrant
                                                                             rep(seq( 90, 180, length.out = n_smooth), n),
                                                                             #  sw quadrant
                                                                             rep(seq(180, 270, length.out = n_smooth), n),
                                                                             #  nw quadrant
                                                                             rep(seq(270, 360, length.out = n_smooth), n)
                                                                           ),
                                                                           data$radius_m)
                                        )
                                      ) %>%
                                      dplyr::select(-x, -y, radius_nm) %>%
                                      #  Because the transform method of the coord object converts the named
                                      #  columns "x" and "y", we must rename the points that make the quadrants.
                                      dplyr::rename(x = lon, y = lat) %>%
                                      dplyr::group_by(group) %>%
                                      dplyr::arrange(group)
                                    
                                    coords <- coord$transform(data, panel_scales)
                                    #  A grob can only have a single colour, fill, etc, so we have to obtain
                                    #  the graphical parameters for each group (n in total).
                                    gp_values <- coords %>%
                                      group_by(group) %>%
                                      select(group, colour, fill, alpha, size, linetype) %>%
                                      distinct()
                                    #  Creates the grob for the hurricane data (i.e. the polygons).
                                    hurricane_grob <- grid::polygonGrob(
                                      coords$x, coords$y,
                                      #  This parameter allows to create a polygon for each group in just one
                                      #  call.
                                      id = coords$group,
                                      gp = grid::gpar(col  = gp_values$colour,
                                                      fill = scales::alpha(gp_values$fill, gp_values$alpha),
                                                      # Through the .pt, we can translate the size, given in
                                                      # mm, to the units that grid uses internally for lwd.
                                                      lwd  = gp_values$size * ggplot2::.pt,
                                                      lty  = gp_values$linetype)
                                    )
                                    #  Creates a grob with the radius scale (i.e., a label).
                                    radius_scale_grob <- grid::textGrob(
                                      paste0("Radius scale: ", radius_scale),
                                      #  Puts the label at the bottom right corner
                                      #  with a horizontal space of 1 mm and a
                                      #  vertical space of 1.5 mm + height of the
                                      #  letter "R" from the vertex.
                                      x = grid::unit(1, "npc") - grid::unit(1, "mm"),
                                      y = grid::unit(0, "npc")
                                      + grid::unit(1.5, "mm")
                                      + grid::grobHeight(grid::textGrob("R")),
                                      just = c("right", "bottom"),
                                      gp = grid::gpar(col = "red", cex = 0.85)
                                    )
                                    #  Creates a grob with the number of observations (i.e., a label).
                                    obs_grob <- grid::textGrob(
                                      paste0("Observations: ", n / 3),
                                      #  Puts the label at the bottom right corner
                                      #  with a space of 1 mm from the vertex.
                                      x = grid::unit(1, "npc") - grid::unit(1, "mm"),
                                      y = grid::unit(0, "npc") + grid::unit(1, "mm"),
                                      just = c("right", "bottom"),
                                      gp = grid::gpar(col = "red", cex = 0.85)
                                    )
                                    #  Returns the polygons with the text labels.
                                    grid::gList(hurricane_grob, radius_scale_grob, obs_grob)
                                  }
)
#  Defines the new geom:
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#-------------------------------------------------- GETTING AND TIDYING THE DATA
#  Read the data:
#  I have used the code of the course's book to read a fixed width text file,
#  which is the same in the prompt.
#    Set ups the width of each column.
column_widths <- c(7, 10, 2, 2,
                   3,  5, 5, 6,
                   4,  5, 4,
                   4,  5, 3,
                   4,  3, 3, 3,
                   4,  3, 3, 3,
                   4,  3, 3, 3,
                   2,  6, 1)
#    Defines the column names.
column_names <- c("storm_id", "storm_name", "month", "day",
                  "hour", "year", "latitude", "longitude",
                  "max_wind", "min_pressure", "rad_max_wind",
                  "eye_diameter", "pressure_1", "pressure_2",
                  paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                  paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                  paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                  "storm_type", "distance_to_land", "final")
#    Reads the file with the above specifications from a Web page of "The
#    Tropical Cyclone Extended Best Track Dataset".
hurricane_data <- readr::read_fwf(file = paste0("http://",
                                                "rammb.cira.colostate.edu/",
                                                "research/",
                                                "tropical_cyclones/",
                                                "tc_extended_best_track_dataset/",
                                                "data/",
                                                "ebtrk_atlc_1988_2015.txt"),
                                  col_positions = readr::fwf_widths(column_widths,
                                                                    column_names),
                                  na            = "-99")
#.......................................................................... N.B.
# The reported longitude has a range from 0 to 360; we need to convert it:
#   Reported            Transformed
#   0                 = 0
#   360               = 0
#   lim
#   longitude -> +180 = -180
#   lim
#   longitude -> -180 = 180
# Please check the plot below:
#   plot(function(x) 180 - (x - 180) %% 360,
#        from = 0, to = 360,
#        xlab = "Reported", ylab = "Converted", main = "Longitude")
# We want that our coordinate system looks like this:
#http://www.mobilefish.com/popupwindow/distance_calculator_help_all.php?help=lon
#...............................................................................
#    Tidies up the data.
hurricane_tidy <- hurricane_data %>%
  dplyr::mutate(storm_id  = paste0(
    stringr::str_to_title(tolower(storm_name)),
    "-",
    year),
    longitude = 180 - (longitude + 180) %% 360) %>%
  tidyr::unite(date, year, month, day, hour) %>%
  dplyr::mutate(date = lubridate::ymd_h(date)) %>%
  dplyr::select(storm_id,
                date,
                latitude, longitude,
                dplyr::starts_with("radius")) %>%
  tidyr::gather(variable_name,
                variable_value,
                dplyr::starts_with("radius")) %>%
  tidyr::separate(variable_name,
                  c("sufix", "wind_speed", "quadrant"),
                  "_") %>%
  dplyr::select(-sufix) %>%
  tidyr::spread(quadrant, variable_value)

#-------------------------------------------------------------- TESTING NEW GEOM
#  Obtains an observation of the Hurricane Ike for a particular latitude (25.8).
storm_observation <- hurricane_tidy %>%
  dplyr::filter(storm_id == "Ike-2008",
                latitude == 25.8) %>%
  dplyr::arrange(date, wind_speed) %>%
  dplyr::mutate(id = 1:n())
#  Gets a map near the United States and plots the observation of the Hurricane
#  Ike obtained above using the hurricane geom.
ggmap::get_map(location = c(lon = -88.9, lat = 25.8),
               zoom = 6,
               maptype = "toner-background") %>%
  ggmap::ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 ggplot2::aes(x = longitude, y = latitude,
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              fill = wind_speed, color = wind_speed,
                              group = id)) +
  ggplot2::scale_color_manual(name   = "Wind speed (kts)",
                              values = c("red", "orange", "yellow")) +
  ggplot2::scale_fill_manual(name   = "Wind speed (kts)",
                             values = c("red", "orange", "yellow"))
ggplot2::ggsave("./../_GRAPHS/submitted_image.png",
                width = 25, height = 20, units = "cm",
                dpi = 500)

#  Obtains the whole path of the Hurricane Ike for a particular hour (18).
storm_observation <- hurricane_tidy %>%
  dplyr::filter(storm_id              == "Ike-2008",
                lubridate::hour(date) == 18) %>%
  dplyr::arrange(date, wind_speed) %>%
  dplyr::mutate(id = 1:n())
#  Gets a map near the United States and plots the path of the Hurricane Ike
#  for the observations obtained above using the hurricane geom.
ggmap::get_map(location = c(lon = -70, lat = 30),
               zoom = 3,
               maptype = "toner-background") %>%
  ggmap::ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 ggplot2::aes(x = longitude, y = latitude,
                              r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                              fill = wind_speed, color = wind_speed,
                              group = id)) +
  ggplot2::scale_color_manual(name   = "Wind speed (kts)",
                              values = c("red", "orange", "yellow")) +
  ggplot2::scale_fill_manual(name   = "Wind speed (kts)",
                             values = c("red", "orange", "yellow"))
ggplot2::ggsave("./../_GRAPHS/Ike_path.png",
                width = 25, height = 20, units = "cm",
                dpi = 500)
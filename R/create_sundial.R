
globalVariables(c('Z', 'd', 'hour', 'theta', 'month', 'x', 'y',
                  'labelx', 'labely', 'label', 'half',
                  'city_name', 'country_name', 'oclock'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an analemmatic sundial
#'
#' @param latitude degrees
#' @param magnetic_declination degrees. Positive is EAST.
#'
#' @return ggplot object
#'
#' @import dplyr
#' @import ggplot2
#' @import ggforce
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_sundial <- function(latitude, magnetic_declination) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # degrees <-> radians helper functions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  d2r <- function(d) d * pi/180
  r2d <- function(r) r * 180/pi

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert args to radians
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  phi                  <- d2r(latitude)
  magnetic_declination <- d2r(magnetic_declination)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ellipse major/minor axis
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  M <- 1
  m <- M * sin(phi)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the hour markers
  # - Every quarter hour
  # - set 'oclock' to TRUE for the hour markers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hours_df <- tibble(
    hour   = seq(-9, 9, 0.25),
    theta  = d2r(15) * hour,
    x      = M * sin(theta),
    y      = M * sin(phi) * cos(theta),

    label  = hour + 12,
    labelx = 1.05 * M * sin(theta),
    labely = 1.05 * M * sin(phi) * cos(theta),

    oclock = hour == as.integer(hour)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # near the equator. compensate labels for squashed ellipse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (abs(latitude) < 10) {
    hours_df <- hours_df %>% mutate(
      labely = case_when(
        abs(labely) < 0.0001 ~ labely,
        labely < 0           ~ labely - 0.03,
        labely > 0           ~ labely + 0.03
      )
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup the sun declination for the gnommon positioning
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sun_declination <- tibble(
    half  =     c(    1,      1,      1,     1,     1,     1,    -1,    -1,    -1,    -1,     -1,    -1  ),
    month =     c( 'Jan',  'Feb',  'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',  'Nov', 'Dec' ),
    d     = d2r(c(-23.13, -17.30, -8.00 ,  4.25, 15.00, 22.00, 23.00, 18.00,  8.50, -2.90, -14.00, -21.70)),
    Z     = M * tan(d) * cos(phi)
  )

  sun_declination <- sun_declination %>% mutate(
    month = paste(1, month)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # How to plot some orientation lines
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  alpha <- 0.3

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set up the canvas
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- ggplot()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ellipse + Hour Marks + Labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- p +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = M, b = m, angle = 0), colour = "grey60") +
    geom_point(data = filter(hours_df, oclock), aes(x, y)) +
    geom_text (data = filter(hours_df, oclock), aes(labelx, labely, label = label))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Quarter hour markers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- p +
    geom_point(data = filter(hours_df, !oclock), aes(x, y), size = 0.75)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Gnomon Positioning - month positions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- p +
    geom_text   (data = sun_declination, aes(x = half * 0.075, y = Z, label = month, hjust = ifelse(half < 0, 1, 0))) +
    geom_segment(data = sun_declination, aes(x = half * 0.075, y = Z, yend = Z), xend = 0, alpha = alpha) +
    geom_point  (data = sun_declination, aes(y = Z), x = 0, size = 0.5)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Gnomon Positioning - Vertical line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- p +
    annotate('segment',
             x        = 0,
             xend     = 0,
             y        = min(sun_declination$Z),
             yend     = max(sun_declination$Z),
             alpha    = 0.25,
             linetype = 3)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Magnetic Declination Line + Label
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mag    <- pi/2 - magnetic_declination
  len    <-  0.2
  xend   <- -0.95
  yend   <-  max(c(abs(m), sun_declination$Z))
  xstart <-  xend -     len * cos(mag)
  ystart <-  yend - abs(len * sin(mag))

  p <- p +
    annotate('segment',
             x     = xstart,
             y     = ystart,
             xend  = xend,
             yend  = yend,
             arrow = arrow(angle = 15, type = 'closed'),
             alpha = alpha) +
    annotate("text",
             x     = xstart,
             y     = ystart,
             label = "Mag. North",
             angle = r2d(mag),
             hjust = -0.2,
             vjust = -0.9,
             alpha = alpha)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Style it
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p +
    coord_fixed() +
    theme_void()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an analemmatic sundial for a given city
#'
#' Create an analemmatic sundial for a given city
#'
#' Create an analemmatic sundial for a given city.  The country name may be
#' specified, otherwise the first country it is within is chosen.  See
#' \code{analemmatic:::cities} for the raw city information.
#'
#' The Lat/Long for \code{cities} is from \code{maps::world.cities}.  The
#' magnetic declination has been calculated offline for each city using
#' \code{oce::magneticField()}.
#'
#' @param city city name
#' @param country default: NULL
#' @param ... arguments passed to \code{create_sundial}
#'
#' @return ggplot object
#'
#' @import dplyr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_sundial_for_city <- function(city, country = NULL, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Filter to find the city the user requested
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_city <- cities %>%
    filter(tolower(city_name) == tolower(city))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If a country was specified, then filter by that as well
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(country)) {
    this_city <- this_city %>%
      filter(tolower(country_name) == tolower(country))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we end up with more than 1 city, then tell the user which countries
  # this city is in, but then just pick the first one.
  # Because the 'cities' data is sorted by capital cities first, and then by
  # largest population, this will likely grab the most popular cities people
  # might try
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nrow(this_city) > 1) {
    message("'", city, "' in multiple countries. Choosing first of: ", paste(this_city$country_name, collapse = ", "))
    this_city <- this_city[1, ]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we end up with no cities then complain loudly
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nrow(this_city) == 0) {
    stop("No such city/country: ", paste(c(city, country), collapse = ", "))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use the city's latitude and magnetic declination to create a sundial
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  create_sundial(this_city$lat, this_city$declination, ...) +
    labs(title = paste(this_city$city_name, this_city$country_name, sep = ", "))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(dplyr)
  library(ggplot2)
  library(ggforce)

  # bath, england.   51.3, -2.38
  create_sundial(latitude = 51.3, magnetic_declination = -0.9)
  create_sundial_for_city('Bath', country = 'UK')

  # brisbane, australia
  create_sundial(latitude = -27.5, magnetic_declination = 11)
  create_sundial_for_city('Brisbane')

  # a set of cities at different latitudes
  for (city in c('Singapore', 'Abuja', 'Brisbane', 'Wellington')) {
    p <- create_sundial_for_city(city)
    ggsave(glue::glue("working/{city}.pdf"), plot=p, width = 10, height = 10)
  }

}





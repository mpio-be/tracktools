#' argos_prepare
#' prepare Argos data
#' @param dat data as returned from an ARGOS table
#' @param sf when TRUE returns a sf. Default to FALSE.
#' @param remove_missing_xy when TRUE (default) removes the missing lat, lon.
#' @export
#' @examples
#' d = dbq(q = "Select * from ARGOS.2018_PESA where tagID ='52753' ")
#' z = argos_prepare(d)
argos_prepare <- function(dat, sf = FALSE, remove_missing_xy = TRUE) {
  x = copy(dat)
  x[, locationClass := factor(locationClass, levels = c("Z", "B", "A", 0:3), ordered = TRUE)]
  x[, tagID := as.character(tagID)]
  setorder(x, tagID, locationDate, locationClass)

  # remove missing coordinates
  if(remove_missing_xy)
    x = x[!is.na(latitude)][!is.na(longitude)]


  x[, dpl := duplicated(x, by = c("tagID", "locationDate", "locationClass"))]
  x <- x[!(dpl)]
  # remove bad duplicated loc classes
  x[, dpl := duplicated(x, by = c("tagID", "locationDate"), fromLast = TRUE)]
  x <- x[!(dpl)]
  x[, dpl := NULL]

  if (sf) {
    x = st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  }

  x

}

#' sf points to lines
#' Convert a sf point object to line.
#' @export
st_points2lines <- function(x, grp) {

  if (!missing(grp)) {
    o = x |>
      dplyr::group_by(.data[[grp]]) |>
      dplyr::summarise(do_union = FALSE, .groups = "keep") |>
      st_cast("LINESTRING")
  } else {
    o = dplyr::summarise(x, do_union = FALSE) |> st_cast("LINESTRING")
  }

  o
}


#' speed along points
#' compute speed along locations by reference 
#' @param .lat lat (default to ARGOS tables nomenclature)
#' @param .lon lon (default to ARGOS tables nomenclature)
#' @param .dt locationDate (default to ARGOS tables nomenclature)
#' @param .grp tagID (default to ARGOS tables nomenclature)
#' @param .measure measure pass to geodist
#' @param clean keep .dst and .deltaT?, default to FALSE
#' @return a new colum in x: speed_kmh

#' Compute speed on a DT that has lat, lon and datetime
#' @examples
#' #' d = dbq(q = "Select * from ARGOS.2018_PESA where tagID ='52753' ")
#' z = argos_prepare(d)
#' speed_along(z)
#' @export
speed_along <- function(x,
                        .lat = "latitude",
                        .lon = "longitude",
                        .dt = "locationDate",
                        .grp = "tagID",
                        .measure = "haversine",
                        clean = TRUE,
                        ...) {
  setnames(x, c(.lat, .lon, .dt, .grp), c(".lat", ".lon", ".dt", ".grp"))
  setorder(x, .dt, .grp)
  x[, .deltaT := difftime(.dt,
    shift(.dt, type = "lag"),
    units = "hour"
  ), by = .(.grp)]

  x[, .dst := geodist::geodist(cbind(.lat, .lon), sequential = TRUE, pad = TRUE, measure = .measure),
    by = .grp
  ]

  x[, speed_kmh := (.dst / 1000) / (.deltaT %>% as.numeric())]
  x[speed_kmh == Inf, speed_kmh := NA]
  x[speed_kmh == -Inf, speed_kmh := NA]

  setnames(x, c(".lat", ".lon", ".dt", ".grp"), c(.lat, .lon, .dt, .grp))

  if (clean) {
    x[, ":="(.dst = NULL, .deltaT = NULL)]
  }
}

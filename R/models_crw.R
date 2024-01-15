
#' makefrm
#' make predictive frame based on time
#' a DT passed to aniMotum::fit_ssm time.step
#' @export
#' @examples
#' x = dbq(q = "Select * from ARGOS.2019_PESA where tagID = 66673 limit 500") |>
#'  argos_prepare() |>
#'  fitcrw() |>
#'  st_points2lines("id")


makefrm <- function(d, bytime = "15 mins", byid = "id") {
  d[, .(date = seq(
    min(date) |> as.Date() |> as.POSIXct(),
    max(date) |> as.Date() |> as.POSIXct(),
    by = bytime
  )), by = byid]
}

#' fitcrw
#' @param x a DT as obtained with argos_prepare
#' A wrapper around aniMotum::fit_ssm
#' @export
fitcrw <- function(x, frm = makefrm(x), ...) {

    setnames(
      x,
      c("tagID", "locationDate", "locationClass", "longitude", "latitude"),
      c("id", "date", "lc", "lon", "lat")
    )


    fm = fit_ssm(x,
      model = "crw",
      spdf = FALSE,
      time.step = frm,
      fit.to.subset = FALSE,
      ...
    )
    grab(fm, what = "predicted", as_sf = TRUE) |>
      st_transform(4326)


}

#' @export
bbox_crw <- function(x) {

  st_geometry(x) |>
    st_cast("MULTIPOINT") |>
    st_bbox() |>
    st_as_sfc(crs= st_crs(x))

}

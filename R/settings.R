

#' Barrow.ll
#' @export
Barrow.ll <- function() {
   
    st_as_sf(data.frame(
        lon = 71.3205457, 
        lat = - 156.6669073),
      coords = c( "lat", "lon"), 
      crs = 4326
    )
  
}


#' proj4 string by project
#' @param lon0 default to Barrow.
#' @export
proj4.pesa <- function(lon_0 = {Barrow.ll() |> st_coordinates()}[1] ) {

  glue("+proj=laea +lat_0=0 +lon_0={lon_0} +x_0=0 +y_0=0 +units=km")

}


#' proj4 string by project
#' @export
proj4.btgo <- function(which_proj) {
  "+proj=nzmg"
}
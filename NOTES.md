

## Pan and zoom protocol



## unused snippets 
```r
 # good proj for pesa: 
 proj4.pesa(-70)
``` 

```r
  # bbox by leg
  leg_domains <-
    group_by(o, leg) |>
    tidyr::nest() |>
    mutate(bbox = purrr::map(data, function(x) {
      z = sf::st_as_sfc(sf::st_bbox(x))
      prepare_canvas(baselayer, limitTo = z)
    }))

prepare_canvas <- function(x, proj, limitTo, expandLimit) {
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(TRUE))

  y <- st_bbox(limitTo) |>
    st_as_sfc()

  if (!missing(expandLimit)) {
    sf_use_s2(TRUE)
    y <- st_buffer(y, dist = expandLimit)
    sf_use_s2(FALSE)
  }

  o <- st_crop(x, y)

  if (!missing(proj)) {
    o <- st_transform(o, proj)
  }

  o
}


```    
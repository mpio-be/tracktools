
#! PACKAGES, SETTINGS, FUNCTIONS
  sapply(
    c(
      "dbo", "glue","stringr",
      "dplyr", 
      "sf", "rnaturalearth"
    ),
    FUN = require, character.only = TRUE, quietly = TRUE
  )
  
  # https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
  path_wwfEcoregions = "wwf_terr_ecos.shp"


  extract_major_breeding_ecoregions <- function(br_x, Prop, isNotEcor) {
    x <- st_intersection(e, br_x)
    abr <- st_area(br_x) %>%
      as.numeric() %>%
      sum()

    x$A <- st_area(x)
    setDT(x)
    z <- x[, .(A = sum(A)), by = ECO_NAME]

    # z[, cum_prop := as.numeric( cumsum(A)/sum(A) ) ]
    z[, prop := as.numeric(A) / abr]
    print(z[prop > Prop]$ECO_NAME)

    o <- e[e$ECO_NAME %in% z[prop > Prop, ECO_NAME], ]
    o <- select(o, ECO_NAME)

    if (!missing(isNotEcor)) {
      o <- filter(o, str_detect(ECO_NAME, isNotEcor, negate = TRUE))
    }

    o
  }



#+ Breeding ranges
  con = dbcon(db = "AVES_ranges")
  br = dbq(con, "SELECT scinam, SHAPE
                      FROM breeding_ranges_v2 WHERE
                       scinam = 'calidris melanotos' ",  geom = "SHAPE")
  st_crs(br) = 4326

#+ Ecoregions
  e = st_read(path_wwfEcoregions)
  # exclude inhabitable habitat
  e = filter(e, !ECO_NAME %in% c("Rock and Ice", "Wrangel Island arctic desert"))
  # exclude non-wader habitat
  e = filter(e, !ECO_NAME %like% "desert")
  e = st_make_valid(e)

  brecor = extract_major_breeding_ecoregions(
    br,
    Prop = 0.01, 
    isNotEcor = c("taiga")
  )

  #' mapview::mapview(brecor, zcol = "ECO_NAME") + mapview::mapview(br)


  # EXPORT
  pesa_breeding_range = br
  pesa_breeding_ecoreg = brecor
  
  usethis::use_data(pesa_breeding_range)
  usethis::use_data(pesa_breeding_ecoreg)
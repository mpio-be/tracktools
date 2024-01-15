
#! PACKAGES, SETTINGS, FUNCTIONS
  sapply(
    c(
      "glue","stringr",
      "dplyr", 
      "sf", "rnaturalearth"
    ),
    FUN = require, character.only = TRUE, quietly = TRUE
  )
  




#+ World
  w = ne_countries(returnclass = "sf", scale = "large") |> 
      select(continent, name) |>
      filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))

  #' mapview::mapview(w, zcol = "name")


  # EXPORT
  world = w
  usethis::use_data(world, overwrite = TRUE)

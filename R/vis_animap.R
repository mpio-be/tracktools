
#' @export
alphaAlong <- function(x, head = 20, skew = -2) {
  if (head >= length(x)) head <- as.integer(length(x) * 0.5)

  x <- as.numeric(x)
  he <- scales::rescale(x[(length(x) - head):length(x)], c(skew, 0)) |> exp()
  ta <- rep(min(he), length.out = length(x) - head - 1)
  c(ta, he)
}

#' @export
sizeAlong <- function(x, head = 20, to = c(1, 3)) {
  if (head >= length(x)) head <- as.integer(length(x) * 0.5)

  x <- as.numeric(x)
  he <- scales::rescale(x[(length(x) - head):length(x)], to)
  ta <- rep(min(he), length.out = length(x) - head - 1)
  c(ta, he)
}


# plan segments (see interactive.R)

#' prepare_segments.temporal
#' @param x default to `triplegs`, the object output of plan_segments()
#' @param crsPreds predictions from a crw model.
#' once marked by plan_segments(), segment start and stop are prepared
prepare_segments.temporal <- function(x = triplegs, crsPreds) {
  x <- x |>
    st_drop_geometry() |>
    select(date) |>
    mutate(leg = dplyr::row_number())

  o <- left_join(crsPreds |> st_drop_geometry(), x, join_by(date))
  o = tidyr::fill(o, leg, .direction = "up")

  mutate(o, leg = coalesce(leg, max(leg, na.rm = TRUE) + 1)) |>
    group_by(leg) |>
    summarise(sta = min(date), sto = max(date))
}

#' prepare segments.spatial
#'  @param x a tibble from prepare_segments.temporal()
#' @param polys a list of polygons of length nrow(x)
#' @examples
#' x = prepare_segments.temporal(crsPreds = m)
#' p1 <- p5 <- pesa_breeding_range
#' p2 <- p4 <- filter(world, str_detect(continent, "America"))
#' p3 <- filter(world, continent == 'South America')
# TODO
prepare_segments.spatal <- function(x, polys) {

}


#' plan frames
#' After segments were planed and defined, frames (date and file path to each frame) are defined too. 
#' @export
plan_frames <- function(sta, sto, By = "20 mins", framedir = paste0(tempdir(), "/frames/")) {
  message("framedir is ", framedir |> shQuote())

  ST <- data.table(date = seq(sta, sto, by = By))
  ST[, path := paste0(framedir, str_pad(1:.N, 5, "left", pad = "0"), ".png")]
  ST[, i := .I]
  ST
}


#' make_frames
#' make_frames
#' @export
#' @examples
#' x = dbq(q = "Select * from ARGOS.2019_PESA where tagID = 66673") |> argos_prepare()
#' flagpts(x)
#' z = x[!(flag)]

#' m = fitcrw(z) |> sf::st_transform( proj4.pesa(-70) )
#' w = st_transform( dplyr::filter(world, continent != "Asia") ,  proj4.pesa(-70) )
#' ff = plan_frames(sta = min(m$date), sto = max(m$date),By = "30 mins")
#' out = make_frames(x = m, frms = ff, baselayer = w)
#' make_moviemap(out)
make_frames <- function(x, frms, baselayer, BBOX = bbox_crw(x)) {
  

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores)
  doSNOW::registerDoSNOW(cl)

  pb <- txtProgressBar(max = nrow(frms), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)


  foreach(i = 1:nrow(frms), 
  .inorder = FALSE, 
  .packages = c("tracktools", "dplyr", "sf", "ggplot2", "stringr", "ggspatial"),
  .options.snow = opts) %dopar% {

    xi <- filter(x, date <= frms[i, date]) |>
      mutate(
        a  = alphaAlong(date, head = 70, skew = -0.2),
        s  = sizeAlong(date, head = 70, to = c(0.2, 3))
        )
    xi = mutate(xi, cl = colorRampPalette(c("#99998a", "red"))(nrow(xi)))
    xi = bind_cols(xi, st_coordinates(xi))



    gi =
      ggplot() +
      layer_spatial(BBOX, color = "transparent", fill = "transparent") +
      annotation_spatial(baselayer, color = "transparent", fill = "#adbea6") +
      geom_path(data = xi, aes(x = X, y = Y), lineend = "round", col = xi$cl, alpha = xi$a, linewidth = xi$s) +
      annotation_scale(location = "bl", line_width = 0.5) +
      theme_bw() +
      theme(axis.title = element_blank()) +
      annotate("text", x = -Inf, y = Inf, label = format(frms[i]$date, "%B %d %H:00"), vjust = 1, hjust = 0, size = 4)


      invisible(
        
      ggsave(frms[i, path],
        gi,
        device = ragg::agg_png, res = 320, units = "in"
      )
      
      )
  
  
  }

  dirname(frms$path[1])


}


#' @export
make_moviemap <- function(dir, outnam, outdir) {
  #frame rate -r 130 (30 secs)

  if(missing(outnam)) 
   outnam = "animap.mp4"

  if(missing(outdir))
   outdir = getwd()



  call =
    glue('cd {dir} && ffmpeg -r 30 -pattern_type glob -i "*.png" -c:v libx264 -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -pix_fmt yuv420p -preset veryslow -crf 1 {outnam}')

  t0 = Sys.time()

  system(call)

  system(glue("mv {dir}/{outnam} {outdir}"))

  difftime(Sys.time(), t0)


}

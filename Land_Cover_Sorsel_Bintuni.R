# 1. PACKAGES
libs <- c(
  "terra",
  "sf",
  "tidyverse",
  "ggtern",
  "elevatr",
  "png",
  "rayshader",
  "magick",
  "magrittr"
)

installed_libraries <- libs %in% rownames(installed.packages())

if(any(installed_libraries == F)){
  install.packages(libs[!installed_libraries])
}

invisible(lapply(libs, library, character.only = T))

# 2. COUNTRY BORDERS
boundary_shapefile <- "D:/Office/R Script/Batas Administrasi Kabupaten/Sorsel dan Bintuni.shp"

boundary_sf <- sf::st_read(boundary_shapefile)
plot(sf::st_geometry(boundary_sf))

png("boundary_borders.png")
plot(sf::st_geometry(boundary_sf))
dev.off()

# 3. DOWNLOAD ESRI LAND COVER TILES
urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/52M_20210101-20220101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/53M_20210101-20220101.tif"
)

for(url in urls){
  download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
  )
}

# 4. LOAD TILES
raster_files <- list.files(
  path = "C:/Users/Sraharjo/Documents",
  pattern = "20210101-20220101.tif$",
  full.names = TRUE
)

crs_utm53s <- "EPSG:32753"  # WGS 1984 UTM Zone 53S

for(raster in raster_files){
  rasters <- terra::rast(raster)
  
  # Adjust boundary if needed
  boundary <- boundary_sf |>
    sf::st_transform(crs = terra::crs(rasters))
  
  # Crop without masking to retain diversity
  cropped_raster <- terra::crop(rasters, terra::vect(boundary), snap = "in")
  
  # Experiment with a smaller aggregation factor or different method
  land_cover <- terra::aggregate(cropped_raster, fact = 3, fun = "mean") |>
    terra::project(crs_utm53s)
  
  # Save the processed raster
  terra::writeRaster(land_cover, paste0("processed_", basename(raster)))
}

# 5. LOAD VIRTUAL LAYER
r_list <- list.files(
  path = getwd(),
  pattern = "processed_",
  full.names = TRUE
)

land_cover_vrt <- terra::vrt(
  r_list,
  "land_cover_vrt.vrt",
  overwrite = TRUE
)

# 6. FETCH ORIGINAL COLORS
ras <- terra::rast(
  raster_files[[1]]
)

raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)

# 7. ASSIGN COLORS TO RASTER
cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_sorsel_bintuni <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_sorsel_bintuni)

# 8. DIGITAL ELEVATION MODEL
elev <- elevatr::get_elev_raster(
  locations = boundary_sf,
  z = 9, clip = "locations"
)

land_cover_sorsel_bintuni_resampled <- terra::resample(
  x = land_cover_sorsel_bintuni,
  y = terra::rast(elev),
  method = "near"
) |>
  terra::project(crs_utm53s)

terra::plotRGB(land_cover_sorsel_bintuni_resampled)

img_file <- "land_cover_sorsel_bintuni.png"

terra::writeRaster(
  land_cover_sorsel_bintuni_resampled,
  img_file,
  overwrite = TRUE,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
elev_utm53s <- elev |>
  terra::rast() |>
  terra::project(crs_utm53s)

elmat <- rayshader::raster_to_matrix(
  elev_utm53s
)

h <- nrow(elev_utm53s)
w <- ncol(elev_utm53s)

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 12,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 5, h / 5
    ),
    zoom = .5,
    phi = 85,
    theta = 0
  )

rayshader::render_camera(
  zoom = .58
)

# 10. RENDER OBJECT
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

filename <- "3d_land_cover_sorsel_bintuni-dark.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  light = F,
  environment_light = hdri_file,
  intensity_env = 1,
  rotate_env = 90,
  interactive = F,
  parallel = T,
  width = w * 1.5,
  height = h * 1.5
)

# 11. PUT EVERYTHING TOGETHER

# Updated colors with new Rangeland color
cols <- c("#82adee", "#419bdf", "#397d49", "#7a87c6", 
          "#e49635", "#c4281b", "#a59b8f", "#edf3e4", # Updated Rangeland color
          "#616161", "#e3e2c3")

# Legend creation code remains the same
legend(
  "center",
  legend = c(
    "Flooded Vegetation", # Added new class
    "Water",
    "Trees",
    "Crops",
    "Built area",
    "Rangeland"
  ),
  pch = 15,
  cex = 2,
  pt.cex = 1,
  bty = "n",
  col = c(cols[1], cols[2:3], cols[5:6], cols[8:10]),
  fill = c(cols[1], cols[2:3], cols[5:6], cols[8:10]),
  border = "grey20"
)
q
dev.off()

# Combine final image with legend
lc_img <- magick::image_read("path_to_land_cover_image") # Replace with your file path

my_legend <- magick::image_read(legend_name)

my_legend_scaled <- magick::image_scale(
  magick::image_background(
    my_legend, "none"
  ), 2500
)

p <- magick::image_composite(
  magick::image_scale(
    lc_img, "x7000" 
  ),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+100+0"
)

magick::image_write(p, "3d_sorsel_bintuni_land_cover_final.png")

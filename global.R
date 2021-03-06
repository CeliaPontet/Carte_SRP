# 
rm(list=ls())
# 
# options(java.parameters = "-Xss2560k")
# 
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinydashboard)
library(jpeg)
library(glue)
library(sf)
library(DT)
# library(plotly)
library(RColorBrewer)
library(shinyjs)
library(colourpicker)
library(png)
library(grid)
# library(flexdashboard)

library(reshape2)

#devtools::install_github('oswaldosantos/ggsn') ==> installation de ggsn depuis espace github, non déposé sur le CRAN
# library(ggsn)#flèche nord




# library(ggiraph)
# library(grid)
# library(readxl)
# library(maps)
# 
# library(shape)
# library(DT)
# library(ggplot2)
# library(gdata)
# 
# library(reshape2)
# library(GISTools)
# #library(ggthemes)
# library(XLConnect)
# library(dplyr)


data_srp <- readRDS(file = "./DATA/data_srp.rds")
summary(data_srp)
Encoding(data_srp$CULTURE)

ref <- as.tibble(readRDS(file = "./DATA/ref.rds")) %>%
  mutate(CODE_REG=as.factor(CODE_REG))


#Fond de carte département
departements_L93 <- st_read(dsn = "./DEPARTEMENTS", layer = "DEPARTEMENT", quiet = TRUE) %>%
  st_transform(2154)%>%
  full_join(ref,by=c("CODE_DEPT","NOM_DEPT","CODE_REG","NOM_REG")) %>%
  st_as_sf() %>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_dept <- departements_L93

fond_anc_reg<- departements_L93 %>%
  group_by(NOM_ANC_REG) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_region <- departements_L93 %>%
  group_by(NOM_REG) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_ird <- departements_L93 %>%
  group_by(ZONE_IRD) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_ti <- departements_L93 %>%
  group_by(ZONE_TI) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_dept_sanscorse <- departements_L93 %>%
  filter(!NOM_DEPT %in% c("CORSE-DU-SUD","HAUTE-CORSE"))%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_anc_reg_sanscorse <- departements_L93 %>%
  filter(!NOM_DEPT %in% c("CORSE-DU-SUD","HAUTE-CORSE")) %>%
  group_by(NOM_ANC_REG) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_region_sanscorse <- departements_L93 %>%
  filter(!NOM_DEPT %in% c("CORSE-DU-SUD","HAUTE-CORSE")) %>%
  group_by(NOM_REG) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_ird_sanscorse <- departements_L93 %>%
  filter(!NOM_DEPT %in% c("CORSE-DU-SUD","HAUTE-CORSE")) %>%
  group_by(ZONE_IRD) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))

fond_ti_sanscorse <- departements_L93 %>%
  filter(!NOM_DEPT %in% c("CORSE-DU-SUD","HAUTE-CORSE")) %>%
  group_by(ZONE_TI) %>%
  summarize()%>%
  mutate(LON = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         LAT = map_dbl(geometry, ~st_centroid(.x)[[2]]))



ti <- readPNG("./DATA/LogoTI.png")
tu <- readPNG("./DATA/LogoTU.png")



couleurs <- c("gold2","darkolivegreen3","coral2","mediumpurple4","steelblue3","deeppink3","darkgrey","aquamarine3","darkseagreen4","violetred4","lightpink2","lightblue2","yellow3")




#PalettePicker
# List of palettes
colors_pal <- lapply(
  X = split(
    x = brewer.pal.info,
    f = factor(brewer.pal.info$category, labels = c("Diverging", "Qualitative", "Sequential"))
  ),
  FUN = rownames
)

# Get all colors given a palette name(s)
get_brewer_name <- function(name) {
  pals <- brewer.pal.info[rownames(brewer.pal.info) %in% name, ]
  res <- lapply(
    X = seq_len(nrow(pals)),
    FUN = function(i) {
      brewer.pal(n = pals$maxcolors[i], name = rownames(pals)[i])
    }
  )
  unlist(res)
}

background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_name)
head(background_pals, 3)

# Calc linear gradient for CSS
linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))

colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))


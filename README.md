# GIS-con-R
Tutoriales sobre Sistemas de Informaciones Geográficas con R
library(dplyr)
library(ggplot2)
library(ggsn)
library(sf)

#peru1 <- readOGR(dsn = './SHP', layer = 'departamentos')


#Alternativa 2
peru2 <- st_read('./SHP/departamentos.shp')
st_geometry_type(peru2)
st_crs(peru2)
st_bbox(peru2)

cap_dep <- st_read('./SHP/Cap_Dep.shp') %>% 
  filter(Nombre != 'PROV. CONSTITUCIONAL CALLAO')


#pdf("./Mapas/Peru1.pdf", width = 11.7, height = 5)
#plot(peru2)
#dev.off()


color = colorRampPalette(c('#01579B','#E1F5FE'))

p1 <- ggplot() +
  geom_sf(data = peru2, color = 'grey80', fill = color (25))  +
  geom_sf_text(peru2, mapping = aes(label = DEPARTAMEN), colour = "black", size = 2.5) +
  ggtitle('Perú : Departamentos') +
  xlab('') +
  ylab('') +
  north(peru2) +
  scalebar(peru2, dist = 250, dist_unit = "km",
           transform = TRUE, model = "WGS84",
           st.size = 3.5, location = "bottomleft") +
  coord_sf()

pdf("./Mapas/Peru.pdf", width = 8.2, height = 11.7)
p1
dev.off()

color = colorRampPalette(c('darkolivegreen1','springgreen', 'royalblue'))

p2 <- ggplot() +
  geom_sf(data = peru2, color = 'darkolivegreen', fill = color (25))  +
  geom_point(cap_dep, mapping = aes(x = X, y = Y),
             shape = 21, fill= 'khaki1', color= 'black', size=1.5) +
  geom_sf_text(cap_dep, mapping = aes(label = Nombre),
               colour = "black", size = 2, position = position_nudge(y = 0.2)) +
  ggtitle('Perú : Capitales de Departamento') +
  xlab('') +
  ylab('') +
  north(peru2) +
  scalebar(peru2, dist = 250, dist_unit = "km",
           transform = TRUE, model = "WGS84",
           st.size = 3.5, location = "bottomleft") +
  coord_sf()


pdf("./Mapas/Peru2.pdf", width = 8.2, height = 11.7)
p2
dev.off()

#Funcion para multiplot con ggplot2
{multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
}


pdf("./Mapas/Peru3.pdf", width = 11.7, height = 8.2)
multiplot(p1, p2, cols = 2)
dev.off()

#Mapa en JPEG
jpeg("./Mapas/Peru.jpeg", width = 1654, height = 2362, 
     units = "px", pointsize =12, bg = "white",
     res = 300, restoreConsole = TRUE)
p2
dev.off()

#Mapa en JPEG
jpeg("./Mapas/Peru2.jpeg", width = 2362, height = 1654, 
     units = "px", pointsize =12, bg = "white",
     res = 300, restoreConsole = TRUE)
multiplot(p1, p2, cols = 2)
dev.off()


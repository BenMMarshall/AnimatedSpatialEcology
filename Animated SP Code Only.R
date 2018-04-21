
##### CODE ONLY #####
#### AnimatedSpatialEcology: "Animtaed Home Ranges and Movement How-to" ####
### "Benjamin Michael Marshall" ###
## "20/03/2018" ##
# V.0.5 #

install.packages(c("beepr", "animation", "magick", "reshape2", "ggplot2", "ggspatial", 
"dplyr", "sp", "ggmap", "adehabitatHR", "scales", "cowplot"))

library(beepr)
library(animation)
library(magick)
library(reshape2)
library(ggplot2)
library(ggmap)
library(ggspatial)
library(dplyr)
library(sp)
library(adehabitatHR)
library(scales)
library(cowplot)

setwd("Your working directory here")

animaldata <- read.csv("FakeTestDataset.csv", header = TRUE, sep = ",")
head(animaldata)

animaldata <- animaldata[!is.na(animaldata$Easting | animaldata$Northing),]
animaldata <- animaldata[animaldata$Easting > 0,]
animaldata <- animaldata[animaldata$Northing > 0,]

attach(animaldata)  
diffE <- function(animaldata) {Easting[1:(length(Easting)-1)]-
Easting[2:(length(Easting))]}
diffE_list <- sapply(animaldata, diffE)[,1]
diffN <- function(animaldata) {Northing[1:(length(Northing)-1)]-
Northing[2:(length(Northing))]}
diffN_list <- sapply(animaldata, diffN)[,1]
UTMdist <- sqrt((diffE_list^2) + (diffN_list^2))
animaldata$Moves <- c(0, UTMdist)
animaldata$CsumMoves <- cumsum(animaldata$Moves)
animaldata$Tracking_date <- as.Date(animaldata$Tracking_date, format = "%Y-%m-%d")
animaldata$ID <- animaldata$Snake_ID
animaldata$Tracking_time <- animaldata$Tracking_time
detach(animaldata)

utm <- CRS('+init=epsg:23847')
latlong <- CRS("+init=epsg:4326")

coords <- cbind(animaldata$Easting, animaldata$Northing)
SP <- SpatialPoints(coords,proj4string = utm)
MCP <- mcp(SP, percent = 95, unin = "m", unout = "ha")
MCParea <- MCP$area

K <- kernelUD(SP, h = 'href', grid = 800, extent = 1)
K_href_95 <- getverticeshr(K, 95)
kernelarea <- kernel.area(K, percent = 95, unin = "m", unout = "ha")
ylimforHRgraph <- max(cbind(MCParea, kernelarea))
ylimforHRgraph

# K@h$h
# h_forYLimit <- K@h$h + 100
# K <- kernelUD(SP, h = h_forYLimit, grid = 800, extent = 10)
# kernelarea <- kernel.area(K, percent = 95, unin = "m", unout = "ha")
# ylimforHRgraph <- max(cbind(MCParea, kernelarea))
# ylimforHRgraph

SPlatlong <- spTransform(SP, latlong)
K_href_95_latlong <- spTransform(K_href_95, latlong)

location <- c(mean(range(SPlatlong@coords[,1])), mean(range(SPlatlong@coords[,2])))

map <- get_map(location = location, crop = F,
               maptype = "satellite",
               source = "google",
               zoom = 13)

ggmap(map) + geom_point(data = as.data.frame(SPlatlong),
                        aes(x = coords.x1, y = coords.x2),
                        colour = "black", pch = 3, size = 0.2)+
  geom_spatial(data = K_href_95_latlong, aes(),
               fill = "white",  alpha = 0.05, linetype = 1, size = 0.5, colour = "black")


Sys.which('ffmpeg')
ani.options(interval = .3, ani.width = 1920, ani.height = 1080, autoplay = FALSE,
            ffmpeg = "/usr/local/bin/ffmpeg")

areasall <- NULL
saveVideo({
  for (i in 5:length(animaldata$ID)) {
    CurrSet <- animaldata[1:i,]
    coords <- cbind(CurrSet$Easting, CurrSet$Northing)
    SP <- SpatialPoints(coords, proj4string = utm)
    MCP <- mcp(SP, percent = 95, unin = "m", unout = "ha")
    K <- kernelUD(SP, h = "href", grid = 1000, extent = 10)
    K50 <- getverticeshr(K, 50)
    K95 <- getverticeshr(K, 95)
    MCParea <- MCP$area
    kernelareas <- kernel.area(K, percent = c(50, 95), unin = "m", unout = "ha")
    areas <- cbind(MCParea, kernelareas[1], kernelareas[2])
    areas <- as.data.frame(areas)
    areas <- cbind(areas, CurrSet$Tracking_date[length(CurrSet$ID)])
    names(areas) <- c("MCP", "K50", "K95", "Date")
    row.names(areas) <- "Area (ha)"
    areasall <- rbind(areasall, areas)
    SPlatlong <- spTransform(SP, latlong)
    MCPlatlong <- spTransform(MCP, latlong)
    K50latlong <- spTransform(K50, latlong)
    K95latlong <- spTransform(K95, latlong)
    fullmap <- ggmap(map) +
      geom_spatial(data = K95latlong, aes(),
                   fill = "white",  alpha = 0.3, linetype = 1, size = 0.5, colour = "black") +
      geom_spatial(data = K50latlong, aes(),
                   fill = "white",  alpha = 0.3, linetype = 1, size = 0.5, colour = "black") +
      geom_spatial(data = MCPlatlong, aes(),
                   colour="black",  alpha = 0, linetype = 1, size = 1.5) +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-1], xend = coords.x1[i],
                       y = coords.x2[i-1], yend = coords.x2[i]), size = 2, colour = "red4") +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-2], xend = coords.x1[i-1],
                       y = coords.x2[i-2], yend = coords.x2[i-1]), size = 1.5, alpha = 0.5, colour = "red4") +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-3], xend = coords.x1[i-2],
                       y = coords.x2[i-3], yend = coords.x2[i-2]), size = 1, alpha = 0.25, colour = "red4") +
      geom_point(data = as.data.frame(SPlatlong),
                 aes(x = coords.x1, y = coords.x2),
                 colour = "black", pch = 3, size = 2) +
      geom_point(data = as.data.frame(SPlatlong),
                 aes(x = coords.x1[i], y = coords.x2[i]),
                 colour = "red", pch = 16, size = 5) +
      labs(colour = "white", title = CurrSet$ID[1], 
           subtitle = paste(CurrSet$Tracking_date[length(CurrSet$ID)], 
                            CurrSet$Tracking_time[length(CurrSet$ID)]),  
           x = "Longitude", y = "Latitude") +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 3),
            title = element_text(face = 1, size = 40),
            plot.title = element_text(size = 70, face = 2, hjust = 0,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
            plot.subtitle = element_text(face = 3, size = 50,
                                         margin = margin(t = 20, r = 0, b = 20, l = 0)), 
            text=element_text(size = 20, colour = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1, colour="black",
                                       margin = margin(t = 15, r = 0, b = 10, l = 0)), 
            axis.text.y = element_text(angle = 45, hjust = 1, colour = "black",
                                       margin = margin(t = 0, r = 10, b = 0, l = 15)),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
            axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"),
            axis.ticks.length = unit(0.5, "cm")) 
    
    CurrSet$Tracking_date <- as.Date(CurrSet$Tracking_date, format = "%Y-%m-%d")
    
    distanceVtime  <- ggplot(CurrSet) + 
      geom_segment(aes(x = as.Date("1971-01-01", format = "%Y-%m-%d"), xend = max(CurrSet$Tracking_date),
                       y = CurrSet$CsumMoves[length(CurrSet$ID)]/1000,
                       yend = CurrSet$CsumMoves[length(CurrSet$ID)]/1000),
                   size = 0.5, colour = "grey45", alpha = 0.2) + 
      geom_point(aes(x = Tracking_date, y = CsumMoves/1000), pch = 43, size = 1.5) + 
      geom_point(aes(x = Tracking_date[length(CurrSet$ID)], y = CsumMoves[length(CurrSet$ID)]/1000),
                 pch = 21, colour = "red", size = 2.5) + 
      geom_line(aes(x = Tracking_date, y = CsumMoves/1000), alpha = 0.8, size = 2) +
      theme_bw() + 
      coord_cartesian(xlim = c(min(animaldata$Tracking_date), max(animaldata$Tracking_date)),
                      ylim = c(min(animaldata$CsumMoves)/1000, max(animaldata$CsumMoves)/1000)) + 
      scale_x_date(breaks = date_breaks("4 weeks"), 
                   labels = date_format("%d %m %Y")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour="black",
                                       margin = margin(t = 6, r = 0, b = 12, l = 0), size = 15),
            axis.title.x = element_text(size = 30),
            axis.text.y = element_text(angle = 0, hjust = 1,
                                       margin = margin(t = 0, r = 6, b = 0, l = 12), size = 20),
            axis.title.y = element_text(size = 30),
            axis.line = element_line(colour = "black", 
                                     size = 0.5, linetype = "solid"),
            axis.ticks = element_line(colour = "black", size = 1.5, linetype = "solid"),
            axis.ticks.length=unit(0.25, "cm")) + 
      annotate("text", x = as.Date(animaldata$Tracking_date[length(animaldata$ID)]),
               y = max(animaldata$CsumMoves)/10000, size = 20,
               label = paste(round(CurrSet$CsumMoves[length(CurrSet$ID)]/1000, digits = 2), "km"),
               hjust = 1) + 
      scale_y_continuous(breaks = seq(0, max(animaldata$CsumMoves)/1000, 5)) + 
      labs(title = "", x = "Date", y = "Distance Moved (km)")
    
    areasall$Date <- as.Date(areasall$Date, format = "%Y-%m-%d")
    
    meltedareas <- melt(areasall)
    meltedareas[meltedareas$variable == "Date",][,2] <- 
      as.Date(meltedareas[meltedareas$variable == "Date",][,2],
              origin = as.Date("1969-01-01", format = "%Y-%m-%d"))
    meltedareas$Date <- as.Date(meltedareas[meltedareas$variable == "Date",][,2],
                                origin = as.Date("1971-01-01", format = "%Y-%m-%d"))
    meltedareas <- meltedareas[!meltedareas$variable == "Date",]
    
    EstimationAreas <- ggplot(meltedareas) + 
      geom_segment(aes(x = as.Date("1971-01-01", format = "%Y-%m-%d"), xend = max(meltedareas$Date),
                       y = tail(meltedareas[meltedareas$variable == "MCP",],1)[1,2],
                       yend = tail(meltedareas[meltedareas$variable == "MCP",],1)[1,2]),
                   size = 0.5, colour = "grey45", alpha = 0.2) +
      geom_segment(aes(x = as.Date("1971-01-01", format = "%Y-%m-%d"), xend = max(meltedareas$Date),
                       y = tail(meltedareas[meltedareas$variable == "K95",],1)[1,2],
                       yend = tail(meltedareas[meltedareas$variable == "K95",],1)[1,2]),
                   size = 0.5, colour = "grey45", alpha = 0.2) +
      geom_segment(aes(x = as.Date("1971-01-01", format = "%Y-%m-%d"), xend = max(meltedareas$Date),
                       y = tail(meltedareas[meltedareas$variable == "K50",],1)[1,2],
                       yend = tail(meltedareas[meltedareas$variable == "K50",],1)[1,2]),
                   size = 0.5, colour = "grey45", alpha = 0.2) +
      geom_point(aes(x = Date, y = value, colour = as.factor(variable)), size = 1.5) + 
      geom_line(aes(x = Date, y = value, colour = as.factor(variable)), alpha = 0.8, size = 1) + 
      geom_point(aes(x = max(meltedareas$Date), y = tail(meltedareas[meltedareas$variable == "MCP",],1)[1,2]), 
                 pch = 21, colour = "red", size = 3) +
      geom_point(aes(x = max(meltedareas$Date), y = tail(meltedareas[meltedareas$variable == "K95",],1)[1,2]), 
                 pch = 21, colour="red", size = 3) + 
      geom_point(aes(x = max(meltedareas$Date), y = tail(meltedareas[meltedareas$variable == "K50",],1)[1,2]), 
                 pch = 21, colour = "red", size = 3) +
      coord_cartesian(xlim = c(min(animaldata$Tracking_date), max(animaldata$Tracking_date)),
                      ylim = c(min(animaldata$CsumMoves), ylimforHRgraph)) + 
      scale_y_continuous(breaks = seq(0, ylimforHRgraph, round(ylimforHRgraph/10, digits = -1))) + 
      scale_x_date(breaks = date_breaks("4 weeks"), labels = date_format("%d %m %Y")) + 
      guides(color = guide_legend(override.aes = list(size = 3))) +
      
      scale_color_hue(l = 65, c = 100, h = c(130, 250), labels = 
                        c(paste0("MCP: ", round(tail(meltedareas[meltedareas$variable == "MCP",],1)[1,2],
                                                digits = 2), " ha  "),
                          paste0("K50: ", round(tail(meltedareas[meltedareas$variable == "K50",],1)[1,2],
                                                digits = 2), " ha  "),
                          paste0("K95: ", round(tail(meltedareas[meltedareas$variable == "K95",],1)[1,2],
                                                digits = 2), " ha  "))) +
      theme_bw() +
      labs(title = "", 
           x = "", y = "Home Range Area (ha)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour="black",
                                       margin = margin(t = 6, r = 0, b = 12, l = 0), size = 15),
            axis.title.x = element_text(size = 30),
            axis.text.y = element_text(angle = 0, hjust = 1,
                                       margin = margin(t = 0, r = 6, b = 0, l = 12), size = 20),
            axis.title.y = element_text(size=30),
            axis.line = element_line(colour = "black", 
                                     size = 0.5, linetype = "solid"),
            axis.ticks = element_line(colour = "black", size = 1.5, linetype = "solid"),
            axis.ticks.length = unit(0.25,"cm"), 
            legend.text=element_text(size = 25, face = 3),
            legend.title = element_blank(),
            panel.background = element_rect(fill=alpha('white', 0.5),colour = NA), 
            plot.background = element_rect(fill = alpha('white', 0.1),colour = NA),
            legend.background = element_rect(fill=alpha('white', 0),colour = NA),
            legend.key = element_rect(fill=alpha('white', 0),colour = NA),
            legend.position = "top")
    
    Grapharrange <- ggdraw() +
      draw_plot(fullmap, x = -0.25, y = 0., width = 1, height = 1) +
      draw_plot(EstimationAreas, x = 0.52, y = 0.5, width = 0.45, height = 0.5) +
      draw_plot(distanceVtime, x = 0.52, y = 0, width = 0.45, height = 0.5)
    
    print(paste("Datapoint", i,"/",length(animaldata$ID), "Complete"))
    print(paste(CurrSet$ID[1], 
                CurrSet$Tracking_date[length(CurrSet$ID)], 
                CurrSet$Tracking_time[length(CurrSet$ID)]))
    
    print(Grapharrange)
  } 
  beep(2) 
}, video.name = paste0(animaldata$ID[1], "animated.mp4")) 


### Alternative map only plot ----------------

ani.options(interval = .3, ani.width = 1920, ani.height = 1920, autoplay = FALSE,
            ffmpeg = "/usr/local/bin/ffmpeg")

areasall <- NULL

saveVideo({
  for (i in 5:length(animaldata$ID)) {
    CurrSet <- animaldata[1:i,]
    coords <- cbind(CurrSet$Easting, CurrSet$Northing)
    SP <- SpatialPoints(coords,proj4string = utm)
    MCP <- mcp(SP, percent = 95, unin = "m", unout = "ha")
    K <- kernelUD(SP, h = 'href', grid = 1000, extent = 10)
    
    # here's an alternative to simply having a 50 and 95% contour, this will make one every 10%
    # BEWARE - this will make the entire process much much longer
    for(contr in c(seq(20, 90, 10), 95)){
      contr_name <- paste0("K_UTM_", contr)
      K_contr <- getverticeshr(K, contr)
      assign(contr_name, K_contr)
    }
    
    MCParea <- MCP$area
    kernelareas <- kernel.area(K, percent = c(50, 95), unin = "m", unout = "ha")
    areas <- cbind(MCParea, kernelareas[1], kernelareas[2])
    areas <- as.data.frame(areas)
    areas <- cbind(areas, CurrSet$Tracking_date[length(CurrSet$ID)])
    names(areas) <- c("MCP", "K50", "K95", "Date")
    row.names(areas) <- "Area (ha)"
    areasall <- rbind(areasall, areas)
    SPlatlong <- spTransform(SP, latlong)
    MCPlatlong <- spTransform(MCP, latlong)
    
    # This is the corresponding loop to convert all the contours generated into lat long format.
    for(kutm in ls(pattern = "K_UTM_*")){
      K_Number <- sub("K_UTM_", "", kutm)
      K_To_Convert <- get(kutm)
      K_LatLong <- spTransform(K_To_Convert, latlong)
      assign(paste0("K_LatLong_", K_Number), K_LatLong)
    }
    
    fullmap=ggmap(map, padding = 0)+
      geom_spatial(data = K_LatLong_95, aes(),
                   fill = "white",  alpha = 0.05, linetype = 1, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_90, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_80, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_70, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_60, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_50, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_40, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_30, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = K_LatLong_20, aes(),
                   fill = "white",  alpha = 0.05, linetype = 2, size = 0.5, colour = "black") +
      geom_spatial(data = MCPlatlong, aes(),
                   colour = "black",  alpha = 0, linetype = 1, size = 2.5) +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-1], xend = coords.x1[i],
                       y = coords.x2[i-1], yend = coords.x2[i]), size = 2, colour = "red4") +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-2], xend = coords.x1[i-1],
                       y = coords.x2[i-2], yend = coords.x2[i-1]), size = 1.5, alpha = 0.5, colour = "red4") +
      geom_segment(data = as.data.frame(SPlatlong), 
                   aes(x = coords.x1[i-3], xend = coords.x1[i-2],
                       y = coords.x2[i-3], yend = coords.x2[i-2]), size = 1, alpha = 0.25, colour = "red4") +
      geom_point(data = as.data.frame(SPlatlong),
                 aes(x = coords.x1, y = coords.x2),
                 colour = "black", pch = 3, size = 3) +
      geom_point(data = as.data.frame(SPlatlong), 
                 aes(x = coords.x1[i], y = coords.x2[i]),
                 colour = "red", pch = 16, size = 5) +
      labs(colour = "white", title = CurrSet$ID[1], 
           subtitle = paste(CurrSet$Tracking_date[length(CurrSet$ID)], 
                            CurrSet$Tracking_time[length(CurrSet$ID)]),
           x = "Longitude", y = "Latitude") +
      theme_bw()+ 
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 3),
            text = element_text(size = 30, colour = "black"),
            title = element_text(face = 1, size = 40), 
            plot.title = element_text(size = 70, face = 2,
                                      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
            plot.subtitle = element_text(face = 3, size = 50,
                                         margin = margin(t = 20, r = 0, b = 20, l = 0)),
            axis.text.x = element_text(angle = 45, hjust = 1, colour = "black",
                                       margin = margin(t = 15, r = 0, b = 25, l = 0)),
            axis.text.y = element_text(angle = 45, hjust = 1, colour = "black",
                                       margin = margin(t = 0, r = 15, b = 0, l = 25)),
            plot.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black", 
                                     size = 0.5, linetype = "solid"),
            axis.ticks = element_line(colour = "black", size = 3, linetype = "solid"),
            axis.ticks.length = unit(1, "cm"))
    
    print(paste("Datapoint", i,"/",length(animaldata$ID), "Complete"))
    print(paste(CurrSet$ID[1], 
                CurrSet$Tracking_date[length(CurrSet$ID)], 
                CurrSet$Tracking_time[length(CurrSet$ID)]))
    print(fullmap)
  }
  beep(2)
}, video.name = paste0(animaldata$ID[1], "MAPONLYanimated.mp4"))

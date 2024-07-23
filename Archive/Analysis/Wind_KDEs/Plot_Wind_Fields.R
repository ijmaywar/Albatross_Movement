# Create a map of wind_vel

map_lat_range <- seq(-70,-30,0.25) # Latitude (rows)
map_lon_range <- seq(-120,-10,0.25) # Longitude (columns)

output <- matrix(ncol=4,nrow=length(map_lat_range)*length(map_lon_range))

for (i_lat in 1:length(map_lat_range)) {
  for (i_lon in 1:length(map_lon_range)) {
    
    lon <- map_lon_range[i_lon]
    lat <- map_lat_range[i_lat]
    
    # isolate coordinates
    xy_j <- as.data.frame(cbind(lon, lat))
    colnames(xy_j) <- c("lon","lat")
    xy_j$lon <- Lon360to180(xy_j$lon) 
    
    # Extract u and v components for time j at location x and y
    u_j <- extract(ustack_timej, xy_j, ID=FALSE)
    v_j <- extract(vstack_timej, xy_j, ID=FALSE)
    
    if (length(u_j) != 1) {
      print("Number of wind measurements chosen != 1.")
      break
    } else if (is.na(u_j[[1]]) || is.na(v_j[[1]])) {
      print("Wind data for this coordinate cannot be found.")
      break
    }
    
    i <- (i_lat-1)*length(map_lon_range) + i_lon
    
    output[i,1] <- lat
    output[i,2] <- lon
    output[i,3] <- u_j[[1]]
    output[i,4] <- v_j[[1]]
  
  }
}

# finish formatting dataframe
output <- data.frame(output)
colnames(output) <- c("lat","lon","u","v")
output$lon <- wrap360(output$lon)

# Calculate wind velocity and direction
ddff <- uv2ddff(output)
output$wind_vel <- ddff$ff # m/s
output$wind_dir <- ddff$dd # [0,360) degrees

# PLOT IT!
ggplot(output, aes(x = lon, y = lat)) +
  # geom_quiver(aes(u = wind_vel * sin(wind_dir),
  #                v = wind_vel * cos(wind_dir),
  #                key = after_stat(str(wind_vel))),
  #            size = 1.5, scale = 0.1, arrow = arrow(length = unit(0.3, "inches"))) +
  geom_quiver(aes(u = u,
                  v = v,
                  key = after_stat(str(output$wind_vel))),
              size = 1.5, scale = 0.1, arrow = arrow(length = unit(0.3, "inches"))) +
  labs(title = "Wind Field") +
  theme_minimal()







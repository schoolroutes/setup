plot_od = function(od_schools_raw) {
  brks = c(0, 1, 10, 100, 1000)
  od_schools_raw %>%
    group_by(pupils) %>%
    summarise(n = n(), n_pupils = sum(pupils)) %>% 
    mutate(n_od_pairs = cut(x = pupils, breaks = brks)) %>% 
    ggplot() +
    geom_point(aes(pupils, n, size = n_pupils)) +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Number of pupils per OD pair") +
    ylab("Number of OD pairs")
}
read_clean_schools = function(file = "edubasealldata20220510.csv") {
  schools_raw = read_csv(file)
  # The LAestab identifier is composed of two parts
  # 3 digit LA number and 4 digit Estab number. In code terms, LAEstab=LA*10000+Estab.
  schools = schools_raw %>%
    mutate(laestab = as.numeric(`LA (code)`) * 10000 + EstablishmentNumber) %>% 
    filter(!is.na(Easting)) %>% 
    sf::st_as_sf(coords = c("Easting", "Northing"), crs = "EPSG:27700") %>% 
    sf::st_transform("EPSG:4326") %>% 
    mutate(NumberOfPupils = as.numeric(NumberOfPupils))
}

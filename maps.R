install.packages("tidyverse")
install.packages("sp") #for spatial data
install.packages("map")
install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")
install.packages("maps")
require(tidyverse)
library(tidyverse)
library(sp) # Konumsal veri için
tur<- readRDS("gadm36_TUR_1_sp.rds") # Turkey's spatial data
plot(tur)
tur@data %>% as_tibble() %>% head(10) 
tur_for <- fortify(tur) # Bu fonksiyon 'sp' paketinin içinde.Ýlleri ayrýmak için kullanýlýr.
head(tur_for)
ggplot(tur_for) + geom_polygon(aes(x = long, y = lat,group = group),color = "white",fill = "red") +
  theme_void() + coord_fixed()
head(tur@data$NAME_1)
table(tur@data$NAME_1)
table(cs$province)
library(readxl)
cs <- read_excel("cs.xlsx")
View(cs)
head(cs)
cs %>% as_tibble
tur@data %>% as_tibble() 

# Change letters from Turkish to English
turkceden_ingilizceye <- function(dataset){
  turkce_harfler<- c("Ç","Þ","Ð","Ý","Ü","Ö","ç","þ","ð","ý","ü","ö")
  ingilizce_harfler<- c("C","S","G","I","U","O","c","s","g","i","u","o")
  dataset=mgsub(turkce_harfler,ingilizce_harfler,dataset)
  return(dataset)
}
mgsub <- function(pattern, replacement, x, ...) {
  n = length(pattern)
  if (n != length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result = x
  for (i in 1:n) {
    result <- gsub(pattern[i],replacement[i],result)
  }
  return(result)
}
# Replacements 
tur@data$NAME_1 <- turkceden_ingilizceye(tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("K. Maras", "Kahramanmaras",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Kinkkale","Kirikkale",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Zinguldak", "Zonguldak", tur@data$NAME_1 )

cs$province=turkceden_ingilizceye(cs$province)
cs %>% as_tibble
id_and_cities<- data_frame(id = rownames(tur@data), province = tur@data$NAME_1) %>% left_join(cs, by = "province")
head(id_and_cities) # I combined tur@data and cs in this line.
final_map <- left_join(tur_for, id_and_cities, by = "id")
head(final_map)
summary(cs$dcases)
head(cs$density)
# Create change in cases map for provinces
ggplot(final_map) +geom_polygon( aes(x = long, y = lat, group = group, fill = dcases ), color = "grey") +
  coord_map() +theme_void() + labs(title = "Change in cases",caption = "Source: Ministry of Health") +
  scale_fill_distiller(name = "dcases",palette = "Spectral", limits = c(0,925.37), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
table(tur@data$NAME_1)
summary(cs$density)
# Create density map for provinces 
ggplot(final_map) +geom_polygon( aes(x = long, y = lat, group = group, fill = density ), color = "grey") +
  coord_map() +theme_void() + labs(title = "Population density",caption = "Source: TURKSTAT") +
  scale_fill_distiller(name = "density",palette = "Spectral", limits = c(0,2980), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
# Life expectancy
summary(cs$lifeexp)
ggplot(final_map) +geom_polygon( aes(x = long, y = lat, group = group, fill = lifeexp ), color = "grey") +
  coord_map() +theme_void() + labs(title = "life expectancy at birth",caption = "Source: TURKSTAT") +
  scale_fill_distiller(name = "life expectancy",palette = "Spectral", limits = c(74.93,80.73), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
# Cases at t=1
summary(cs$cases_t0)
# Plot cases at initial period on Turkey's map
ggplot(final_map) +geom_polygon( aes(x = long, y = lat, group = group, fill = cases_t0 ), color = "grey") +
  coord_map() +theme_void() + labs(title = "cases at t=1",caption = "Source: TURKSTAT") +
  scale_fill_distiller(name = "initial cases",palette = "Spectral", limits = c(2.75,230), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

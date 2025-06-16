---
  title: "klasyfikacja_nadzorowana"
output: html_document
date: "2025-06-13"
---
  Ten dokument opisuje działanie skryptu klasyfikacja_nadzorowana_wlodawski_3.R, którego celem jest przeprowadzenie klasyfikacji nadzorowanej. Algorytm rozpoznaje różne formy pokrycia terenu na podstawie zdjęć satelitarnych. Dane treningowe dostarczone zostaną z bazy danych CORINE Land Cover.
Analiza jest przeprowadzana dla konkretnego obszaru – powiatu włodawskiego.
Podstawowym źródłem danych są zdjęcia satelitarne Sentinel-2.


Instalacja potrzebnych pakietów.
terra: Biblioteka do pracy z danymi przestrzennymi (rastrowymi i wektorowymi).
rstac: Służy do wyszukiwania i pobierania danych z repozytoriów STAC (Spatiotemporal Asset Catalog), co umożliwia dostęp do zdjęć satelitarnych, w tym Sentinel-2.
sf: biblioteka do pracy z danymi wektorowymi (np. granicami powiatów, poligonami).
caret: Zestaw narzędzi ułatwiających budowę modeli uczenia maszynowego, tutaj używany do podziału danych na zbiór treningowy i testowy.
randomForest: Zawiera implementację algorytmu klasyfikacyjnego "Random Forest" (Las Losowy), który zostanie użyty do nauki i predykcji pokrycia terenu.
RColorBrewer: Dostarcza gotowe palety kolorów, które są używane do tworzenia estetycznych i czytelnych wizualizacji map.

Dodatkowo dodajemy zwiększenie limitu czasu na pobieranie plików do 600 sekund (10 minut). Ta opcja zapobiega przerwaniu operacji z powodu przekroczenia limitu czasu.

```{r setup, include=FALSE}
library("terra")
library("rstac")
library("sf")
library("caret")         # do podziału na zbiór treningowy i testowy
library("randomForest")  # klasyfikator Random Forest
library("RColorBrewer")  # ładne kolory do wizualizacji
options(timeout = 600)

#-----Weronika--------------------------------------------------------------
main_dir <- "D:/Nowy folder/studia/zap_projekt/zap_klasyfikacja_nadzorowana"
setwd(main_dir)

# Ścieżki do danych wejściowych
powiat_path <- file.path(main_dir, "powiat_wlodawski.shp")
clc_path <- file.path(main_dir, "CLC18_PL", "clc18_PL.shp")
bdot_dir <- file.path(main_dir, "bdot10k", "bdot10k_0619_SHP")
ptlz_path <- file.path(bdot_dir, "PL.PZGiK.3700.BDOT10k.0619__OT_PTLZ_A.shp")
ptwp_path <- file.path(bdot_dir, "PL.PZGiK.3700.BDOT10k.0619__OT_PTWP_A.shp")
pttr_path <- file.path(bdot_dir, "PL.PZGiK.3700.BDOT10k.0619__OT_PTTR_A.shp")
ptzb_path <- file.path(bdot_dir, "PL.PZGiK.3700.BDOT10k.0619__OT_PTZB_A.shp")

# Ścieżki do danych wyjściowych i tymczasowych
wyniki_dir <- file.path(main_dir, "wyniki")
sentinel_2023_dir <- file.path(main_dir, "sentinel_2023")
sentinel_enhanced_dir <- file.path(main_dir, "sentinel_enhanced")
mozaika_2023_path <- file.path(wyniki_dir, "mozaika_2023.tif")

print("Wczytanie granic powiatu.")
powiat <- vect(powiat_path)
powiat_wgs84 <- project(powiat, "EPSG:4326")
powiat_utm <- project(powiat, "EPSG:32634")

bbox_wgs84 <- ext(powiat_wgs84)
margin <- 0.1
bbox_buf <- c(bbox_wgs84[1] - margin, bbox_wgs84[3] - margin,
              bbox_wgs84[2] + margin, bbox_wgs84[4] + margin)

kanaly <- c("blue", "green", "red", "nir")

obrazy_2023 <- stac("https://earth-search.aws.element84.com/v1") |>
  stac_search(
    collections = "sentinel-2-c1-l2a",
    bbox = bbox_buf,
    datetime = "2023-05-01T00:00:00Z/2023-09-30T00:00:00Z"
  ) |>
  ext_query(`eo:cloud_cover` < 10) |>
  post_request() |>
  items_fetch()

print(paste("Znaleziono", length(obrazy_2023$features), "obrazów dla 2023 roku"))

if (length(obrazy_2023$features) > 0) {
  obrazy_info <- data.frame(
    nr = 1:length(obrazy_2023$features),
    tile_id = sapply(obrazy_2023$features, function(x) x$properties$`s2:tile_id`),
    data = sapply(obrazy_2023$features, function(x) substr(x$properties$datetime, 1, 10)),
    chmury = round(sapply(obrazy_2023$features, function(x) x$properties$`eo:cloud_cover`), 1),
    platforma = sapply(obrazy_2023$features, function(x) x$properties$platform)
  )
  print(obrazy_info)
}

# Pobieranie wybranych kafli 2023
tile_ids_2023 <- c("S2A_OPER_MSI_L2A_TL_2APS_20230829T133355_A042747_T34UFB_N05.09",
                   "S2A_OPER_MSI_L2A_TL_2APS_20230829T133355_A042747_T34UFC_N05.09")

base_dir_2023 <- sentinel_2023_dir
dir.create(base_dir_2023, showWarnings = FALSE)

# sprawdzenie czy wszystkie kafle są już pobrane, jeżeli tak to nie pobiera ponownie
all_tiles_downloaded_2023 <- TRUE
for (tile in tile_ids_2023) {
  tile_dir <- file.path(base_dir_2023, tile)
  if (!dir.exists(tile_dir)) {
    all_tiles_downloaded_2023 <- FALSE
    break
  }
  # sprawdzenie czy wszystkie kanały są pobrane dla tego kafla
  expected_files <- length(kanaly)
  actual_files <- length(list.files(tile_dir, pattern = "\\.tif$"))
  if (actual_files < expected_files) {
    all_tiles_downloaded_2023 <- FALSE
    break
  }
}

if (all_tiles_downloaded_2023) {
  print("Kafle 2023 są już pobrane.")
} else {
  print("Pobieranie kafli 2023...")
  for (tile in tile_ids_2023) {
    tile_dir <- file.path(base_dir_2023, tile)
    dir.create(tile_dir, showWarnings = FALSE)
    
    urls <- obrazy_2023 |>
      items_filter(properties$`s2:tile_id` == tile) |>
      assets_select(asset_names = kanaly) |>
      assets_url()
    
    for (i in seq_along(urls)) {
      file_path <- file.path(tile_dir, basename(urls[i]))
      if (!file.exists(file_path)) {
        print(paste("Pobieranie:", basename(file_path)))
        download.file(urls[i], destfile = file_path, mode = "wb")
      } else {
        print(paste("Plik już istnieje:", basename(file_path)))
      }
    }
  }
  print("Pobieranie kafli 2023 zakończone.")
}

# mozaikowanie 2023 - sprawdzenie czy plik już istnieje, jeżeli nie to mozaikuje
dir.create(wyniki_dir, showWarnings = FALSE)

if (file.exists(mozaika_2023_path)) {
  print("Wczytanie istniejącej mozaiki 2023.")
  r_2023 <- rast(mozaika_2023_path)
} else {
  print("Tworzenie mozaiki 2023.")
  rastry_2023 <- list.files(sentinel_2023_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  kafle_2023 <- lapply(unique(dirname(rastry_2023)), function(dir) {
    r <- rast(list.files(dir, pattern = "\\.tif$", full.names = TRUE))
    names(r) <- kanaly
    return(r)
  })
  
  r_2023 <- mask(crop(mosaic(sprc(kafle_2023)), powiat_utm), powiat_utm) |>
    clamp(lower = 0, upper = 1, values = FALSE)
  
  # Zapisanie mozaiki
  writeRaster(r_2023, mozaika_2023_path, overwrite = TRUE)
  print("Mozaika 2023 zapisana do pliku.")
}

plotRGB(r_2023, r = 3, g = 2, b = 1, stretch = "lin", main = "Sentinel-2 RGB 2023")
plot(powiat_utm, add = TRUE, col = NA, border = "red", lwd = 2)

#--------------------------------------------------------------------------------

#-Rafał----------------------------------------------------------------------
clc = vect(clc_path)
clc_utm = crop(project(clc, crs(r_2023)), powiat_utm)

selected_classes = c("112", "211", "311", "511", "231")
clc_sel = clc_utm[clc_utm$CODE_18 %in% selected_classes, ]
clc_sel$class = as.factor(clc_sel$CODE_18)

set.seed(42)
points_train = spatSample(clc_sel, size = 600, method = "random")
points_train$class = as.factor(points_train$CODE_18)

train_data = extract(r_2023, points_train, df = TRUE)
train_data$class = points_train$class
train_data = na.omit(train_data)

set.seed(123)
split_index = createDataPartition(train_data$class, p = 0.7, list = FALSE)
train_set = train_data[split_index, ]
test_set = train_data[-split_index, ]

model_rf = randomForest(as.factor(class) ~ blue + green + red + nir,
                        data = train_set,
                        ntree = 200,
                        importance = TRUE)

classified = predict(r_2023, model_rf, type = "response")

colors = brewer.pal(length(selected_classes), "Set1")
plot(classified, col = colors, main = "Wynik klasyfikacji pokrycia terenu (RF)")
legend("topright", legend = selected_classes, fill = colors, title = "Klasy CLC")

pred_test = predict(model_rf, test_set)
conf_matrix = confusionMatrix(pred_test, test_set$class)
print(conf_matrix)
print(conf_matrix$table)
print(conf_matrix$byClass[, c("Precision", "Recall", "F1")])

ptlz = vect(ptlz_path)   # lasy
ptwp = vect(ptwp_path)   # wody
pttr = vect(pttr_path)   # pastwiska,trawiasta
ptzb = vect(ptzb_path)   # zabudowa

crs_utm = crs(r_2023)
ptlz = project(ptlz, crs_utm)
ptwp = project(ptwp, crs_utm)
pttr = project(pttr, crs_utm)
ptzb = project(ptzb, crs_utm)

ptlz$KOD_KLASY = 1  # las
ptwp$KOD_KLASY = 2  # wody
pttr$KOD_KLASY = 3  # trawiasta, grunty rolne
ptzb$KOD_KLASY = 4 #zabudowy

bdot_all = rbind(ptlz, ptwp, pttr, ptzb)

bdot_raster = rasterize(bdot_all, r_2023[[1]], field = "KOD_KLASY")

bdot_crop = crop(bdot_raster, powiat_utm)
bdot_crop = mask(bdot_crop, powiat_utm)

compare = cbind(values(classified), values(bdot_crop))
conf_matrix_bdot = table(klasyfikacja = compare[,1], bdot = compare[,2])
print("Macierz pomyłek Klasyfikacja vs BDOT:")
print(conf_matrix_bdot)

# legenda
class_labels_rf <- c("Zabudowa", "Pola uprawne", "Pastwiska", "Lasy", "Wody")
class_labels_bdot <- c("Lasy", "Wody", "Pola i pastwiska", "Zabudowa")

# kolory
colors_rf <- brewer.pal(length(selected_classes), "Set1")
colors_bdot <- c("darkgreen", "blue", "gold", "red")

layout(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE), heights = c(0.8, 0.2))

par(mar = c(2, 2, 3, 1))
plot(classified, col = colors_rf, main = "Klasyfikacja RF (2023)", legend = FALSE)

bdot_crop_factor = as.factor(bdot_crop)
levels(bdot_crop_factor) = data.frame(value = 1:4, label = class_labels_bdot)
par(mar = c(2, 2, 3, 1))
plot(bdot_crop_factor, col = colors_bdot, main = "Pokrycie terenu BDOT", legend = FALSE)

par(mar = c(0, 2, 0, 2))
plot.new()
legend("center", legend = class_labels_rf, fill = colors_rf, title = "Klasy", bty = "n", ncol = 3, cex = 0.75)

par(mar = c(0, 2, 0, 2))
plot.new()
legend("center", legend = class_labels_bdot, fill = colors_bdot, title = "Klasy BDOT", bty = "n", ncol = 2, cex = 0.75)

par(mfrow = c(1, 1))

freq_table = freq(classified)
freq_df = data.frame(class = freq_table[, "value"],
                     count = freq_table[, "count"])
freq_df$percent = round(100 * freq_df$count / sum(freq_df$count), 2)
print(freq_df)

INTERPRETACJA WYNIKÓW

1. Klasa 311, 231 i 211 (pastwiska, lasy, pola) stanowią zdecydowaną większość obszaru (około 32-33%).
2. Klasa 112 (zabudowa) to tylko około 1% powierzchni.
3. Klasa 511 (wody) ma bardzo niski udział — tylko 0.07%

Możemy zwizualizować porównanie klasyfikacji i BDOT10k. Możemy ujednolicić klasy do wizualizacji.
```{r wizualizacja_porownawcza_reklasyfikacja, echo=FALSE}
# Wartości RF (z sorted(selected_classes)): 1="112", 2="211", 3="231", 4="311", 5="511"
# Mapowanie na BDOT: Zabudowa(4), Pola(3), Pastwiska(3), Lasy(1), Wody(2)
reclass_matrix <- matrix(c(1, 4,  # Zabudowa
                           2, 3,  # Pola uprawne
                           3, 3,  # Pastwiska
                           4, 1,  # Lasy
                           5, 2), # Wody
                         ncol = 2, byrow = TRUE)

classified_reclass <- classify(classified, reclass_matrix, others = NA)

layout(matrix(c(1, 2, 3), nrow = 1), widths = c(0.425, 0.425, 0.15))

par(mar = c(2, 2, 3, 1))
plot(classified_reclass, col = colors_bdot, main = "Klasyfikacja RF (kolory BDOT)", legend = FALSE)

plot(bdot_crop_factor, col = colors_bdot, main = "Pokrycie terenu BDOT", legend = FALSE)

par(mar = c(2, 0, 3, 0))
plot.new()
legend("left", legend = class_labels_bdot, fill = colors_bdot, title = "Klasy", bty = "n", cex = 0.9)

par(mfrow = c(1, 1))

#-------------------------------------------------------------------------------

#---Weronika--------------------------------------------------------------------
kanaly_enhanced <- c("blue", "green", "red", "nir", "swir16")
mozaika_enhanced_path <- file.path(wyniki_dir, "mozaika_uproszczona_2023.tif")

if (file.exists(mozaika_enhanced_path)) {
  print("Wczytanie istniejącej mozaiki ulepszonej.")
  r_2023_enhanced <- rast(mozaika_enhanced_path)
} else {
  print("Tworzenie ulepszonej mozaiki (5 kanałów).")
  
  kafle_do_mozaiki <- lapply(unique(dirname(list.files(sentinel_2023_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE))), function(dir) {
    paths_10m <- sort(list.files(dir, pattern = "B0[2348]\\.tif$", full.names = TRUE))
    path_20m <- list.files(dir, pattern = "B11\\.tif$", full.names = TRUE)
    
    if (length(paths_10m) < 4 || length(path_20m) < 1) return(NULL)
    
    r_10m <- rast(paths_10m)
    r_20m <- rast(path_20m)
    
    r_10m_cropped <- crop(r_10m, powiat_utm, snap="out")
    r_20m_cropped <- crop(r_20m, powiat_utm, snap="out")
    
    if (is.null(r_10m_cropped)) return(NULL)
    
    r_20m_resampled <- resample(r_20m_cropped, r_10m_cropped, method = "bilinear")
    
    r_combined <- c(r_10m_cropped, r_20m_resampled)
    names(r_combined) <- kanaly_enhanced
    return(r_combined)
  })
  
  kafle_do_mozaiki <- kafle_do_mozaiki[!sapply(kafle_do_mozaiki, is.null)]
  
  r_2023_enhanced <- mosaic(sprc(kafle_do_mozaiki))
  r_2023_enhanced <- mask(crop(r_2023_enhanced, powiat_utm), powiat_utm) |>
    clamp(lower = 0, upper = 1, values = FALSE)
  
  writeRaster(r_2023_enhanced, mozaika_enhanced_path, overwrite = TRUE)
  print("Ulepszona mozaika zapisana.")
}

print("Obliczanie wskaźników spektralnych.")
nir <- r_2023_enhanced[["nir"]]
red <- r_2023_enhanced[["red"]]
green <- r_2023_enhanced[["green"]]
swir16 <- r_2023_enhanced[["swir16"]]

ndvi <- (nir - red) / (nir + red); names(ndvi) <- "ndvi"
ndwi <- (green - nir) / (green + nir); names(ndwi) <- "ndwi"
ndbi <- (swir16 - nir) / (swir16 + nir); names(ndbi) <- "ndbi"

r_2023_enhanced <- c(r_2023_enhanced, ndvi, ndwi, ndbi)
#------------------------------------------------------------------------

#----Rafał--------------------------------------------------------------

print("Przeprowadzanie uproszczonego próbkowania losowego.")
set.seed(123)

points_train_enhanced <- spatSample(clc_sel, size = 2000, method = "random")

print(paste("Pobrano łącznie", nrow(points_train_enhanced), "punktów treningowych."))
print("Liczba pobranych próbek dla każdej klasy:")
print(table(points_train_enhanced$CODE_18))

train_data_enhanced <- extract(r_2023_enhanced, points_train_enhanced, df = TRUE)
train_data_enhanced$class <- as.factor(points_train_enhanced$CODE_18)
train_data_enhanced <- na.omit(train_data_enhanced)

print("Dzielenie danych na zbiór treningowy i testowy.")
split_index_enhanced <- createDataPartition(train_data_enhanced$class, p = 0.7, list = FALSE)
train_set_enhanced <- train_data_enhanced[split_index_enhanced, ]
test_set_enhanced <- train_data_enhanced[-split_index_enhanced, ]

print("Trenowanie ulepszonego modelu Random Forest.")
model_rf_enhanced <- randomForest(as.factor(class) ~ blue + green + red + nir + swir16 + ndvi + ndwi + ndbi,
                                  data = train_set_enhanced,
                                  ntree = 200,
                                  importance = TRUE)

print("Ocena ulepszonego modelu...")
pred_test_enhanced <- predict(model_rf_enhanced, test_set_enhanced)
conf_matrix_enhanced <- confusionMatrix(pred_test_enhanced, test_set_enhanced$class)
print("Wyniki ulepszonego modelu:")
print(conf_matrix_enhanced)

print("Tworzenie mapy ulepszonej klasyfikacji.")
classified_enhanced <- predict(r_2023_enhanced, model_rf_enhanced, type = "response")

print("Rysowanie map do porównania.")
layout(matrix(c(1, 2, 3), nrow = 1), widths = c(0.425, 0.425, 0.15))

par(mar = c(2, 2, 3, 1))
plot(classified_enhanced, col = colors_rf, main = "Ulepszona klasyfikacja RF", legend = FALSE)

plot(classified, col = colors_rf, main = "Oryginalna klasyfikacja RF", legend = FALSE)

par(mar = c(2, 0, 3, 0))
plot.new()
legend("left", legend = class_labels_rf, fill = colors_rf, title = "Klasy", bty = "n", cex = 0.9)

par(mfrow = c(1, 1))

Porównanie ulepszonej klasyfikacji z BDOT10k.
Najpierw trzeba zreklasyfikować model do klas BDOT i wtedy można wyświetlić i porównać mapy.

print("Porównanie wizualne ulepszonej klasyfikacji z BDOT10k.")

classified_enhanced_reclass <- classify(classified_enhanced, reclass_matrix, others = NA)

layout(matrix(c(1, 2, 3), nrow = 1), widths = c(0.425, 0.425, 0.15))

par(mar = c(2, 2, 3, 1))
plot(classified_enhanced_reclass, col = colors_bdot, main = "Ulepszona klasyfikacja RF (kolory BDOT)", legend = FALSE)

bdot_crop_factor <- as.factor(bdot_crop)
levels(bdot_crop_factor) <- data.frame(value = 1:4, label = class_labels_bdot)
plot(bdot_crop_factor, col = colors_bdot, main = "Pokrycie terenu BDOT", legend = FALSE)

par(mar = c(0, 0, 1, 0))
plot.new()
legend("left", legend = class_labels_bdot, fill = colors_bdot, title = "Klasy", bty = "n", cex = 0.9)

par(mfrow = c(1, 1))

#----------------------------------------------------------------------


#---Weronika------------------------------------------------------------------------
print("Porównanie powierzchniowe klas (ulepszona RF vs BDOT).")

pixel_area_ha <- prod(res(classified_enhanced_reclass)) / 10000

freq_enhanced <- freq(classified_enhanced_reclass)
df_enhanced <- data.frame(
  class_id = freq_enhanced[, "value"],
  area_ha_rf = round(freq_enhanced[, "count"] * pixel_area_ha, 2)
)

freq_bdot <- freq(bdot_crop)
df_bdot <- data.frame(
  class_id = freq_bdot[, "value"],
  area_ha_bdot = round(freq_bdot[, "count"] * pixel_area_ha, 2)
)

class_labels_bdot_df <- data.frame(class_id = 1:4, label = class_labels_bdot)

df_enhanced <- merge(df_enhanced, class_labels_bdot_df, by = "class_id")
df_bdot <- merge(df_bdot, class_labels_bdot_df, by = "class_id")
comparison_df <- merge(df_enhanced[, c("label", "area_ha_rf")], df_bdot[, c("label", "area_ha_bdot")], by = "label", all = TRUE)
comparison_df[is.na(comparison_df)] <- 0

comparison_df$roznica_ha <- round(comparison_df$area_ha_rf - comparison_df$area_ha_bdot, 2)

print("Porównanie powierzchni klas (w hektarach):")
print(comparison_df)

#----------------------------------------------------------------------

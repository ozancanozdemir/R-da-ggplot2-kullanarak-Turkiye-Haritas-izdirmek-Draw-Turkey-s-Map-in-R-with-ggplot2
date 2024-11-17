
library(ggplot2) #veri görselleştirme için
library(sf) #konum verisini işlemek için
library(dplyr) #veri manipülasyonu için
library(stringr) #string işlemleri için

# Türkiye'nin il sınırlarını içeren veriyi indir

veriniz <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TUR_1.json")

# İlk 6 satırı göster

head(veriniz)

# Veri setini grafiğini çizdir 

ggplot(data = veriniz) +geom_sf()

# Türkiye'nin 2023 genel seçim sonuçlarını içeren veriyi indir

secim_2023 <- turkeyelections::genel_secim_2023_il
secim_2023 |> head()

secim_2023 |> glimpse()

# CHP verilerini seç ve işle

chp_data <- secim_2023 |> select(Il,`CHP_%`)

chp_data

# Veri ön işlemesi

chp_data <- chp_data |>  mutate(
    NAME_1 = stringr::str_extract(Il, "^[A-Za-zÇĞÖŞÜİ]+") #sayısal değerleri çıkarttık 
  ) |>   group_by(NAME_1) %>%
  # Aynı şehirlerin ortalama CHP_%% değerini hesapla
  summarise(
    CHP_oy = mean(`CHP_%`, na.rm = TRUE),
    .groups = "drop"
  ) |>  mutate(
    NAME_1 = stringr::str_to_sentence(NAME_1)
  )

chp_data

chp_data$NAME_1<-stringr::str_replace_all(chp_data$NAME_1,"i̇","i")

# İl isimlerini düzelt

veriniz$NAME_1<-c("Adana", "Adiyaman", "Afyonkarahisar", "Ağri", "Aksaray", "Amasya",  "Ankara", "Antalya", "Ardahan", "Artvin", "Aydın", "Balıkesir", 
 "Bartın", "Batman", "Bayburt", "Bilecik", "Bingöl", "Bitlis",   "Bolu", "Burdur", "Bursa", "Çanakkale", "Çankırı", "Çorum", "Denizli", "Diyarbakır", "Düzce", "Edirne", "Elazığ", "Erzincan", "Erzurum", "Eskişehir", "Gaziantep", "Giresun", "Gümüşhane", "Hakkari", "Hatay", "Iğdır", "Isparta", "İstanbul", "İzmir",  "Kahramanmaraş", "Karabük", "Karaman", "Kars", "Kastamonu", "Kayseri", "Kilis","Kirikkale", "Kirklareli", "Kırşehir", "Kocaeli", "Konya",  "Kütahya", "Malatya", "Manisa", "Mardin", "Mersin", "Muğla",  "Muş", "Nevşehir", "Niğde", "Ordu", "Osmaniye", "Rize", "Sakarya",  "Samsun", "Şanlıurfa", "Siirt", "Sinop", "Şırnak", "Sivas",  "Tekirdağ", "Tokat", "Trabzon", "Tunceli", "Uşak", "Van", 
"Yalova", "Yozgat", "Zonguldak")

# Verileri birleştir

veriniz_joined <- left_join(veriniz, chp_data, by = c("NAME_1" = "NAME_1"))

veriniz_joined |> head()

# Haritayı çizdir

ggplot(data = veriniz_joined) +geom_sf(aes(fill = CHP_oy))+
  scale_fill_gradient2(high = "steelblue", low = "darkorange",mid="white", midpoint = mean(veriniz_joined$CHP_oy,na.rm=T))+theme_void() +
  labs(title = "CHP'nin 2023 Seçim Sonuçları Oy Dağılımı", fill = "CHP Oy Oranı",caption = "Haritayı Hazırlayan: Ozancan Özdemir") + theme(plot.title = element_text(size = 15,face="bold"), plot.subtitle = element_text(size = 10,face="bold"), plot.caption = element_text(size = 10,face="bold"),legend.position = "top")

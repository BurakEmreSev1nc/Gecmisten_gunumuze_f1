# F1 de Geçmişten Günümüze Değişimler

Bu çalışma, Eskişehir Teknik Üniversitesi İstatistik Bölümü lisans programında 2023-2024 Güz döneminde yürütülen Veri Görselleştirme dersi kapsamında gerçekleştirilen dönem sonu projesidir. Proje, Formula 1 (F1) yarışlarına dair geçmişten günümüze kadar olan verileri içermekte olup, özellikle pist özellikleri, yüksek hızlar ve pilot istatistikleri üzerine odaklanmıştır.

İstatistiksel Analiz ve Veri Görselleştirme
Çalışma, istatistik alanındaki temel prensipleri uygulayarak Formula 1 verilerini analiz etmeyi amaçlamaktadır. Bu analizler, yarış pistlerinin teknik özelliklerini, araçların ulaştığı yüksek hızları ve pilot performansını içermektedir. Veri görselleştirme yöntemleri, grafikler, tablolar ve benzeri araçlar kullanılarak elde edilen bulguların daha anlaşılır bir şekilde ifade edilmesini hedeflemektedir.

Araştırma Konusu ve Katkılar
Çalışma, Formula 1 dünyasının geçmişinden günümüze kadar olan evrimini istatistiksel bir bakış açısıyla ele alarak, pist özellikleri ile yüksek hızların nasıl değiştiğini ve pilot istatistiklerinin nasıl evrimleştiğini incelemektedir. Bu bağlamda, projenin amacı, Formula 1'in teknik yönlerini ve sporun evrimini anlamak için istatistiksel analizleri kullanarak bilimsel bir katkı sağlamaktır.


## Veri Seti

Formula 1 (kısaca F1), Fédération Internationale de l'Automobile (FIA) tarafından onaylanan ve Formula One Group tarafından sahip olunan, tek koltuklu otomobil yarışlarının en üst sınıfını temsil eden bir motorsporu dalıdır. FIA Formula One World Championship, 1950'deki ilk sezonundan itibaren dünya çapında düzenlenen yarış etkinliklerinin zirvesini temsil etmektedir. Terimde yer alan "formula" kelimesi, tüm katılımcı araçların uymak zorunda olduğu belirli bir kural ve düzen setine atıfta bulunmaktadır. Formula 1 sezonları, genellikle özel olarak tasarlanmış pistlerde ve bazen kamusal yollarda düzenlenen bir dizi yarış olan Grand Prix'leri içermektedir.

İçerik
Veri kümesi, Formula 1 yarışlarına, sürücülere, takımlara, sıralama turlarına, pistlere, tur zamanlarına, pit stoplarına ve şampiyonluklara dair 1950'den 2023'e kadar olan dönemi kapsayan geniş bir bilgi yelpazesini içermektedir.

Bu veri seti, http://ergast.com/mrd/ adresinden derlenmiştir.

İlham
"Yarışlar pistte kazanılır. Şampiyonalar ise fabrikada kazanılır." - Mercedes (2019)

Yakalanan, analiz edilen ve Formula 1 araçlarının tasarımı, inşası ve sürüşü için kullanılan veri miktarı, bu alanda gerçekleşen olağanüstü gelişmelere ışık tutmaktadır. Formula 1, dünya genelinde milyonlarca takipçi tarafından ilgiyle izlenen küresel bir spor dalıdır ve sürücülerin bu araçlar üzerinde limitlerini zorlayarak dünya çapında en hızlı yarışçılar olma hedeflerini takip etmek oldukça etkileyicidir

## Paketler Ve Veri Setimiz


```
library(ggplot2)

library(dplyr)

library(gridExtra)

library(ggthemes)

library(RColorBrewer)

library(grid)

library(gridExtra)

library(ggrepel)

library(viridis)

library(circlize)



circuits <- read.csv("circuits.csv")

constructor_results <- read.csv("constructor_results.csv")

constructor_standings <- read.csv("constructor_standings.csv")

constructors <- read.csv("constructors.csv")

driver_standings <- read.csv("driver_standings.csv")

drivers <- read.csv("drivers.csv")

lap_times <- read.csv("lap_times.csv")

pit_stops <- read.csv("pit_stops.csv")

qualifying <- read.csv("qualifying.csv")

races <- read.csv("races.csv")

results <- read.csv("results.csv")

seasons <- read.csv("seasons.csv")

sprint_results <- read.csv("sprint_results.csv")

status <- read.csv("status.csv")
```





## Grafik 1 = Pistlere Göre Yüksek Hız Oratalamaları

```
#result2 oluşturma
results<-read.csv('results.csv',sep=',',stringsAsFactors=F)
results$fastestLapSpeed<-as.numeric(results$fastestLapSpeed)
convertFastestLap<-function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
results$fastestLapTimeNum<-sapply(results$fastestLapTime, convertFastestLap)
races<-read.csv('races.csv',stringsAsFactors=F,sep=',')
#convert character to Date
races$date<-as.Date(races$date,"%Y-%m-%d")
#remove "Grand Prix" in the name
races$name<-gsub(" Grand Prix","",races$name)
results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')
circuits<-read.csv("circuits.csv",sep=",",stringsAsFactors=F)
races<-left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')
```

## 1.Grafik
```
results_2 %>%
  filter(year >= 2006) %>%
  group_by(name, year) %>%
  summarize(medianFastestLapSpeed = median(fastestLapSpeed, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = medianFastestLapSpeed, color = medianFastestLapSpeed)) +
  geom_point() +
  theme_bw() + 
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  theme(
    axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 8), 
    strip.text.x = element_text(size = 8),
    plot.subtitle = element_text(hjust = 1, vjust = -217),  
    axis.title.y = element_blank(),  
    axis.title.x = element_blank()   
  ) +
  facet_wrap(~name, ncol = 6, scales = "free_x") +
  labs(
    title = 'Pıstlere Gore En Yuksek Hızlar',
    subtitle = 'Mil/Saat cinsinden hesaplanmıştır',
  ) +
  guides(color = FALSE)
```

![image](https://github.com/BurakEmreSev1nc/Gecmisten_gunumuze_f1/assets/155571442/48690ad5-b704-420a-96a8-3cd82b0aa9cc)


Bu grafik, Formula 1 pistlerinin yıllara göre ortalama en yüksek hızlarını temsil etmekte olup, özellikle şehir içi pistler ile standart pistler arasındaki belirgin farkları göstermektedir. Yüksek hızların pistler arasındaki değişkenliği ve şehir içi pistlerin öne çıkan performans özellikleri, grafik üzerinde açıkça görülmektedir.En yükse hız ortalamasına sahip pist italyadır,en düşük hız ortalamasına sahip piste monacodur.




## Grafik 2 = Sezon Başına Tur Sürelerinin Ortalamaları
```
results_2 %>% 
  dplyr::filter(year > 2005) %>% 
  dplyr::group_by(name, year) %>% 
  summarize(medianFastestLapTimeNum = median(fastestLapTimeNum, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(year), y = medianFastestLapTimeNum, color = medianFastestLapTimeNum)) +
  geom_boxplot(alpha = 0.25) +
  theme_fivethirtyeight() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 3.5) +
  geom_smooth(method = 'loess', aes(group = 1), color = 'red', lty = 2, size = 0.5) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    title = 'Sezon Başına Tur Surelerinin Ortalamaları',
    subtitle = 'Saniye cınsınden, bir sezona gore gruplandırılmıştır'
  ) +
  guides(color = FALSE) +
  theme(
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16),  
    plot.subtitle = element_text(hjust = 1, vjust = 1)  
  )
```
![b49b84a4-e55d-4d8d-be92-437b9982a18d](https://github.com/BurakEmreSev1nc/Gecmisten_gunumuze_f1/assets/155571442/2ed1c52f-4f06-4ebd-be78-3b23e9d072e2)
Bu grafik, Formula 1 yarışlarında tur sürelerini etkileyen ana faktörün motor güçleri olduğunu ortaya koymaktadır. Analizde, motor güçlerinin yanı sıra kazalar ve iklim hava koşullarının da tur sürelerine etkisi dikkate alınmıştır. Grafik üzerinde gözlemlendiği üzere, en kısa tur süresi 2020 yılına aittir, bu da muhtemelen teknolojik gelişmeler ve araç performansındaki ilerlemelerin bir yansıması olabilir. Diğer yandan, en uzun tur süreleri 2014-2015 yıllarına aittir ve bu durum, o dönemdeki belirli koşullar veya yarış şartları, örneğin kazalar veya iklim değişiklikleri, tarafından etkilenebileceğini göstermektedir. Bu analiz, Formula 1 tur sürelerinin zaman içindeki değişimini anlamak ve motor güçlerinin yanı sıra diğer faktörlerin etkilerini değerlendirmek için önemli bir çerçeve sunmaktadır.


## Grafik 3 = Yıllara Gore Hız Ortalamaları
```
results_2 %>%
  filter(year >= 2006) %>%
  group_by(name, year) %>%
  summarize(medianFastestLapSpeed = median(fastestLapSpeed, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = medianFastestLapSpeed, color = medianFastestLapSpeed)) +
  geom_boxplot(alpha = 0.25) +
  theme_fivethirtyeight() +
  geom_smooth(method = 'loess', aes(group = 1), color = 'red', lty = 2, size = 0.5) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    title = 'Yıllara Gore Hız Ortalamaları',
    subtitle = 'Mil/Saat cinsinden hesaplanmıştır'
  ) +
  guides(color = FALSE) +
  theme(
    plot.subtitle = element_text(hjust = 1, vjust = 1),  
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_text(size = 16)   
  )
```
![63a89b6e-9c64-44ec-ab28-a92d62996902](https://github.com/BurakEmreSev1nc/Gecmisten_gunumuze_f1/assets/155571442/44dab4fa-334c-4c70-94b5-55294a654f38)


Bu grafik, Formula 1 yarışlarında elde edilen en yüksek hız ortalamaları ile takımların motor değişimine geçiş süreleri arasında ters orantılı bir ilişkiyi göstermektedir. Analiz, V8 motorundan sonraki dönemlerde takımların yeni motor teknolojilerine uyum sağlama ve entegrasyon süreçlerini ele almaktadır. Grafikte belirgin olarak görüldüğü üzere, 2020 yılı en yüksek hız ortalamasına sahiptir, bu durum muhtemelen takımların daha modern ve gelişmiş motor teknolojilerini benimsemeleri ve optimize etmeleriyle ilişkilidir.


## Grafik 4 = En Cok Yarıs Kazanan 10 Pilot
```
results <- read.csv("results.csv")
drivers <- read.csv("drivers.csv")
constructors <- read.csv("constructors.csv")


merged_data <- results %>% 
  left_join(drivers, by = "driverId") %>%
  left_join(constructors, by = "constructorId")


top_winners <- merged_data %>% 
  filter(positionOrder == 1) %>%
  group_by(driverId, forename, surname, name) %>%
  summarise(total_wins = n()) %>%
  arrange(desc(total_wins)) %>%
  head(10) 


ggplot(top_winners, aes(x = reorder(paste(forename, surname), -total_wins), y = total_wins, fill = name, label = total_wins)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "En Cok Yarıs Kazanan 10 Pilot",
       x = "Pilotlar",
       y = "Toplam Kazanılan Yarış Sayısı",
       fill = "Takımlar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.position = "top") +
  
  coord_flip()
```
![27734d40-488a-465d-8774-01de7bed017d](https://github.com/BurakEmreSev1nc/Gecmisten_gunumuze_f1/assets/155571442/6f3e775c-eb4a-4970-8cd8-118e0a3a9de1)


Grafik analizi, Formula 1 yarışlarında en fazla yarış kazanan pilotun Lewis Hamilton olduğunu belirtmektedir. Bu önemli gözlem, Hamilton'un kariyeri boyunca elde ettiği yüksek başarı düzeyini vurgulamaktadır. Özellikle, grafik üzerinden anlaşıldığı üzere, Hamilton'un en çok yarış kazanan pilot olarak öne çıkmasında bir takıma bağımlılığın da etkisi belirgin olarak görülmektedir.

Hamilton'un takıma olan bu belirgin bağlılığı, sürücünün bireysel yeteneklerinin yanı sıra, araç performansını ve teknik destek düzeyini de içeren bir bütünlük oluşturmuştur. Bu durum, Formula 1'de başarı elde etmenin sadece sürücü yetenekleriyle değil, aynı zamanda etkili bir takım işbirliği ve teknik altyapıyla da sağlanabileceğini göstermektedir.

Hamilton'un bu istatistiksel üstünlüğü, sadece bireysel sürücü başarısını değil, aynı zamanda takım ve sürücü arasındaki uyumun ve işbirliğinin kritik önemini vurgulayarak, Formula 1'in karmaşıklığını anlamak açısından akademik bir perspektif sunmaktadır.




## Grafik 5 = Rekabet Ve Dominasyonun Yıllara Gore Gorsellestirilmesi

```
#result3 oluşturma

drivers<-read.csv('drivers.csv',sep=',',stringsAsFactors=F)
drivers$age_driver <- 2017 - sapply(drivers$dob, function(x) as.numeric(strsplit(x,'/')[[1]][3]))
driversStandings<-read.csv('driver_standings.csv',sep=',',stringsAsFactors=F)
drivers<-left_join(drivers %>% select(-url), driversStandings,by='driverId')

results_3<-left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 

results_3<-left_join(results_3,races %>% select(-time), by='raceId')

temp<-(results_3 %>% filter(position==1) %>% group_by(driverRef) %>% summarize(count=n()) %>% arrange(-count) %>% top_n(10))$driverRef
results_3$top10<-ifelse(results_3$driverRef %in% temp,results_3$driverRef,'other')
```

## Grafik 5
```
filtered_results <- results_3 %>%
  filter(position == 1, !is.na(top10), top10 != "other") %>%
  group_by(top10, year) %>%
  summarize(count = n())

ggplot(filtered_results, aes(x = factor(year), y = count, fill = top10)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +  
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1),  
        axis.text.y = element_text(size = 14)) +  
  scale_fill_brewer(name = "", palette = 'Paired') +
  guides(fill = guide_legend(ncol = 25)) +
  labs(title = 'Rekabet Ve Dominasyonun Yıllara Gore Gorsellestirilmesi',
       subtitle = '1962-2023')
```
![65788d3d-f88e-4d72-9a97-9ba6cb7fc46b](https://github.com/BurakEmreSev1nc/Gecmisten_gunumuze_f1/assets/155571442/40478aa1-02c5-4b46-9098-27f3b09d06d6)

1960'ların ve 1970'lerin hegemonyası, Jim Clark ve Jackie Stewart'ın üstünlüğüne işaret etmekteydi. 1970'lerin sonlarına doğru ise Niki Lauda'nın hakimiyeti belirginleşti. 1980'lerden erken 1990'lara kadar geçen süreçte, Nigel Mansell, Alain Prost ve Ayrton Senna'nın üstünlük mücadelesi dikkat çekti. Kırmızı Baron, 1990'ların tamamını ve 2000'lerin başlarını egemenliği altında geçirdi. Daha yakın zamanlarda ise Fernando Alonso, Lewis Hamilton ve Sebastian Vettel arasında bir rekabet gözlemlenmektedir. Bu dönemsel değişimler, Formula 1 tarihinde farklı pilotların sahip olduğu üstünlükleri yansıtarak sporun evrimini göstermektedir.









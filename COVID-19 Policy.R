library("dplyr")
library("tidyverse")
library("chron")
library("ggplot2")
library("janitor")
library("Hmisc")
library("funModeling")
library("tidyverse")
library("openair")
library("rticles")
library("lubridate")
library("tidyr")
library("skimr")
library("rmarkdown")
library("visdat")
library("maps")
library("leaflet")
library("plotly")
library("waffle") 
library("DataExplorer")
library("lattice")
library("wordcloud")
library("gridExtra")
library("scales")
library("DataExplorer")
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(tidyverse)
library(tidyquant)
library(timetk)
library(deSolve)
library(tidyverse)
library(mgcv)
library(caret)
library(MLmetrics)
install.packages("MLmetrics")


#Memasukkan dataset Province dan Regional
corona1<-read.csv(file.choose(),sep=",")
corona2<-read.csv(file.choose(),sep=",")

#Menghapus seluruh duplicate data pada dataset
province<-unique(corona1)
regional<-unique(corona2)

#Mengubah seluruh blank cells menjadi NA
regional<-regional %>% mutate_all(na_if,"")
province<-province %>% mutate_all(na_if,"")

#Merangkum jumlah missing data pada tiap kolom pada masing-masing dataset
sort(sapply(province, function(x) sum(is.na(x))), decreasing = TRUE)
sort(sapply(regional, function(x) sum(is.na(x))), decreasing = TRUE)

#Menunjukkan persentase data yang hilang dari masing-masing dataset
plot_missing(province,ggtheme=theme_bw())
plot_missing(regional,ggtheme=theme_bw())

#Data preparation
#Menghapus kolom Region_Code, Province_Code, dan Province_GeoCode dari dataset province
province<-province[,-2]
province<-province[,-3]
province<-province[,-4]

#Menghapus kolom Region_Code dari dataset regional
regional<-regional[,-2]

#Melakukan pengecekan missing data setelah dilakukan penghapusan kolom
plot_missing(province,ggtheme=theme_bw())
plot_missing(regional,ggtheme=theme_bw())

#Melakukan penghapusan waktu dari kolom Date
province$Date<-substr(province$Date,1,10)
regional$Date<-substr(regional$Date,1,10)

#Melakukan pengubahan tipe data Date dari string menjadi date
province$Date <- as.Date(province$Date, format = '%Y-%m-%d')
regional$Date <- as.Date(regional$Date, format = '%Y-%m-%d')

#Melakukan agregasi data regional untuk mendapat data nasional
national<-dplyr::group_by(regional, Date) %>% dplyr::summarise(Hospitalized_with_Symptoms=sum(Hospitalized_with_Symptoms),
                                                               Intensive_Care=sum(Intensive_Care),
                                                               TotalHosp=sum(Total_Hospitalized),
                                                               Tot_Home_Isolation=sum(Tot_Home_Isolation),
                                                               Total_Actually_Positive=sum(Total_Actually_Positive),
                                                               New_Actually_Positive=sum(New_Actually_Positive),
                                                               Healed=sum(Healed),
                                                               Deceased=sum(Deceased),
                                                               Total_Cases=sum(Total_Cases),
                                                               Total_Medical_Swabs=sum(Total_Medical_Swabs))

#Melakukan perhitungan rasio kematian dan kesembuhan
national$HealedRatio<-national$Healed/national$Total_Cases
national$DeceasedRatio<-national$Deceased/national$Total_Cases

#Melakukan plotting terhadap jumlah kasus total Corona di Italia hingga 23 April 2020
ggplot(national,aes(x=Date,y=Total_Cases,fill=Date))+
  geom_bar(stat="identity")+
  labs(title = "Total Kasus COVID-19 di Italia hingga 23 April 2020")

#Melakukan plotting terhadap jumlah kasus baru tiap harinya
ggplot(national,aes(x=Date,y=New_Actually_Positive,fill=Date))+
  geom_bar(stat="identity")+
  labs(title = "Penambahan Kasus Baru COVID-19 di Italia hingga 23 April 2020",hjust=0.5)

#Menunjukkan perbandingan antara rasio kematian dan kesembuhan
ggplot(national, aes(x=HealedRatio, y=DeceasedRatio, size=Total_Cases, color=Date)) +
  geom_point(alpha=0.9) + ggtitle('Perbandingan Rasio Kematian dengan Rasio Kesembuhan Pasien COVID-19 di Italia') + theme(legend.position="bottom")

#Menunjukkan perbandingan jumlah pasien IGD dengan pasien total
ggplot(national, aes(x=Date, y=TotalHosp),fill="yellow") +
  geom_area(fill="yellow") + 
  geom_area(aes(x=Date,y=Intensive_Care), data=national,fill="blue")+
  ggtitle('Perbandingan Antara Pasien ICU dengan Pasien Total') + theme(legend.position="bottom")

#Menunjukkan Pembagian Dataset Total Kasus COVID-19 di Italia
national %>%
  ggplot(aes(x = Date, y = Total_Cases)) +
  geom_rect(xmin = as.numeric(ymd("2020-04-10")),
            xmax = as.numeric(ymd("2020-05-23")),
            ymin = -10000, ymax = 200000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-24"), y = 150000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2020-04-17"), y = 30000,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Pembagian Dataset Total Kasus COVID-19 di Italia", x = "") +
  geom_vline(xintercept=2020-03-04, color="Green")+
  geom_vline(xintercept=14, color="Orange")+
  geom_vline(xintercept=15, color="red")+
  geom_vline(xintercept=17, color="black")+
  geom_vline(xintercept=28, color="purple")+
  theme_tq()

#Melakukan pembagian dataset menjadi dataset training dan test
train_tbl <- national %>% filter(Date < ymd("2020-04-10"))
test_tbl  <- national %>% filter(Date >= ymd("2020-04-10"))

train_tbl<-train_tbl[,-2:-9]
train_tbl<-train_tbl[,-3:-5]
test_tbl<-test_tbl[,-2:-9]
test_tbl<-test_tbl[,-3:-5]

#Melakukan proses preprocessing data Time Series
recipe_spec_timeseries <- recipe(Total_Cases ~ ., data = train_tbl) %>%
  step_timeseries_signature(Date) 

bake(prep(recipe_spec_timeseries), new_data = train_tbl)

recipe_spec_final <- recipe_spec_timeseries %>%
  step_rm(Date) %>%
  step_rm(contains("iso"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num")) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

bake(prep(recipe_spec_final), new_data = train_tbl)

#Melakukan proses pembentukan model Time Series Regression
model_spec_glmnet <- linear_reg(mode = "regression") %>%
  set_engine("lm")

workflow_glmnet <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_glmnet)

workflow_glmnet

#Melakukan training model pada dataset Training
workflow_trained <- workflow_glmnet %>% fit(data = train_tbl)

#Melakukan prediksi pada dataset Test
prediction_tbl <- workflow_trained %>% 
  predict(test_tbl) %>%
  bind_cols(test_tbl) 

prediction_tbl

halfdata<-FULLDATA[-61:-90,]
halfdata<-halfdata[-1:-46,]
halfdata2<-halfdata[-47:-90,]

#Melakukan Visualisasi untuk melakukan perbandingan antara data real terhadap data hasil prediksi
ggplot(aes(x = Date), data = national) +
  geom_rect(xmin = as.numeric(ymd("2020-04-10")),
            xmax = as.numeric(ymd("2020-05-23")),
            ymin = -10000, ymax = 300000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-24"), y = 150000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2020-04-17"), y = 30000,
           color = palette_light()[[1]], label = "Test Region") + 
  geom_point(aes(x = Date, y = Total_Cases),  
              alpha = 0.5) +
  geom_smooth(aes(x = Date, y = .pred), data = prediction2_tbl,
              method = 'loess', color="red")+
  labs(title = "Visualisasi Hasil Regresi Pada Training Set", x = "", y="Total_Cases")+
theme_tq()

#Melakukan proses pengecekan error model
data.frame(
  RMSE = RMSE(prediction_tbl$Total_Cases,prediction_tbl$.pred),
  R2 = R2(prediction_tbl$Total_Cases,prediction_tbl$.pred),
  MAPE = MAPE(prediction_tbl$Total_Cases,prediction_tbl$.pred)
)

#Melakukan proses prediksi terhadap jumlah total kasus COVID-19 selama 30 hari ke depan
idx <- national %>% tk_index()
national_summary <- idx %>% tk_get_timeseries_summary()

national_summary[7:12]

idx_future <- idx %>% tk_make_future_timeseries(n_future = 30)

future_tbl <- tibble(Date = idx_future) 

future_tbl

future_predictions_tbl <- workflow_glmnet %>% 
  fit(data = national) %>%
  predict(future_tbl) %>%
  bind_cols(future_tbl)

#Melakukan visualisasi terhadap data forecast selama 30 hari setelah data terakhir
national %>%
  ggplot(aes(x = Date, y = Total_Cases)) +
  geom_rect(xmin = as.numeric(ymd("2020-04-10")),
            xmax = as.numeric(ymd("2020-04-23")),
            ymin = -20000, ymax = 500000,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2020-04-23")),
            xmax = as.numeric(ymd("2020-05-30")),
            ymin = -20000, ymax = 500000,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-24"), y = 150000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2020-04-17"), y = 30000,
           color = palette_light()[[1]], label = "Test\n Region") +
  annotate("text", x = ymd("2020-05-10"), y = 30000,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  # future data
  geom_point(aes(x = Date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = Date, y = .pred), data = future_predictions_tbl,
              method = 'loess') + 
  labs(title = "Total Kasus COVID-19 di Italia Hingga Tanggal 23 Mei 2020", x = "") +
  theme_tq()

#Melakukan perhitungan error dari data forecast
stdev<-sd(prediction_tbl$Total_Cases-prediction_tbl$.pred)

future_predictions_tbl <- future_predictions_tbl %>%
  mutate(
    lo.95 = .pred - 1.96 * stdev,
    lo.80 = .pred - 1.28 * stdev,
    hi.80 = .pred + 1.28 * stdev,
    hi.95 = .pred + 1.96 * stdev
  )

#Melakukan visualisasi forecast dengan error
national %>%
  ggplot(aes(x = Date, y = Total_Cases)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_ribbon(aes(y = .pred, ymin = lo.95, ymax = hi.95), 
              data = future_predictions_tbl, 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(y = .pred, ymin = lo.80, ymax = hi.80, fill = key), 
              data = future_predictions_tbl,
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point(aes(x = Date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = Date, y = .pred), data = future_predictions_tbl,
              method = 'loess', color = "white") + 
  labs(title = "Total Kasus COVID-19 di Italia Hingga 23 Mei 2020 dengan Confidence Interval", x = "") +
  theme_tq()


fpt3<-future_predictions_tbl[,-3:-6]
fpt<-fpt3[,-1]
fpt2<-fpt3[,-2]

fptasli<-cbind(fpt,fpt2)
fptasli<-fptasli %>% 
  rename(
    Total_Cases = .pred
  )
national_tbl<-national[,-2:-9]
national_tbl<-national_tbl[,-3:-5]
FULLDATA<-rbind(national_tbl,fptasli)

#Model Logistic Growth
tcs<-FULLDATA[,2]
tcs %>% pull('Total_Cases')
tcs<-as.numeric(unlist(tcs))
times2<-c(0:89)
SS<-getInitial(tcs~SSlogis(times2,alpha,xmid,scale),data=data.frame(tcs=tcs,times2=times2))

K_start<-SS["alpha"]
R_start<-1/SS["scale"]
N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)

log_formula<-formula(tcs~K*N0*exp(R*times2)/(K+N0*(exp(R*times2)-1)))

m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start))

summary(m)

cor(tcs,predict(m))

PREDICTED<-PREDICTED[-1:-46]
PREDICTED2<-predict(m)[-61:-90]

LOGGROWTH<-data.frame(national_tbl,PREDICTED2)

data.frame(
  RMSE = RMSE(PREDICTED, test_tbl2$Total_Cases),
  R2 = R2(PREDICTED, test_tbl2$Total_Cases),
  MAPE = MAPE(PREDICTED, test_tbl2$Total_Cases)
)

#Melakukan visualisasi
plot(times2,tcs)
lines(times2,predict(m),col="red",lty=2,lwd=3)

logmodel<-predict(m)
FULLDATA
FULLDATA$LogPrediction<-logmodel
  
predictiondata<-FULLDATA[-1:-60,]

#Visualisasi prediksi menggunakan logistic growth model
national %>%
  ggplot(aes(x = Date, y = Total_Cases)) +
  geom_rect(xmin = as.numeric(ymd("2020-04-10")),
            xmax = as.numeric(ymd("2020-04-23")),
            ymin = -20000, ymax = 500000,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2020-04-23")),
            xmax = as.numeric(ymd("2020-05-30")),
            ymin = -20000, ymax = 500000,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-24"), y = 150000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2020-04-17"), y = 30000,
           color = palette_light()[[1]], label = "Test\n Region") +
  annotate("text", x = ymd("2020-05-10"), y = 30000,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  # future data
  geom_smooth(aes(x = Date, y = LogPrediction), data = FULLDATA,
              method = 'loess', color="red") + 
  labs(title = "Total Kasus COVID-19 di Italia Hingga Tanggal 23 Mei 2020 Menggunakan Logistic Growth Model", x = "") +
  theme_tq()

#Membandingkan model time series dengan logistic growth
ggplot(aes(x = Date), data = predictiondata) +
  geom_rect(xmin = as.numeric(ymd("2020-04-24")),
            xmax = as.numeric(ymd("2020-06-12")),
            ymin = -200000, ymax = 300000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-03-24"), y = 180000,
           color = palette_light()[[1]], label = "Daerah Data Aktual") +
  annotate("text", x = ymd("2020-05-10"), y = 30000,
           color = palette_light()[[1]], label = "Daerah Prediksi") + 
  geom_point(aes(x=Date, y=Total_Cases), data=national, alpha = 0.5, color = palette_light()[[1]])+
  geom_smooth(aes(x = Date, y = Total_Cases),  
              alpha = 0.5, color = "green") +
  geom_smooth(aes(x = Date, y = LogPrediction), data = predictiondata,
              method = 'loess')+
  labs(title = "Perbandingan Hasil Prediksi Menggunakan Time Series Regression dan Logistic Growth", x = "", y="Total_Cases")
theme_tq() 

#Model prediksi penambahan kasus baru
model <- lm(New_Actually_Positive ~ poly(Num, 2, raw = TRUE), data = train_tbl2)

predictions <- model %>% predict(test_tbl2)

data.frame(
  RMSE = RMSE(predictions, test_tbl2$New_Actually_Positive),
  R2 = R2(predictions, test_tbl2$New_Actually_Positive),
  MAPE = MAPE(predictions, test_tbl2$New_Actually_Positive)
)

mixtable<-rbind(train_tbl2,test_tbl2)

#Visualisasi tanggal penting
ggplot(mixtable, aes(Num, New_Actually_Positive) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))+
  geom_vline(xintercept=10, color="Green")+
  geom_vline(xintercept=14, color="Orange")+
  geom_vline(xintercept=15, color="red")+
  geom_vline(xintercept=17, color="black")+
  geom_vline(xintercept=28, color="purple")+
  labs(title = "Hasil Regresi Terhadap Penambahan Kasus COVID-19 di Italia", x = "Number of Days after first infection", y="New_Actually_Positive")

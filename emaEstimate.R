library(httr)
library(readr)
library(dplyr)
library(ggplot2)

### Defino url, fecha inicial del dataset, datasets a bajar y columnas a mantener
url = 'https://charts.alphacast.io/api/datasets/'
fechaInicio = '2004-01-01'

df = tibble(
  dataset = c('adefa', 'portland', 'mecon', 'merval', 'bcra', 'bcra_arg', 'utdt', 'emae'),
  name = c('5600', '5537', '5632', '5816', '6054', '5277', '5596', '5331'),
  columnsToKeep = list(
    list('Year', 'Production - Cars - sa', 'Exports - Cars - sa', 'Sales to domestic market - Total', 'Sales to domestic market - National vehicles'),
    list('Year','Cement Dispatches - Total'),
    list('Year','GANANCIAS - sa', 'GANANCIAS DGI - sa', 'GANANCIAS DGA - sa', 'IVA - sa', 'IVA DGI - sa'),
    list('Year','MERVAL'),
    list('Year','Tasas de interés, promedio mensual en porcentaje nominal anual - Por depósitosa plazo fijo de 30 a 59 días de plazo - De moneda nacional'),
    list('Year','Private M2 - sa'),
    list('Year','Indice de Confianza del Consumidor (ICC) - Nacional'),
    list('Year','Emae - sa_orig')
  )
)

### Baja todos los datasets y los pega en una lista
ds = list()
i = 1
for (dataset in df$name) {
  ds[[df$dataset[i]]] = read_csv(
    rawToChar(
      GET(
        url = paste0(url, df$name[i], '.csv'),
                     authenticate(Sys.getenv('alphacast'), '')
        )$content)
    ) %>% select(
      unlist(df$columnsToKeep[[i]]) ) %>% filter(Year >= fechaInicio)
  i = i + 1
}

### Cambiar las fechas al 1 de cada mes para el df bcra que informa el último día del mes
ds$bcra$Year =  lubridate::floor_date(as.Date(ds$bcra$Year), "month")

### construyo el df con todos
full = Reduce(function (d1, d2) merge(d1, d2, by = "Year", all.x = TRUE, all.y = FALSE), ds)
colnames(full) = c('Year',
                  'Produccion Automoviles Total (sa)',
                  'Exportacion Automoviles (sa)',
                  'Ventas Totales Mdo Domestico (sa)',
                  'Ventas de Vehiculos Nacionales Mdo Domestico',
                  'Despachos de Cemento',
                  'Ganancias (sa)',
                  'Ganancias DGI (sa)',
                  'Ganancias DGA (sa)',
                  'IVA (sa)',
                  'IVA DGI (sa)',
                  'Merval',
                  'Tasa de Interes',
                  'Private M2 (sa)',
                  'Indice de Confianza del Consumidor',
                  'EMAE (sa)')


full %>% 
  select(`Tasa de Interes`, `EMAE (sa)`) %>% 
  ggplot(
    aes(x = `Tasa de Interes`, y = `EMAE (sa)`)) + geom_point()


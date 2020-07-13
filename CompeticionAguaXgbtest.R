### FUNCIONES
#estas son funciones de debug. Cortesía de Ange. 
existeColumna <- function(nombreColumna, tabla)
{
  return(nombreColumna %in% colnames(tabla))
}

printExisteColumna <- function(nombreColumna, tabla)
{
  if(existeColumna(nombreColumna, tabla))
  {
    print(paste("Existe ", nombreColumna ))
  }
  else
  {
    print(paste("NO existe ", nombreColumna ))
  }
}
  

#Escribimos un dumificador flexible para convertir columnas
dummyGen <- function(nueva_tabla, tabla_original, columna_dumificada, valores_columna){
  
  nombre_nuevo <- paste(columna_dumificada, "_", sep = "")
  
  i <- 1
  for (val in valores_columna){
    nueva_variable <- data.frame(matrix(0, nrow(nueva_tabla), 1))
    nueva_variable[tabla_original[,columna_dumificada] == val, 1] <- 1
    colnames(nueva_variable) <- paste0(nombre_nuevo, i)
    nueva_tabla <- cbind(nueva_tabla,nueva_variable)
    i <- i + 1
  }
  
  return(nueva_tabla)
}

limpiaDatosAgua <- function(tabla_raw){
  
  #lo primero es crear una tabla donde ir guardando el res
  nueva_tabla <- data.frame(matrix(0, nrow(tabla_raw), 1))
  #nombre_columna_dummy <- colnames(nueva_tabla)
  #ID: la primera variable es el id. Se mantiene tal cual. 
  
  nueva_tabla$id <- tabla_raw$id

  #se genera una columna no deseada al comienzo del dataset 
  #nueva_tabla <- nueva_tabla[,-1]
  
  #amount_tsh: la siguiente variable. La conversion ha de ser logarítimica. Además, hemos
  #de eliminar los valores 0, los convertiremos en 1. 
  
  nueva_tabla$amount_tsh     <- tabla_raw$amount_tsh
  #convertimos los valores 0 en 1
  indicesCero <- trainingsetvalues$amount_tsh == 0
  nueva_tabla$amount_tsh[indicesCero] <- nueva_tabla$amount_tsh[indicesCero] + 1
  
  nueva_tabla$cantidad <- log10(tabla_raw$amount_tsh + 1)
  
  nueva_tabla$cantidad_sq <- nueva_tabla$cantidad * nueva_tabla$cantidad
  
  nueva_tabla$cantidad_q3 <- 0
  nueva_tabla$cantidad_q3[nueva_tabla$cantidad_sq > quantile(nueva_tabla$cantidad_sq)[[4]][[1]]] <- 1
  
  #date_recorded. de toda la informacion que se podria sacar de esta columna, nos quedamos con 
  #el year que se recogió
  
  #convertimos a un formato de fechas con el que podamos trabajar.
  fechas   <- as.Date(tabla_raw$date_recorded, "%Y-%m-%d")
  
  
  fechaslt <- as.POSIXlt(fechas)
  
  #generamos las columnas dummy para los anios
  nueva_tabla$y1  <- 0
  nueva_tabla$y2  <- 0
  nueva_tabla$y3  <- 0
  
  #generamos las columnas dummy para los meses
  nueva_tabla$m01 <- 0
  nueva_tabla$m02 <- 0
  nueva_tabla$m03 <- 0
  nueva_tabla$m04 <- 0
  nueva_tabla$m05 <- 0
  nueva_tabla$m06 <- 0
  nueva_tabla$m07 <- 0
  nueva_tabla$m08 <- 0
  nueva_tabla$m09 <- 0
  nueva_tabla$m10 <- 0
  nueva_tabla$m11 <- 0
  nueva_tabla$m12 <- 0
  
  
  
  i<-1
  
  #escribimos un bucle para los anios que queremos rescatar.
  
  #al transformar las fechas a formato POSIX tenemos el diferencial con respecto a 1900
  #esto quiere decir, que 2011 sería 2011 - 1900 = 111. Asi sucesivamente.
  
  #segun el formato fecha POSIX, fechaslt$year da el anio solamente. 
  for (anio in fechaslt$year){
    if (anio == 111) {
      nueva_tabla[i, "y1"] <- 1
    }
    else if (anio == 112){
      nueva_tabla[i, "y2"] <- 1
    }
    else if(anio == 113){
      nueva_tabla[i, "y3"] <- 1
    }
    i <- i +1
  }

  i<- 1
  for(mes in fechaslt$mon){
    if(mes == 0){
      nueva_tabla[i,"m01"] <- 1
    }
    else if(mes == 1){
      nueva_tabla[i,"m02"] <- 1
    }
    else if(mes == 2){
      nueva_tabla[i,"m03"] <- 1
    }
    else if(mes == 3){
      nueva_tabla[i,"m04"] <- 1
    }
    else if(mes == 4){
      nueva_tabla[i,"m05"] <- 1
    }
    else if(mes == 5){
      nueva_tabla[i,"m06"] <- 1
    }
    else if(mes == 6){
      nueva_tabla[i,"m07"] <- 1
    }
    else if(mes == 7){
      nueva_tabla[i,"m08"] <- 1
    }
    else if(mes == 8){
      nueva_tabla[i,"m09"] <- 1
    }
    else if(mes == 9){
      nueva_tabla[i,"m10"] <- 1
    }
    else if(mes == 10){
      nueva_tabla[i,"m11"] <- 1
    }
    else if(mes == 11){
      nueva_tabla[i,"m12"] <- 1
    }
    i <- i + 1
  }
  
  #Anio de construccion. Son muchos anios posibles. De ahi que lo dividamos en cuatro grupos
  
  #definimos una serie de categorias
  limites <- seq(from = 1960, to = 2010, by = 10)
  
  i <- 1
  
  for (limite in c(0,limites)) {
       
        nueva_variable <- data.frame(matrix(0, nrow(nueva_tabla), 1))
        j <- 1
        if (i == 1){
          for(anio in tabla_raw$construction_year)
          {
            if(anio < limites[1]) 
            {
              nueva_variable[j,] <- 1
            }
            j<- j +1
          }
        }
        else if (i == (length(limites) +1)){

          for(anio in tabla_raw$construction_year)
          {
            if( anio >= limite ) 
            {
              nueva_variable[j,] <- 1
            }
            j<- j +1
          }
        }
        
        else {
          for(anio in tabla_raw$construction_year)
          {
            if( anio >= limite && anio < limites[i] ) 
            {
              nueva_variable[j,] <- 1
            }
            j<- j +1
          }
        }
        colnames(nueva_variable) <- paste0("construction_year", limite)
        nueva_tabla <- cbind(nueva_tabla,nueva_variable)
        i <- i + 1
  }
  
  
  #extraction_type
  
  tipo_extraccion <- c("ksb",
                       "afridev",
                       "india mark ii",
                       "mono",
                       "swn 80",
                       "submersible",
                       "other",
                       "nira/tanira",
                       "gravity")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "extraction_type", tipo_extraccion)
  
  #source
  
  fuente <- c("spring",
              "rainwater harvesting",
              "dam",
              "machine dbh",     
              "other",
              "shallow well",
              "river",
              "hand dtw",            
              "lake",
              "unknown")
  
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"source",fuente)

  
  #basin
  
  basin <- c("internal",
             "lake nyasa",
             "lake rukwa",             
             "lake tanganyika",
             "lake victoria",
             "pangani",                
             "rufiji",
             "ruvuma / southern coast",
             "wami / ruvu")
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"basin",basin)
  
  region <- c("Arusha",
              "Dodoma",
              "Iringa",
              "Kagera",
              "Kigoma", 
              "Kilimanjaro",
              "Lindi",
              "Manyara",
              "Mara",
              "Mbeya",
              "Morogoro", 
              "Mtwara",
              "Mwanza",
              "Pwani",
              "Rukwa",
              "Ruvuma",
              "Shinyanga", 
              "Singida",
              "Tabora",
              "Tanga")
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"region",region)
  
  #scheme_management
  
  esquema_manage <- c("Company",
                      "Parastatal",
                      "Private operator", 
                      "VWC",
                      "Water authority",
                      "Water Board",
                      "WUA", 
                      "WUG")
  
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"scheme_management",esquema_manage)
  
  #extraction_type
  
  extracciones <- c("afridev",
                    "gravity",
                    "india mark ii", 
                    "mono",
                    "nira/tanira",
                    "other",
                    "submersible", 
                    "swn 80")
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"extraction_type",extracciones)
  
  #payment
  
  pago <-c("never pay",
           "other",
           "pay annually",
           "pay monthly",
           "pay per bucket", 
           "pay when scheme fails",
           "unknown")
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"payment",pago)
  
  #payment_type
  
  tipo_pago <- c("never pay",
                 "per bucket",
                 "monthly",
                 "unknown",
                 "on failure", 
                 "annually",
                 "other")
  
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"payment_type",tipo_pago)
  
  #water_quality
  
  calidad_agua <- c("soft", "salty", "unknown", "milky", "coloured")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "water_quality",calidad_agua)

  #quality_group
                          
  grupo_calidad_agua <- c("communal standpipe",
                          "hand pump",
                          "other",
                          "improved spring")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "quality_group", grupo_calidad_agua)
  
  #quantity
  
  cantidad <- c("enough",
                "insufficient",
                "dry",
                "seasonal",
                "unknown")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "quantity", cantidad)
  
  #scheme_name
  
  nombre_esquema <- c("",
                      "(Other)",
                      "K",
                      "None",
                      "Borehole",
                      "Chalinze wate",
                      "M", 
                      "DANIDA",
                      "Government")
  
  nueva_tabla <- dummyGen(nueva_tabla,tabla_raw,"scheme_name",nombre_esquema)
  
  #ward
  
  ward <- c("(Other)","Igosi", "Imalinyi", "Siha Kati", "Mdandu", "Nduruma", 
            "Kitunda", "Mishamo", "Msindo", "Chalinze", "Maji ya Chai", "Usuka", 
            "Ngarenanyuki", "Chanika", "Vikindu", "Mtwango", "Matola", "Zinga/Ikerege", 
            "Maramba", "Wanging'ombe", "Itete", "Magomeni", "Ifakara", "Kikatiti", 
            "Olkokola", "Maposeni", "Igongolo", "Mvomero", "Mlangali", "Nkoma", 
            "Mahongole", "Nkungulu", "Rujewa", "Simbo", "Masama Magharibi", 
            "Kiranyi", "Mamire", "Kidatu", "Lupalilo", "Ihanda", "Kagongo", 
            "Hedaru", "Chinamili", "Isongole", "Siha Mashariki", "Masama Mashariki", 
            "Mkongo", "Makuyuni", "Tinde", "Yombo", "Kanga", "Bugarama", 
            "Kimochi", "Makwale", "Diongoya", "Malindi", "Soga", "Mabwerebwere", 
            "Mahembe")
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "ward", ward)
  
  #funder
  
  fundador <- c("(Other)", "Government Of Tanzania", "", "Danida", "Hesawa", 
                "Rwssp", "World Bank", "Kkkt", "World Vision", "Unicef", "Tasaf", 
                "District Council", "Dhv", "Private Individual", "Dwsp", "0", 
                "Norad", "Germany Republi", "Tcrs", "Ministry Of Water", "Water", 
                "Dwe", "Netherlands", "Hifab", "Adb", "Lga", "Amref", "Fini Water", 
                "Oxfam", "Wateraid", "Rc Church", "Isf", "Rudep", "Mission", 
                "Private", "Jaica", "Roman", "Rural Water Supply And Sanitat", 
                "Adra", "Ces(gmbh)", "Jica", "Shipo", "Wsdp", "Rc", "Finw", "Dh", 
                "Ded", "Plan Int", "Kiliwater", "Dmdd", "Go", "Lawatefuka Water Supply", 
                "Oxfarm", "Magadini-makiwaru Water", "Fw", "W.B", "Kkkt_makwale", 
                "Ces (gmbh)", "Wvt", "Oikos E.Afrika", "Nethalan", "Mkinga Distric Coun")
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "funder", fundador)
  
  instalador <- c("DWE", "(Other)", "", "Government", "RWE", "Commu", "DANIDA", 
                  "KKKT", "Hesawa", "0", "TCRS", "Central government", "CES", "Community", 
                  "DANID", "District Council", "HESAWA", "LGA", "World vision", 
                  "WEDECO", "TASAF", "District council", "Gover", "AMREF", "TWESA", 
                  "WU", "Dmdd", "ACRA", "World Vision", "SEMA", "DW", "OXFAM", 
                  "Da", "Gove", "Idara ya maji", "UNICEF", "Sengerema Water Department", 
                  "Kiliwater", "FinW", "NORAD", "DH", "Villagers")
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "installer", instalador)
   
   #management_group
  
  grupo_managers <- c("parastatal",
                      "user-group",
                      "other",
                      "commerical",
                      "unknown")
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "management_group",grupo_managers)
  
  #management
  
  managers   <- c("parastatal",
                  "vwc",
                  "water board",
                  "other - school",
                  "wug",
                  "wua",
                  "private operator",
                  "company",
                  "other",
                  "water authority",
                  "unknown",
                  "trust")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "management", managers)
  
  #subvillage
  
  poblados <- c("(Other)", "Madukani", "Shuleni", "Majengo", "Kati", "", "Mtakuja", 
                "Sokoni", "M", "Muungano", "Mbuyuni", "Mlimani", "Songambele", 
                "Miembeni", "Msikitini", "1", "Kibaoni", "Kanisani", "I", "Mapinduzi", 
                "Mjimwema", "Mjini", "Mkwajuni", "Mwenge", "Azimio", "Mabatini", 
                "Mbugani", "Mission", "Bwawani", "Bondeni", "Chang'Ombe", "Zahanati", 
                "Kichangani", "Mtaa Wa Kitunda Kati", "Senta", "Misufini", "Center", 
                "Nyerere", "Amani", "K", "Kawawa", "Kisiwani", "Maendeleo", "Ccm", 
                "Ikulu", "Ilala", "Ushirika", "Gulioni", "Marurani Kati", "Mtaa Wa Kivule", 
                "Shule", "Darajani", "Uzunguni", "Barabarani", "Magharibi", "Marurani Juu", 
                "Sokoine", "Mpakani", "Dodoma", "Msufini", "Kaloleni", "Magomeni")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "subvillage", poblados)
  
  #wpt_name
  
  nombre_wpt <- c("(Other)", "none", "Shuleni", "Zahanati", "Msikitini", "Kanisani", 
                  "Bombani", "Sokoni", "Ofisini", "School", "Shule Ya Msingi", 
                  "Shule", "Sekondari", "Muungano", "Mkombozi", "Madukani", "Hospital", 
                  "Mbugani", "Upendo", "Kituo Cha Afya", "Mkuyuni", "Umoja", "Center", 
                  "Ccm", "Kisimani", "Mtakuja", "Ofisi Ya Kijiji", "Tankini", "Bwawani", 
                  "Songambele", "Maendeleo", "Bondeni", "Mbuyuni", "Uwanjani", 
                  "Kilabuni", "Mnadani", "Kijiweni", "Miembeni", "Secondary", "Amani", 
                  "Majengo", "Rc Church", "Shuleni Sekondari", "Tank La Shule", 
                  "Church", "Mwembeni", "Darajani", "Dispensary", "Mashineni", 
                  "Kwa John", "Kwa Juma", "Mahakamani", "Mtoni", "Tenkini", "Kwa Joseph", 
                  "Tangini", "Tupendane")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "wpt_name", nombre_wpt)
  
  #lga
  
  lga <- c("(Other)", "Njombe", "Arusha Rural", "Moshi Rural", "Bariadi", 
           "Rungwe", "Kilosa", "Kasulu", "Mbozi", "Meru", "Bagamoyo", "Singida Rural", 
           "Kilombero", "Same", "Kibondo", "Kyela", "Kahama", "Kigoma Rural", 
           "Magu", "Maswa", "Karagwe", "Mbinga", "Iringa Rural", "Serengeti", 
           "Lushoto", "Namtumbo", "Songea Rural", "Mpanda", "Mvomero", "Ngara", 
           "Ulanga", "Makete", "Kwimba", "Mbarali", "Hai", "Rombo", "Shinyanga Rural", 
           "Nzega", "Ludewa", "Mkuranga", "Iramba", "Masasi", "Kondoa", 
           "Morogoro Rural", "Sumbawanga Rural", "Mufindi", "Mwanga", "Bukombe", 
           "Babati", "Ilala", "Geita", "Bukoba Rural", "Mbeya Rural", "Meatu", 
           "Rufiji", "Bunda", "Siha", "Nkasi", "Mtwara Rural", "Tunduru", 
           "Korogwe", "Biharamulo", "Muleba", "Kishapu", "Musoma Rural", 
           "Kilwa", "Lindi Rural", "Mpwapwa", "Urambo", "Manyoni", "Kongwa", 
           "Dodoma Urban", "Kilolo", "Missungwi", "Chamwino", "Ukerewe", 
           "Uyui", "Igunga", "Muheza", "Sengerema", "Karatu", "Longido", 
           "Simanjiro", "Pangani", "Nachingwea")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "lga", lga)
  
  #public_meeting
  public_meeting <- c("False", "True", "")
  
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "public_meeting",public_meeting)
  
  #permit
  permit <- c("False", "True", "")
  nueva_tabla <-dummyGen(nueva_tabla,tabla_raw,"permit",permit)
  
  return(nueva_tabla)
}


### MAIN

trainingsetvalues  <- read.csv("D:/Universidad/master/documentacion/Master BDBA/AplicacionesBigData/trainingsetvalues.csv")
#trainingsetvalues <- read.csv("C:/Users/JaqDe/Desktop/MBDA/Master BDBA/AplicacionesBigData/trainingsetvalues.csv")

#Eliminamos columnas de poca importancia
data <- trainingsetvalues[,-10] #eliminamos la columna num_private. No contiene informacion. 


datos_limpios <-limpiaDatosAgua(trainingsetvalues)


#### AÑADIMOS LOS LABELS DEL TRAINING SET AL DATASET

labels_train  <- read.csv("D:/Universidad/master/documentacion/Master BDBA/AplicacionesBigData/trainingsetlabels.csv")
target_labels <- labels_train[,2]

target_labels <- rep(0, nrow(labels_train))
target_labels[labels_train$status_group == levels(labels_train$status_group)[2]] <- 1
target_labels[labels_train$status_group == levels(labels_train$status_group)[3]] <- 2

target_labels <- as.factor(target_labels)

datos_limpios <- cbind(datos_limpios,target_labels)




### IMPORTAMOS XGBOOST

datos_limpios <- datos_limpios[,-1]

library(xgboost)
library(caret)

set.seed(12345)

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,5000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(make.names(target_labels)~.,data=datos_limpios,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)



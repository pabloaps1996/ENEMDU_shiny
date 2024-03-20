if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}

pacman::p_load(shiny, shinyjs, shinycssloaders, shinythemes, DT, plotly, lubridate,
               glue, tidyr, tidyverse, data.table, magrittr, dplyr, purrr, openxlsx, readxl, 
               zoo, aweek, stringr, shinydashboard, ggplot2, shinyWidgets, gapminder,rio,sf,leaflet,kableExtra)

# 1. required packages ----------------------------------------------------------
library(cicerone) # for guided tours}
library(shiny) # shiny functions
library(shinyBS) # modals
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(plotly) # interactive graphs
library(leaflet) #javascript maps
library(shinyWidgets) # for extra widgets
library(shinyjs) # for using javascript
library(rintrojs) # for introbox in summary tab
library(lubridate) # for automated list of dates in welcome modal
library(shinycssloaders) # for loading icons
library(webshot) # to download plotly charts
library(tidyr) # for pivoting
library(stringr) # for working with strings
library(reactable) # interactive tables
library(htmltools) # for using html tags
library(purrr) # for applying function to lists

if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}

###################### ESTIMACIÓN

# 2. required datafiles ------------------------------------------------------------

# main datasets 
data_estimacion <- import("data/data_estimacion.xlsx") # main dataset with indicator data
depr_data <- import("data/deprivation_data.xlsx") # deprivation/inequalities dataset

# lookups 
geo_lookup <- import("data/geo_lookup.xlsx") # geography lookup


#3. lists for filter dropdowns ------------------------------------------------------


hb_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="Estimación Puntual"])


indicator_list <- sort(unique(data_estimacion$indicator)) # for all other tabs


# indicators that contain gap years due to COVID-19
gap_indicators_ids <- c(21005,
                        20901)

# indicators_with_gap_years <- data_estimacion %>%
#   filter(ind_id %in% gap_indicators_ids) %>%
#   select(indicator) %>% unique() # come back to this - might be able to remove?




# scotpho profile names -----
profile_list <- setNames(c('HWB',
                           'CYP',
                           'ALC',
                           'DRG',
                           'MEN', 
                           'TOB', 
                           'POP'),
                         c('Health & wellbeing',
                           'Children & young people',
                           'Alcohol',
                           'Drugs',
                           'Mental Health', 
                           'Tobacco control', 
                           'Population'))



# common parameters for plots
xaxis_plots <- list(title = FALSE,
                    tickfont = list(size=14),
                    titlefont = list(size=14),
                    showline = TRUE,
                    tickangle = 270,
                    fixedrange=TRUE)


yaxis_plots <- list(title = FALSE,
                    rangemode="tozero",
                    fixedrange=TRUE,
                    size = 4,
                    tickfont = list(size=14),
                    titlefont = list(size=14))


font_plots <- list(family = '"Helvetica Neue",
                              Helvetica,
                              Arial,
                              sans-serif')



# 6. sourcing functions created for app (see functions folder) -------------------------------
list.files("functions") %>% 
  map(~ source(paste0("functions/", .)))


##END






###################### FIN


### lectura de datos 

base<-import("./insumos/tasas_12.Rdata")
provincias<- read_sf("./insumos/provincias.gpkg") %>% filter(!dpa_provin==90) %>% sf::as_Spatial()
es<-import("./insumos/cobertura.xlsx")
es<-import("./insumos/cobertura.xlsx")


base<- base %>% mutate(tasas= factor(tasas, c("TED", "TNR", "TRE","TNE")))  




redondear<- function(x){
  round(x,1)
}

es[,6:13]<- apply(es[,6:13],2,redondear)

es[,6:13]<- apply(es[,6:13],2,redondear)
es[,2:12]<- apply(es[,2:12],2,as.numeric)

#lectura de la información de cobertura

data_c<-import("./insumos/data2.xlsx")%>% rename(dpa_provin=DPA_PROVIN) %>% filter(!dpa_provin=="90")%>% 
  mutate(dpa_provin = as.numeric(dpa_provin),
         TNE_1=round(TNE,3)*100,
         TED_1=round(TED,3)*100,
         TNR_1=round(TNR,3)*100,
         TRE_1=round(TRE,3)*100,
         TRE=paste0(round(TRE,3)*100,"%"),
         TNR=paste0(round(TNR,3)*100,"%"),
         TNE=paste0(round(TNE,3)*100,"%"),
         TED=paste0(round(TED,3)*100,"%"), 
         Filtros= ifelse(Filtros=="1","TASA DE RESPONDIENDES (TRE)",
                         ifelse(Filtros=="2","TASA DE NO RESPONDIENTES (TNR)",
                                ifelse(Filtros=="3","TASA DE NO ELEGIBLES (TNE)",
                                       ifelse(Filtros=="4","TASA DE ELEGIBILIDAD DESCONOCIDA (TED)",NA))))) 



pal <- colorNumeric("Blues", NULL, na.color = "black")




base_fexp <- readRDS("./insumos/personas_fexp_cal_202402.rds")



base_fexp <- base_fexp%>%
  mutate(gedad = ifelse(edad<15, 1,
                        ifelse(edad>=15 , 2, NA)))

est_pob <- readRDS("./insumos/est_pob_v.rds")
vis<-import("./insumos/vis.xlsx")

wk <- readRDS("./insumos/wk_202402.rds")

text_tbl <- data.frame(
  Digit = c("00", "1", "1","1"),
  Description = c(
    "Dominio geográfico: nacional",
    "Área: urbana (1) o rural (2)", 
    "Sexo: hombre (1) mujer (2)",
    "Grupo de edad: menor a 15 años (1) 15 o más años (2)"
    
  )
)

text_tbl2 <- data.frame(
  Digit = c("00", "1", "1","1"),
  Description = c(
    "Dominio geográfico: nacional",
    "Área: urbana (1) o rural (2)", 
    "Sexo: hombre (1) mujer (2)",
    "Grupo de edad: menor a 15 años (1) 15 o más años (2)"
    
  )
)





### estadisticas de los factores
as<- import("./insumos/FEXP_ENEMDU_T_202402.rds")

teorico<-summary(as$fexp_teo)
ajustado<-summary(as$fexp_aju)
recortado<-summary(as$fexp_rec45)
calibrado<-summary(as$fexp)
nombres<-names(teorico)
teorico<-  matrix(teorico, nrow = 1, ncol = 6) %>% as.data.frame() 
ajustado<-  matrix(ajustado, nrow = 1, ncol = 6) %>% as.data.frame() 
recortado<-  matrix(recortado, nrow = 1, ncol = 6) %>% as.data.frame() 
calibrado<-  matrix(calibrado, nrow = 1, ncol = 6) %>% as.data.frame() 

colnames(teorico) <- nombres;colnames(ajustado) <- nombres;colnames(recortado) <- nombres;colnames(calibrado) <- nombres

teorico<-cbind(teorico,"sum_fexp"=sum(as$fexp_teo))
ajustado<-cbind(ajustado,"sum_fexp"=sum(as$fexp_aju))
recortado<-cbind(recortado,"sum_fexp"=sum(as$fexp_rec45))
calibrado<-cbind(calibrado,"sum_fexp"=sum(as$fexp))

#### Medidas de calidad

medidas1<-read_excel("./insumos/Medidas de calidad Silva.xlsx", sheet = 2)
medidas2<-read_excel("./insumos/Medidas de calidad Silva.xlsx", sheet = 3)
medidas3<-read_excel("./insumos/Medidas de calidad Silva.xlsx", sheet = 4)
medidas4<-read_excel("./insumos/Medidas de calidad Silva.xlsx", sheet = 5)
medidas5<-read_excel("./insumos/Medidas de calidad Silva.xlsx", sheet = 6)
medidas6<-read_excel("./insumos/Medidas de calidad Silva.xlsx", sheet = 7)
### estimaciones
estima <- readRDS("./insumos/Estimacion_Empleo.rds")
redondear<- function(x){
  round(x,2)
}
es1 <- estima %>% 
  select(Dominio=ngrupos,adec,adec_se,adec_low,adec_upp,adec_cv,adec_deff) %>% 
  filter(Dominio=="Nacional" | Dominio=="Urbana" | Dominio=="Rural")
es1[,2:7]<- apply(es1[,2:7], 2, redondear)

es2 <- estima %>% 
  select(Dominio=ngrupos,desem,desem_se,desem_low,desem_upp,desem_cv,desem_deff)%>% 
  filter(Dominio=="Nacional" | Dominio=="Urbana" | Dominio=="Rural")

es2[,2:7]<- apply(es2[,2:7], 2, redondear)

es3 <- estima %>% 
  select(Dominio=ngrupos,sub,sub_se,sub_low,sub_upp,sub_cv,sub_deff)%>% 
  filter(Dominio=="Nacional" | Dominio=="Urbana" | Dominio=="Rural")
es3[,2:7]<- apply(es3[,2:7], 2, redondear)

es4 <- estima %>% 
  select(Dominio=ngrupos,pobre,pobre_se,pobre_low,pobre_upp,pobre_cv,pobre_deff)%>% 
  filter(Dominio=="Nacional" | Dominio=="Urbana" | Dominio=="Rural")

es4[,2:7]<- apply(es4[,2:7], 2, redondear)



###################################### USER y SERVER
#User 
ui <- {fluidPage(
  shinythemes::themeSelector(),
  shinyjs::useShinyjs(),
  #Browser tab title:
  list(tags$head(HTML('<link rel="icon", href="image001.png",
                      type="image/png" />'))),
  div(style="padding: 1px 0px; width: '90%'",
      titlePanel(
        title="", windowTitle="My app title"
      )
  ),
  
  navbarPage(
    title=div(img(src = "image001.png", style = "margin-top: -10px;", height = 40),
              "DINEM"),
    #shinytheme. Currently using "united"
    #theme = shinytheme("united"),
    
    # Tab Panel
    
    tabPanel("COBERTURA",
             tabsetPanel(
               tabPanel("Por estrato",
                        fluidPage(
                          selectInput("estrato",
                                      "Escoja el estrato",
                                      selected = "BAJO",
                                      choices=c("BAJO", "MEDIO", "ALTO")),
                          
                          plotOutput('graf_1'))),
               tabPanel("Condición de la vivienda",
                        fluidPage(
                          titlePanel("Cobertura"),
                          selectInput("Filtros_1",
                                      "Escoja: ",
                                      selected = "TRE",
                                      choices=unique(data_c$Filtros)),
                          leafletOutput("MAP1"))),
               tabPanel("Cobertura muestral de viviendas",
                        fluidPage(
                          mainPanel(
                            tableOutput("mtcars_kable")
                          )
                        ))
               
             )
             
    ),
    
    tabPanel("FACTOR DE EXPANSIÓN",
             fluidPage(
               titlePanel("”RESUMEN ENEMDU 2024”"),
               theme = shinytheme("readable"),
               tableOutput("distPlot"),
               tableOutput("distPlot2"),
               fluidRow(
                 column (10 ,offset = 1,
                         plotOutput("distPlot3")
                 ),
                 column (10 ,offset = 1,
                         plotOutput("distPlot4"))),
               fluidRow(
                 column(9, offset =1,
                        plotOutput("distPlot5")),
                 column(1, offset = 1,
                        actionButton("button1",label="Ayuda", icon= icon('"list-alt"'), class ="btn-info"))      
               ),
               fluidRow(
                 column (9 ,offset = 1,
                         plotOutput("distPlot6")),
                 column(1, offset = 1,
                        actionButton("button2",label="Ayuda", icon= icon('"list-alt"'), class ="btn-info"))
               ),
               fluidRow(
                 column (9 ,offset = 1,
                         plotOutput("distPlot7")),
                 column(1, offset = 1,
                        actionButton("button3",label="Ayuda", icon= icon('"list-alt"'), class ="btn-info"))
               ),
               fluidRow(
                 sidebarPanel(width=12,
                              column(1), "TOTAL POBLACIÓN EXPANDIDA CON FACTOR DE EXPANSIÓN TEÓRICO, AJUSTADA Y ESTIMACIÓN POBLACIONAL.",
                              fluid = TRUE   ),
                 fluidRow(
                   fluidPage(
                     mainPanel(
                       tableOutput("teorico")))),
                 fluidRow(
                   fluidPage(
                     mainPanel(
                       tableOutput("ajustado")))),
                 fluidRow(
                   fluidPage(
                     mainPanel(
                       tableOutput("recortado")))),
                 fluidRow(
                   fluidPage(
                     mainPanel(
                       tableOutput("calibrado")))),
                 fluidRow(
                   column (9 ,offset = 1,
                           plotOutput("distPlot8")),
                   column(1, offset = 0,
                          actionButton("button4",label="Ayuda", icon= icon('"list-alt"'), class ="btn-info"))
                 ),
                 
                 tableOutput("distPlot9"),
                 tableOutput("distPlot10"),
                 
                 
                 fluidRow(
                   sidebarPanel(width=12,
                                column(4), "MEDIDAS DE CALIDAD DE SILVA A.G.",
                                fluid = TRUE   )),
                 tableOutput("m1"),
                 tableOutput("m2"),
                 tableOutput("m3"),
                 tableOutput("m4"),
                 tableOutput("m5"),
                 tableOutput("m6"),
               ),
               fluidRow(
                 sidebarPanel(width=12,
                              column(4), "MEDIDAS DE PRECISIÓN",
                              fluid = TRUE   )),
               tableOutput("e1"),
               tableOutput("e2"),
               tableOutput("e3"),
               tableOutput("e4"),
               
               
               
             )
    ),
    
    tabPanel("ESTIMACIONES",
             fluidPage(
               
               shinyUI(fluidPage(
                 
                 tabPanel(div(
                   div(class="fa fa-line-chart", role = "navigation"),
                   "Trend"),
                   sidebarPanel(width=4,
                                column(12,
                                       shiny::hr(),
                                       div(title="Selecciona los principales indicadores para la encuesta de EMPLEO",
                                           selectInput("indic_trend", shiny::HTML("<p>Paso 1. Seleccionar un indicador. <br/> <span style='font-weight: 400'>Presione la tecla  para buscar un indicador</span></p>"),
                                                       choices=indicator_list, selected = "Desempleo")),
                                       shiny::hr(),
                                       div(title="Dominios de representatividad de la ENEMDU puntual",
                                           p(tags$b("Paso 2. Seleccionar los dominios de representatividad."),
                                             p("Presione la tecla  para seleccionar los dominios de representatividad de la ENEMDU puntual")))),
                                column(6,
                                       selectizeInput("hbname_trend", "Estimación Puntual", choices = c("Select " = "", paste(hb_name)),
                                                      multiple=TRUE, selected = "")),
                                column(12,
                                       shiny::hr(),
                                       div(tags$b("Paso 3. Decida cómo descargar el gráfico.")),
                                       div(title= "525",
                                           awesomeRadio("var_plot_trend", label =NULL, inline = TRUE,
                                                        choices = c("Estimación" = "measure"))),
                                       div(title=".", # tooltip
                                           awesomeCheckbox("ci_trend", label = "coeficiente de variación", value = TRUE)),
                                       savechart_button('download_trendplot', 'Save chart',  class = "down", disabled=FALSE))),
                   mainPanel(width = 8, #Main panel
                             bsModal("mod_defs_trend", "Definitions", "defs_trend", htmlOutput('defs_text_trend')),
                             h4(textOutput("title_trend"), style="color: black; text-align: left"),
                             h4(textOutput("gap_year_notice"), style="color: red; text-align: left"),
                             h5(textOutput("subtitle_trend"), style="color: black; text-align: left"),
                             withSpinner(plotlyOutput("trend_plot"))
                   )
                 )  
                 
                 
                 
                 
                 
                 
               ))
               
               
             )
             
             
             
    )
    
    
    
    
    
    
    
  ))
  
}

server <- function(input, output, session) {
  output$graf_1 <- renderPlot({
    escoger <- base %>% 
      filter(est == input$estrato)
    ggplot(escoger, aes(x = prov, y=porc, fill=tasas) ) +
      geom_bar(width = 0.9, stat="identity", position = 'stack')+
      geom_hline(yintercept = 0.20,linetype = 8,size = 1)+
      ylim(c(0,1))+
      labs(x="Provincias") +
      labs(y= "Porcentajes")+
      scale_fill_discrete(name = "Tasas de conformidad") +
      scale_fill_manual( values = c("#f7b668","red","#5AA354","#5d8aa8"))+
      theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +
      facet_grid(~"TASAS DE CONFORMIDAD A NIVEL DE PROVINCIA")+
      coord_flip()+
      labs( title = "TASAS DE CONFORMIDAD POR PROVINCIA",
            subtitle = "ENEMDU Febrero 2024",
            caption = "Dirección de Infraestructura Estadística y Muestreo (DINEM)")
  })
  
  output$MAP1 <- renderLeaflet({
    
    data_c <- data_c %>%
      filter(Filtros==input$Filtros_1)              
    
    provincias@data = provincias@data %>%
      select(dpa_provin, dpa_despro) %>% 
      mutate(dpa_provin = as.numeric(dpa_provin)) %>% 
      full_join(data_c, by = "dpa_provin")
    
    provincias %>% 
      leaflet() %>%
      setView(-78, -1.8, 6) %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>%
      addPolygons(fillColor = ~pal(Cobertura),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.85,
                  popup = ~paste("Provincia:", dpa_despro, "<br/>",
                                 "Tasa de respuesta:", TRE, "<br/>",
                                 "Tasa de no respuesta:", TNR, "<br/>",
                                 "Tasa de no elegibles:", TNE, "<br/>",
                                 "Tasa de elegibilidad desconocida:", TED,
                                 "Total:", Total))  %>%
      addLegend(title = " Total de Vivendas ", pal = pal, values = ~Cobertura, opacity = 1.0)
  })
  
  output$mtcars_kable <- function() {
    
    es |> 
      mutate(
        Total_t = cell_spec(Total_t, color = ifelse(Total_t == 100, "white", "black"),
                            background = ifelse(Total_t == 100, "#7AC5CD", "white"),
                            bold = ifelse(Total_t == 100, T, F)),
        Total_m = cell_spec(Total_m, color = ifelse(Total_m > 80, "white", "black"),
                            background = ifelse(Total_m > 60, "#7AC5CD", "white"),
                            bold = ifelse(Total_m > 60, T, F)),
        Total_d = cell_spec(Total_d, color = ifelse(Total_d%in% c(0,es[13,13]) , "white", "black"),
                            background = ifelse(Total_d%in% c(0,es[13,13]), "#7AC5CD", "white"),
                            bold = ifelse(Total_d%in% c(0,es[13,13]), T, F))) %>%
      
      kable(escape = F, booktabs = T) %>%
      kable_styling("bordered", full_width = F) %>% 
      add_header_above(c("", "Distribución Teórica_t" = 4, "Viviendas levantadas_m" =4, "Diferencia Viviendas_d"=4))
  }
  
  output$distPlot <- function() {
    kbl(est_pob,caption = "Variables para la calibración “id_calib") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")
    
    
  }
  output$distPlot2 <- function() {
    kbl(text_tbl) %>%
      kable_paper(full_width = F) %>%
      column_spec(1, bold = T, border_right = T) %>%
      column_spec(2, width = "30em", background = "#EEEEEO") %>%
      kable_styling(bootstrap_options = "striped", full_width = F,position = "float_left")
  }  
  
  output$distPlot3 <- renderPlot({
    plot(base_fexp$fexp_teo, base_fexp$fexp_aju,
         xlab = "Factor de expansión teórico", 
         ylab = "Factor de expansión ajustado")
    abline(a=0, b=1, col="blue")
    title(main = "Gráfico de comparación entre el factor de expansión teórico y ajustado")
  })
  
  output$distPlot4 <- renderPlot({
    plot(base_fexp$fexp_aju, base_fexp$fexp_rec45,
         xlab = "Factor de expansión ajustado", 
         ylab = "Factor de expansión recortado")
    abline(a=0, b=1, col="blue")
    title(main = "Gráfico de comparación entre el factor de expansión ajustado y recortado 4.5 la mediana.")
    
  })
  
  
  output$distPlot5 <- renderPlot({
    g1 <- base_fexp %>%
      ggplot(aes(x = dominio,
                 y = fexp_aju)) +
      geom_boxplot(fill = "#56B1F7", color = "#132B43") +
      labs(x = "12 Dominios",
           y = "Factores de expansión ajustado por cobertura",
           title = "Diagrama de caja de los factores de expansión ajustados por cobertura ENEMDU Febrero 2024") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size=14))
    print(g1)
  })
  
  #BUTTON
  observeEvent(input$button1, {
    
    showModal(modalDialog(
      title = "Los 12 dominios son los siguientes: ",
      p(column(5,
               p("1. Quito"),
               p("2. Guayaquil"),
               p("3. Cuenca"),
               p("4. Machala"),
               p("5. Ambato"),
               p("6. Resto Sierra Urbano"),
               p("7. Resto Costa Urbano"),
               p("8. Amazonía Urbano"),
               p("9. Sierra Rural"),
               p("10. Costa Rural"),
               p("11. Amazonía Rural"),
               p("12. Región Insular"))),
      size = "l", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
  })
  
  output$distPlot6 <- renderPlot({
    g2 <- base_fexp %>%
      ggplot(aes(x = dominio,
                 y = fexp_rec45)) +
      geom_boxplot(fill = "#56B1F7", color = "#132B43") +
      labs(x = "12 Dominios",
           y = "Factores de expansión recortado",
           title = "Diagrama de caja de los factores de expansión recortado ENEMDU Febrero 2024") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size=14))
    print(g2)
  })
  
  #BUTTON
  observeEvent(input$button2, {
    
    showModal(modalDialog(
      title = "Los 12 dominios son los siguientes: ",
      p(column(5,
               p("1. Quito"),
               p("2. Guayaquil"),
               p("3. Cuenca"),
               p("4. Machala"),
               p("5. Ambato"),
               p("6. Resto Sierra Urbano"),
               p("7. Resto Costa Urbano"),
               p("8. Amazonía Urbano"),
               p("9. Sierra Rural"),
               p("10. Costa Rural"),
               p("11. Amazonía Rural"),
               p("12. Región Insular"))),
      size = "l", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
  })
  
  
  output$distPlot7 <- renderPlot({
    g3 <- base_fexp %>%
      mutate(Gedad = factor(gedad, c("1", "2"), c("< 15 añ0s", ">= 15 añ0s"))) %>% 
      ggplot(aes(x = fexp_aju,
                 y = fexp_rec45)) + 
      geom_point(aes(colour = Gedad)) +
      geom_abline(aes(intercept = 0,
                      slope = 1),
                  linetype = "dashed",
                  alpha=0.75) +
      #color = "#C5BEBA") +
      facet_wrap(~dominio,
                 scales = "free",
                 labeller = label_value) +
      labs(x = "Factor de expansión ajustado por cobertura",
           y = "Factor de expansión recortado",
           title = "Comparación de los factores de expansión ajustado vs recortado ENEMDU Febrero 2024") +
      scale_color_manual(values = c("#113743", "#C5001A")) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "#E4E3DB",
                                            colour = "#E4E3DB",
                                            size = 0.5, linetype = "solid"),
            strip.background = element_rect(fill="white"))
    print(g3)
  })
  #BUTTON
  observeEvent(input$button3, {
    
    showModal(modalDialog(
      title = "Los 12 dominios son los siguientes: ",
      p(column(5,
               p("1. Quito"),
               p("2. Guayaquil"),
               p("3. Cuenca"),
               p("4. Machala"),
               p("5. Ambato"),
               p("6. Resto Sierra Urbano"),
               p("7. Resto Costa Urbano"),
               p("8. Amazonía Urbano"),
               p("9. Sierra Rural"),
               p("10. Costa Rural"),
               p("11. Amazonía Rural"),
               p("12. Región Insular"))),
      size = "l", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
  })
  
  
  output$teorico <- function() {
    kbl(teorico,caption = "Estadísticas descriptivas teórico") %>% 
      kable_styling("bordered", full_width = F)
  }  
  output$ajustado <- function() {
    kbl(ajustado,caption = "Estadísticas descriptivas factor ajustado") %>% 
      kable_styling("bordered", full_width = F)
  }  
  output$recortado <- function() {
    kbl(recortado,caption = "Estadísticas descriptivas factor recortado") %>% 
      kable_styling("bordered", full_width = F)
  }  
  output$calibrado <- function() {
    kbl(calibrado,caption = "Estadísticas descriptivas factor calibrado") %>% 
      kable_styling("bordered", full_width = F)
  }  
  
  output$distPlot8 <- renderPlot({
    g4 <- base_fexp%>%
      mutate(Gedad = factor(gedad, c("1", "2"), c("< 15 añ0s", ">= 15 añ0s")))%>%
      ggplot(aes(x = fexp_rec45,
                 y = fexp_cal_upm)) + 
      geom_point(aes(colour = Gedad)) +
      geom_abline(aes(intercept = 0,
                      slope = 1),
                  linetype = "dashed",
                  alpha=0.75) +
      #color = "#C5BEBA") +
      facet_wrap(~dominio,
                 scales = "free",
                 labeller = label_value) +
      labs(x = "Factor de expansión recortado",
           y = "Factor de expansión calibrado",
           title = "Comparación  de los factores de expansión recortado vs calibrado ENEMDU Febrero 2024") +
      scale_color_manual(values = c("#113743", "#C5001A")) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "#E4E3DB",
                                            colour = "#E4E3DB",
                                            size = 0.5, linetype = "solid"),
            strip.background = element_rect(fill="white"))
    print(g4)
  })
  
  #BUTTON
  observeEvent(input$button4, {
    
    showModal(modalDialog(
      title = "Los 12 dominios son los siguientes: ",
      p(column(5,
               p("1. Quito"),
               p("2. Guayaquil"),
               p("3. Cuenca"),
               p("4. Machala"),
               p("5. Ambato"),
               p("6. Resto Sierra Urbano"),
               p("7. Resto Costa Urbano"),
               p("8. Amazonía Urbano"),
               p("9. Sierra Rural"),
               p("10. Costa Rural"),
               p("11. Amazonía Rural"),
               p("12. Región Insular"))),
      size = "l", easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
  })
  
  output$distPlot9 <- function() {
    kbl(vis,caption = "Comprobaciones: d y t.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")
    
    
  }
  output$distPlot10 <- function() {
    kbl(text_tbl2) %>%
      kable_paper(full_width = F) %>%
      column_spec(1, bold = T, border_right = T) %>%
      column_spec(2, width = "30em", background = "#EEEEEO") %>%
      kable_styling(bootstrap_options = "striped", full_width = F,position = "float_left")
  }  
  
  output$m1 <- function() {
    kbl(medidas1,caption = "Medida 1: er_upm_c.") %>% 
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  }  
  
  output$m2<- function() {
    kbl(medidas2,caption = "Medida 2: CV.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  }  
  
  output$m3<- function() {
    kbl(medidas3,caption = "Medida 3: M3_upm_prop_c, (minimos y maximos).") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  output$m4<- function() {
    kbl(medidas4,caption = "Medida 4: M4_upm_U3_prop_c.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  output$m5<- function() {
    kbl(medidas5,caption = "Medida 5: cv_g_upm_c.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  output$m6<- function() {
    kbl(medidas6,caption = "Medida 6: dist_g_upm_c.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  
  output$e1<- function() {
    kbl(es1,caption = "Estimación Empleo Adecuado") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  output$e2<- function() {
    kbl(es2,caption = "Estimación Desempleo") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  output$e3<- function() {
    kbl(es3,caption = "Estimación Subempleo.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  output$e4<- function() {
    kbl(es4,caption = "Estimación Pobreza por Ingresos.") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  } 
  
  

  
  
  observeEvent(input$indic_trend, {
    
    trend <- data_estimacion %>% subset(indicator == input$indic_trend) %>%
      droplevels()
    
    
    toggleState("hbname_trend",
                condition = ("Estimación Puntual" %in%  unique(trend$areatype)))
    if (!("Estimación Puntual" %in% unique(trend$areatype) ) ) {
      updateSelectizeInput(session, "hbname_trend",
                           label = "Estimación Puntual (not available)")
    } else {
      updateSelectizeInput(session, "hbname_trend",
                           label = "Estimación Puntual")
    }
    
    
    

  })
  
  
  trend_data <- reactive({
    
    trend <- data_estimacion %>%
      subset((areaname %in% input$hbname_trend & areatype == "Estimación Puntual") &
               indicator == input$indic_trend) %>%
      droplevels() %>%
      mutate(areaname_full = as.factor(areaname_full),
             # adjusting levels of areatype, so Scotland always plotted as black
             areatype = factor(areatype,
                               levels = c("Scotland", "Estimación Puntual"))) %>%
      arrange(year, areatype, areaname_full) #Needs to be sorted by year for Plotly
  })
  
  #####################.
  # Creating plot ----
  #####################.
  # titles
  output$title_trend <- renderText(paste0(input$indic_trend))
  output$subtitle_trend <- renderText(paste0(trend_type()))
  

  
  trend_type <- function () {
    # y axis title
    yaxis_title <- case_when(input$var_plot_trend == "measure" ~ paste0(unique(trend_data()$type_definition)))
  }
  
  
  #####################.
#Plot
  plot_trend_chart <- function() {
    #If no data available for that period then plot message saying data is missing
    # Also if numerator is all NA
    if (is.data.frame(trend_data()) && nrow(trend_data()) == 0 )
    {
      plot_nodata()
    } else { #If data is available then plot it

      # Creating palette of colors: colorblind proof
      # First obtaining length of each geography type, if more than 6, then 6,
      # this avoids issues. Extra selections will not be plotted
      trend_length <- ifelse(length(input$scotname_trend)+length(input$hbname_trend)+ length(input$caname_trend)+
                               length(input$adpname_trend)+length(input$partname_trend)+
                               length(input$locname_trend)+length(input$izname_trend) > 12, 12,
                             length(input$scotname_trend)+length(input$hbname_trend)+ length(input$caname_trend)+
                               length(input$adpname_trend)+length(input$partname_trend)+
                               length(input$locname_trend)+length(input$izname_trend))

      # First define the palette of colours used, then set a named vector, so each color
      # gets assigned to an area. I think is based on the order in the dataset, which
      # helps because Scotland is always first so always black.
      trend_palette <- c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                         "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")

      trend_scale <- c(setNames(trend_palette, unique(trend_data()$areaname_full)[1:trend_length]))
      trend_col <- trend_scale[1:trend_length]

      #Modifying standard layout
      yaxis_plots[["title"]] <- trend_type()

      xaxis_plots[["tickangle"]] <- ifelse(max(nchar(as.character(trend_data()$trend_axis)))>7, -45, 0)
      xaxis_plots[["dtick"]] <- ifelse(length(unique(trend_data()$trend_axis)) >=10, 2, 1)


      # Same approach for symbols
      symbols_palette <-  c('circle', 'diamond', 'circle', 'diamond', 'circle', 'diamond',
                            'square','triangle-up', 'square','triangle-up', 'square','triangle-up')
      symbols_scale <- c(setNames(symbols_palette, unique(trend_data()$areaname_full)[1:trend_length]))
      symbols_trend <- symbols_scale[1:trend_length]

      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data()$areaname, "<br>", trend_data()$trend_axis,
                                "<br>", paste0(unique(trend_data()$type_definition)),": ", trend_data()$measure))

      #Creating time trend plot
      trend_plot <- plot_ly(data=trend_data(), x=~trend_axis,  y = ~get(input$var_plot_trend),
                            color = ~areaname_full, colors = trend_col,
                            text=tooltip_trend, hoverinfo="text", height = 600 ) %>%
        add_trace(type = 'scatter', mode = 'lines+markers', marker = list(size = 8),
                  symbol = ~areaname_full, symbols = symbols_trend) %>%
        #Layout
        layout(annotations = list(), #It needs this because of a buggy behaviour of Plotly
               margin = list(b = 180, t=5), #to avoid labels getting cut out
               yaxis = yaxis_plots, xaxis = xaxis_plots, font = font_plots,
               showlegend = TRUE,
               legend = list(orientation = 'h', x = 0, y = 1.18)) %>%  #legend on top
        config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo button

      #Adding confidence intervals depending on user input
      if (input$ci_trend == TRUE) {
        trend_plot %>%
          add_ribbons(data = trend_data(), ymin = 0, ymax = 0, showlegend = F,
                      opacity = 0.2)

      } else if (input$ci_trend == FALSE) {
        trend_plot
      }
    }
  }
  # Creating plot for ui side
  output$trend_plot <- renderPlotly({ plot_trend_chart()  })
}

# Run the application
app <- shinyApp(ui = ui, server = server)
#runApp(app, launch.browser = F)
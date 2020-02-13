#########################################################################
###                                                                   ###
###     VISOR DE LA MATRIZ DE ORIGEN DESTINO DE LA ISLA DE TENERIFE   ###
###                                                                   ###
###     Datos procedentes de los resultados del trabajo de:           ###
###       Análisis de la movilidad y modelización del                 ###
###       sistema de transporte de la isla de Tenerife                ###
###                                                                   ###
###     Desarrollo de Miguel Pintor  mpsepulveda@ciccp.es             ###
###                                                                   ###
#########################################################################

shinyServer(function(input, output, session) {
  
  observe_helpers(withMathJax = TRUE)
  totalIsla <- 0
  mapaActual <- "defZonas"   # valores posibles 1.- "defZonas" - definicion de zonificacion con nombre (o numero) 
                                              # 2.- "viajesGAseleccion" - viajes atraidos o generados desde zona seleccionada 
                                              # 3.- "viajesGAzonas" - viajes atraidos o generados por cada una de las zonas
                                              # 4.- "viajesGAsaldo" - saldo = generados - atraidos por cada una de las zonas

  # carga datos geograficos y de viajes
  zonas <- readRDS("data/zonas.Rda")
  dfZonas <- readRDS("data/dfZonas.Rda")
  municipios <- readRDS("data/municipios.Rda")
  centros <- readRDS("data/centros.Rda")
  macroZonas <- readRDS("data/macroZonas.Rda")
  viajes <- readRDS("data/viajes.Rda")
  dfViajesHora <- readRDS("data/dfViajesHora.Rda")
  
  
  # valores posibles para los motivos de viaje, residencia del viajero y tpo de dia
  motivos <- c("T", "H", "O", "NF", "-")
  residencias <- c("Alemania", "Belgica", "Francia", "Irlanda", "Italia", "Paises Bajos", "Paises Nordicos", 
                   "Portugal", "Reino Unido", "Resto de Espana", "Resto de Europa", "Resto del mundo", "Rusia", "Tenerife")
  
  distancias <- c("0-1000", ">1000")
  
  choiceMotivo <- list("Trabajo" = 1, "Casa" = 2, "Otra actividad frecuente" = 3, "Actividad no frecuente" = 4, "Extranjeros" = 5)
  
  choiceResidencia <- list("Alemania" = 1, "Belgica" = 2, "Francia" = 3, "Irlanda" = 4, "Italia" = 5,
                           "Paises Bajos" = 6, "Paises Nordicos" = 7, "Portugal" = 8, "Reino Unido" = 9, "Resto de Espana" = 10,
                           "Resto de Europa" = 11, "Resto del mundo" = 12, "Rusia" = 13, "Tenerife" = 14)
  
  tiposZonificacion <- c("Macrozonas de Transporte", "Municipios", "Zonas de Transporte")
  tiposZonificacionSeleccionada <- c("la Macrozona de Transporte seleccionada.", "el Municipio seleccionado.", "la Zona de Transporte seleccionada.")
  tituloGA <- c("generados", "atraídos")
  
  od <- NULL
  

  # genera plano inicial ############################################################################################################################
  

  output$mymap <- renderLeaflet({
    
    # limites del mapa
    bBox <- bbox(municipios)
    
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>%
      fitBounds(lat1 = bBox[2], lng1 = bBox[1], lat2 = bBox[4], lng2 = bBox[3]) %>%
      addPolygons(data = municipios, color = "#000000", weight = 1, smoothFactor = 0.5, opacity = 1.0, 
                  fillOpacity = 0.2,  fillColor = "blue", layerId = ~id, group = "zonas",
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))  %>%
      addLabelOnlyMarkers(data = centros, lng = ~x, lat = ~y, label = ~id,  group = "zonas",
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(
                            "color" = "white", "font-family" = "serif", "font-style" = "bold",
                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)", "font-size" = "14px", "border-color" = "rgba(0,0,0,0.5)"
                          ))) %>%
      addEasyButtonBar(easyButton(title = "Definición de la Zonificación", position = "topleft", id = "refreshZonasIniciales",
                                  icon = "glyphicon-fast-backward", onClick = JS("function(btn, map) {Shiny.onInputChange('mensaje',  {msg:'defZonas', '.nonce': Math.random()})}")),
                       easyButton(title = "Viajes Generados o Atraidos", position = "topleft", id = "viajesGA",
                                  icon = "glyphicon-sort", onClick = JS("function(btn, map) {Shiny.onInputChange('mensaje',  {msg:'viajesGAzonas', '.nonce': Math.random()})}")),
                       easyButton(title = "Saldo de viajes (Generados-Atraidos)", position = "topleft", id = "viajesGA",
                                  icon = "fa-balance-scale", onClick = JS("function(btn, map) {Shiny.onInputChange('mensaje',  {msg:'viajesGAsaldo', '.nonce': Math.random()})}")),
                       easyButton(title = "Recalcular", position = "topleft", id = "refreshZonas",
                                  icon = "fa-sync", onClick = JS("function(btn, map) {Shiny.onInputChange('mensaje',  {msg:'refrescar', '.nonce': Math.random()})}")))
  })
  
  # calcula la matriz OD en base a la selección efectuada (O<>D, motivo, residencia) ################################################################
  matrizOD <- function(){
    # lee los motivos ty residencias seleccionados
    motOrSelec <- as.integer(input$motivoOrigen)
    motDestSelec <- as.integer(input$motivoDestino)
    longViaje <- as.integer(input$distancia)
    resSelec <- as.integer(input$residencia)
    hourSel <- input$horas
    type <- input$analisis
    #numero de zonas
    maxZone <- max(dfZonas$idZona)
    # de la tabla desagrgada de viajes, selecciona segun motivo, residencia
    trips <- subset(viajes, viajes$ACT_ORIGEN %in% motivos[motOrSelec])
    trips <- subset(trips, trips$ACT_DESTINO %in% motivos[motDestSelec])
    trips <- subset(trips, trips$DISTANCIA %in% distancias[longViaje])
    trips <- subset(trips, trips$RESIDENCIA %in% residencias[resSelec])
    #tabla para graficas horarias
    dfViajesHora <<- trips[, c("PERIODO", "ACT_ORIGEN", "ACT_DESTINO", "VIAJES", "RESIDENCIA")]
    dfViajesHora <<- aggregate(VIAJES~., dfViajesHora, FUN = sum)
    # selecciona por dias y horas
    #trips <- subset(trips, trips$DIA == dias[as.integer(dia)])
    trips <- subset(trips, trips$PERIODO >= hourSel[1] & trips$PERIODO <= hourSel[2])
    
    totalIsla <<- round(sum(trips$VIAJES))
    
    if(nrow(trips) > 0){
      # agrega los datos sumando los viajes para cada par de origen-destino
      trips <- aggregate(VIAJES ~ ORIGEN + DESTINO, data = trips, FUN = sum)
      #convierte en matriz utilizando los valores de ORIGEN y DESTINO como filas y columnas
      odMatrix <- dcast(trips, ORIGEN~DESTINO, value.var = "VIAJES")
      #pasa los origenes como nombre de filas y borra la columna ORIGEN
      row.names(odMatrix) <- odMatrix$ORIGEN
      odMatrix$ORIGEN <- NULL
      # convierte los NAs en ceros
      odMatrix[is.na(odMatrix)] <- 0.0
      #inserta columnas inexistentes
      colInex <- !(dfZonas$idZona %in% colnames(odMatrix))
      colInex <- dfZonas[colInex, "idZona"]
      odMatrix[, as.character(colInex)] <- 0.0
      odMatrix <- odMatrix[, as.character(dfZonas$idZona)]
      #inserta filas inexistentes
      rowInex <- !(dfZonas$idZona %in% row.names(odMatrix))
      rowInex <- dfZonas[rowInex, "idZona"]
      odMatrix[as.character(rowInex), ] <- 0.0
      odMatrix <- odMatrix[as.character(dfZonas$idZona), ]
    } else {
      odMatrix <- matrix(rep(0, nrow(dfZonas)^2), nrow(dfZonas), nrow(dfZonas))
      odMatrix <- as.data.frame(odMatrix)
      colnames(odMatrix) <- as.character(dfZonas$idZona)
      rownames(odMatrix) <- as.character(dfZonas$idZona)
    }
    if(type==1){
      odMZ <- NULL
      # agrupa la matriz od por macrozonas
      # agrega las columnas
      for(i in seq.int(nrow(macroZonas))){
        columnasMZ <- as.character(subset(zonas, zonas$idMacrozona == i)$id)
        columnasMZ <- columnasMZ[columnasMZ %in% colnames(odMatrix)]
        colSum <- rowSums(odMatrix[,  columnasMZ])
        odMZ <- cbind(odMZ, colSum)
        colnames(odMZ)[ncol(odMZ)] <- as.character(i)
      }
      auxMZ <- odMZ
      odMZ <- NULL
      # agrega las filas
      for(i in seq.int(nrow(macroZonas))){
        filasMZ <- as.character(subset(zonas, zonas$idMacrozona == i)$id)
        filasMZ <- filasMZ[filasMZ %in% rownames(auxMZ)]
        rowSum <- colSums(auxMZ[filasMZ,])
        odMZ <- rbind(odMZ, rowSum)
        row.names(odMZ)[nrow(odMZ)] <- as.character(i)
      }
      return(odMZ)
    } else if(type==2){
      odM <- NULL
      # agrupa la matriz od por municipios
      # agrega las columnas
      for(i in seq.int(nrow(municipios))){
        columnasM <- as.character(subset(dfZonas, dfZonas$idMunicipio == municipios$id[i])$idZona)
        columnasM <- columnasM[columnasM %in% colnames(odMatrix)]
        colSum <- NULL
        if(length(columnasM)  > 1){
          colSum <- rowSums(odMatrix[,  columnasM])
        } else if (length(columnasM)  == 1){
          colSum <- odMatrix[,  columnasM]
        }
        odM <- cbind(odM, colSum)
        colnames(odM)[ncol(odM)] <- as.character(municipios$id[i])
        
      }
      auxMZ <- odM
      odM <- NULL
      # agrega las filas
      for(i in seq.int(nrow(municipios))){
        filasM <- as.character(subset(dfZonas, dfZonas$idMunicipio == municipios$id[i])$idZona)
        filasM <- filasM[filasM %in% rownames(auxMZ)]
        rowSum <- NULL
        if(length(filasM)  > 1){
          rowSum <- colSums(auxMZ[filasM,])
        } else if (length(filasM)  == 1){
          rowSum <- auxMZ[filasM,]
        }
        odM <- rbind(odM, rowSum)
        row.names(odM)[nrow(odM)] <- as.character(municipios$id[i])
      }
      return(odM)
    } else if (type == 3){
      return(odMatrix)
    }
  }
  
  ##########################################################################################################################################
  drawMap <- function(tipoMapa) { # actualiza el mapa según lo seleccionado ################################################################
  ##########################################################################################################################################
    
    if (tipoMapa == "refrescar") tipoMapa <- mapaActual
    mapaActual <<- tipoMapa
    #calcula la matriz OD
    od <- matrizOD()
    # modifica el valor de la variable de selección 
    if(tipoMapa == "viajesGAseleccion"){
      click <- input$mymap_shape_click
      if(is.null(click)) return()
      #selecciona los viajes a tratar según se haya seleccionado origen (fila de la matriz OD) o destinos (columna de la matriz OD)
      odSele <- as.integer(input$od)
      if(odSele == 1){
        trips <- od[as.character(click$id),]
      } else {
        trips <- od[,as.character(click$id)]
        names(trips) <- row.names(od)
      }
    } else if (tipoMapa == "viajesGAzonas" & input$od == 1){ # generados
      trips <- as.integer(rowSums(od))
      names(trips) <- colnames(od)
    }  else if (tipoMapa == "viajesGAzonas" & input$od == 2){ # atraídos
      trips <- as.integer(colSums(od))
      names(trips) <- rownames(od)
    } else if (tipoMapa == "viajesGAsaldo"){ # generados menos atraidos
      tripsGenerados <- as.integer(rowSums(od))
      names(tripsGenerados) <- colnames(od)
      tripsGenerados <- tripsGenerados[order(names(tripsGenerados))]
      
      tripsAtraidos <- as.integer(colSums(od))
      names(tripsAtraidos) <- rownames(od)
      tripsAtraidos <- tripsAtraidos[order(names(tripsAtraidos))]
      
      trips <- tripsGenerados - tripsAtraidos
    }
    
    # zonificación seleccionada
    tipoZona <- input$analisis
    if (tipoZona == 1){ ###############################################################################################################analisis macrozonas
      centros <<- data.frame(gCentroid(macroZonas, byid = TRUE))
      centros$id <<- as.character(macroZonas$comarca)
      poliSelec <- macroZonas
      if (tipoMapa != "defZonas"){
        #pasa el numero de viajes hacia/desde a cada zona a la variable auxiliar de la capa geografica
        macroZonas$aux <<- 0
        for(i in seq(1, length(names(trips)))){
          if(!is.na(trips[i])){
            macroZonas$aux[match(names(trips)[i], macroZonas$id)] <<- trips[i]
          }
        }
        macroZonas$aux <<- unlist(macroZonas$aux)
        # genera la paleta de colores
        mypal <- colorNumeric(palette = "RdYlGn", domain = c(round(min(macroZonas$aux)), round(max(macroZonas$aux))+1), n = 20, reverse = TRUE)
        if(tipoMapa == "viajesGAseleccion"){
          # indice de la zona seleccionada
          idHL <- match(click$id, macroZonas$id)
          poliSelec <- macroZonas[idHL,]
          poliNoSelec <- macroZonas[-idHL,]
          # muestra datos en texto
          textoAuxiliar <- paste("<b>Macrozona:</b> ",    macroZonas$comarca[click$id], 
                                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Viajes:</b> ", format(round(sum(macroZonas$aux)), big.mark = ","),
                                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Total de Viajes en la Isla:</b> ", format(round(totalIsla), big.mark = ","), sep = "")
        } else {
          poliSelec <- macroZonas
          textoAuxiliar <- paste("<b>Total de Viajes en la Isla:</b> ", format(totalIsla, big.mark = ","), sep = "")
        }
        # pasa a la tabla de centroides los valores de los viajes para etiquetar las zonas
        centros$id <<- format(round(macroZonas$aux), big.mark = ",")
      }
    } else if (tipoZona == 2){ ###############################################################################################################analisis municipal
      # genera un dataset con los centroides de cada zona para etiquetar las zonas en el plano
      centros <<- data.frame(gCentroid(municipios, byid = TRUE))
      centros$id <<- as.character(municipios$municipio)
      # rectifica La Laguna
      centros[1, "y"] <<- centros[3, "y"] # la y de Tacoronte
      # rectifica La Orotava
      centros[29, "x"] <<- centros[16, "x"] # la x de arico
      centros[29, "y"] <<- centros[8, "y"] # la y del Realejo
      poliSelec <- municipios
      if (tipoMapa != "defZonas"){
        #pasa el numero de viajes hacia/desde a cada zona a la variable auxiliar de la capa geografica
        municipios$aux <<- 0
        for(i in seq(1, length(names(trips)))){
          if(!is.na(trips[i])){
            municipios$aux[match(names(trips)[i], municipios$id)] <<- trips[i]
          }
        }
        municipios$aux <<- unlist(municipios$aux)
        # genera la paleta de colores
        mypal <- colorNumeric(palette = "RdYlGn", domain = c(round(min(municipios$aux)), round(max(municipios$aux))+1), n = 20, reverse = TRUE)
        
        if(tipoMapa == "viajesGAseleccion"){
          # indice de la zona seleccionada
          idHL <- match(click$id, municipios$id)
          poliSelec <- municipios[idHL,]
          poliNoSelec <- municipios[-idHL,]
          textoAuxiliar <- paste("<b>Municipio:</b> ",    municipios$municipio[idHL], 
                                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Viajes:</b> ", format(round(sum(municipios$aux)), big.mark = ","),
                                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Total de Viajes en la Isla:</b> ", format(totalIsla, big.mark = ","), sep = "")
        } else {
          poliSelec <- municipios
          textoAuxiliar <- paste("<b>Total de Viajes en la Isla:</b> ", format(totalIsla, big.mark = ","), sep = "")
        }
        
        # pasa a la tabla de centroides los valores de los viajes para etiquetar las zonas
        centros$id <<- format(round(municipios$aux), big.mark = ",")
      }
    } else if (tipoZona == 3) { ###############################################################################################################analisis zonas de transporte
      # genera un dataset con los centroides de cada zona para etiquetar las zonas en el plano
      centros <<- data.frame(gCentroid(zonas, byid = TRUE))
      centros$id <<- as.character(zonas$idZona)
      #centros$id <<- as.character(zonas$idMunicipio)
      poliSelec <- zonas
      if (tipoMapa != "defZonas"){
        #pasa el numero de viajes hacia/desde a cada zona a la variable auxiliar de la capa geografica
        zonas$aux <<- 0
        for(i in seq(1, length(names(trips)))){
          if(!is.na(trips[i])){
            zonas$aux[match(names(trips)[i], zonas$id)] <<- trips[i]
          }
        }
        #print("kk")
        zonas$aux <<- unlist(zonas$aux)
        # genera la paleta de colores
        mypal <- colorNumeric(palette = "RdYlGn", domain = c(round(min(zonas$aux)), round(max(zonas$aux))+1), n = 20, reverse = TRUE)
        if(tipoMapa == "viajesGAseleccion"){
          # indice de la zona seleccionada
          idHL <- match(click$id, zonas$id)
          poliSelec <- zonas[idHL,]
          poliNoSelec <- zonas[-idHL,]
          # muestra datos en texto
          textoAuxiliar <- paste("<b>Zona:</b> ", as.character(click$id), 
                                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Viajes:</b> ", format(round(sum(zonas$aux)), big.mark = ","),
                                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Total de Viajes en la Isla:</b> ", format(totalIsla, big.mark = ","), sep = "")
        } else {
          poliSelec <- zonas
          textoAuxiliar <- paste("<b>Total de Viajes en la Isla:</b> ", format(totalIsla, big.mark = ","), sep = "")
        }
        # pasa a la tabla de centroides los valores de los viajes para etiquetar las zonas
        centros$id <<- format(round(zonas$aux), big.mark = ",")
      }
    }
    if(tipoMapa == "defZonas"){ # modifica plano para presentar la definición de las zonas
      
      bBox <- bbox(municipios)
      
      output$textTipoGrafico <- renderText(HTML("<h4><b>Mapa de definición de zonificación por&nbsp;", tiposZonificacion[as.integer(input$analisis)], "</h4>Viajes en el Total de la Isla:</b> ", format(totalIsla, big.mark = ",")))
      
      proxy <- leafletProxy("mymap")
      proxy %>% 
        clearGroup("zonas") %>% clearControls() %>%
        fitBounds(lat1 = bBox[2], lng1 = bBox[1], lat2 = bBox[4], lng2 = bBox[3]) %>%
        addPolygons(data = poliSelec, 
                    color = "#000000", 
                    weight = 1, 
                    smoothFactor = 0.5, 
                    opacity = 1.0, 
                    fillOpacity = 0.2, 
                    fillColor = "blue", 
                    layerId = ~id, group = "zonas",
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))  %>%
        addLabelOnlyMarkers(data = centros,
                            lng = ~x, lat = ~y, 
                            label = ~id,
                            group = "zonas",
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(
                              "color" = "white",
                              "font-family" = "Calibri",
                              "font-style" = "bold",
                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                              "font-size" = "14px",
                              "border-color" = "rgba(0,0,0,0.5)"
                            ))) 
    } else if (tipoMapa == "viajesGAseleccion") {
      if(as.integer(input$od) == 1){
        salida <- paste("<h4><b>Mapa de viajes con Origen en ", tiposZonificacionSeleccionada[as.integer(input$analisis)], "</b></h4>", sep = "")
      }
      else{
        salida <- paste("<h4><b>Mapa de viajes con Destino en ", tiposZonificacionSeleccionada[as.integer(input$analisis)], "</b></h4>", sep = "")
      }
      
      output$textTipoGrafico <- renderUI(HTML(salida, textoAuxiliar))
      
      
      proxy <- leafletProxy("mymap")
      proxy %>% 
        clearGroup("zonas") %>% clearControls() %>%
        addPolygons(data = poliNoSelec, color = "#000000", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.35,
                    fillColor = ~mypal(poliNoSelec$aux),
                    layerId = ~id, group = "zonas",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>%
        
        addPolygons(data = poliSelec, color = "red", weight = 2, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.35,
                    fillColor = ~mypal(poliSelec$aux),
                    layerId = ~id, group = "zonas",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>%
        addLabelOnlyMarkers(data = centros, 
                            lng = ~x, lat = ~y, 
                            label = ~id, group = "zonas",
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(
                              "color" = "white",
                              "font-family" = "Calibri",
                              "font-style" = "bold",
                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                              "font-size" = "14px",
                              "border-color" = "rgba(0,0,0,0.5)"
                            ))) %>%
        addLegend(pal = mypal, values = c(poliSelec$aux, poliNoSelec$aux) , opacity = 0.7, title = NULL, position = "bottomright", group = "zonas")
      
    } else {
      if(tipoMapa == "viajesGAsaldo"){
        salida <- paste("<h4><b>Mapa de saldo de viajes por ", tiposZonificacion[as.integer(input$analisis)], "</b></h4>", sep = "")
      } else if(as.integer(input$od) == 1){
        salida <- paste("<h4><b>Mapa de viajes Generados por ", tiposZonificacion[as.integer(input$analisis)], "</b></h4>", sep = "")
      }
      else  if(as.integer(input$od) == 2){
        salida <- paste("<h4><b>Mapa de viajes Atraídos por ", tiposZonificacion[as.integer(input$analisis)], "</b></h4>", sep = "")
      }
      
      output$textTipoGrafico <- renderUI(HTML(salida, textoAuxiliar))
      
      
      proxy <- leafletProxy("mymap")
      proxy %>% 
        clearGroup("zonas") %>%  clearControls() %>%
        addPolygons(data = poliSelec, color = "#000000", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.35,
                    fillColor = ~mypal(poliSelec$aux),
                    layerId = ~id, group = "zonas",
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>%
        addLabelOnlyMarkers(data = centros, 
                            lng = ~x, lat = ~y, 
                            label = ~id, group = "zonas",
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(
                              "color" = "white",
                              "font-family" = "Calibri",
                              "font-style" = "bold",
                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                              "font-size" = "14px",
                              "border-color" = "rgba(0,0,0,0.5)"
                            ))) %>%
        addLegend(pal = mypal, values = poliSelec$aux , opacity = 0.7, title = NULL, position = "bottomright", group = "zonas") 
    }
  }
  
  ##############################################################################################################################
  actualizaCuerdas <- function(){  # actualiza el gráfico de cuerdas ###########################################################
  ##############################################################################################################################
    
    type <- input$analisis
    
    if (type == 3) return()
    
    od <- matrizOD()
    
    if (type==1){
      # genera vectores de nombres de origenes y destionos en macrozonas
      origen <- macroZonas$comarca[match(as.integer(colnames(od)),  macroZonas$id)]
      destino <- macroZonas$comarca[match(as.integer(row.names(od)),  macroZonas$id)]
      # presnta gráfico de cuerdas
      output$diagCuerdas <- renderChorddiag({
        dimnames(od) = list(destino = destino, origen = origen)
        chorddiag(od)
      })
    } else if (type==2){
      # genera vectores de nombres de origenes y destionos municipales
      origen <- municipios$municipio[match(as.integer(colnames(od)),  municipios$id)]
      destino <- municipios$municipio[match(as.integer(row.names(od)),  municipios$id)]
      # presenta gráfico de cuerdas
      output$diagCuerdas <- renderChorddiag({
        dimnames(od) = list(destino = destino, origen = origen)
        chorddiag(od)
      })
      
    }
  }
  
  #################################################################################################################################
  presentaGrafico <- function(){ # presenta el gráfico de lineas ##################################################################
  #################################################################################################################################
    
    matrizOD()
    
    if(nrow(dfViajesHora) == 0) return()
    
    tipoGrafico <- input$tipoGrafico
    
    # si el tipo de gráfico no es por días selecciona el día de interés

    if(tipoGrafico == 1){ #grafico por actividad en origen
      dfViajesHora <- aggregate(VIAJES~PERIODO+ACT_ORIGEN, dfViajesHora, sum)
      levels(dfViajesHora$ACT_ORIGEN) <- names(choiceMotivo)[match(levels(dfViajesHora$ACT_ORIGEN), motivos)]
      output$grafica <- renderPlot(ggplot(data=dfViajesHora, aes(x=PERIODO, y=VIAJES, group=ACT_ORIGEN, colour = ACT_ORIGEN)) + 
                                     geom_point(size=3) +
                                     geom_line(size=1) + 
                                     labs(y="Viajes", colour="Actividad en Origen") +
                                     scale_x_continuous(name = "Hora", breaks = seq.int(24), limits = c(0,23)) +
                                     theme(legend.position = "left"))
    } else if(tipoGrafico == 2){ # grafico por actividad en destino
      dfViajesHora <- aggregate(VIAJES~PERIODO+ACT_DESTINO, dfViajesHora, sum)
      levels(dfViajesHora$ACT_DESTINO) <- names(choiceMotivo)[match(levels(dfViajesHora$ACT_DESTINO), motivos)]
      output$grafica <- renderPlot(ggplot(data=dfViajesHora, aes(x=PERIODO, y=VIAJES, group=ACT_DESTINO, colour = ACT_DESTINO)) + 
                                     geom_point(size=3) +
                                     geom_line(size=1) + 
                                     labs(y="Viajes", colour="Actividad en Destino") +
                                     scale_x_continuous(name = "Hora", breaks = seq.int(24), limits = c(0,23)) +
                                     theme(legend.position = "left"))
    } else if(tipoGrafico == 3){ # grafico por residencia
      dfViajesHora <- aggregate(VIAJES~PERIODO+RESIDENCIA, dfViajesHora, sum)
      levels(dfViajesHora$RESIDENCIA) <- names(choiceResidencia)[match(levels(dfViajesHora$RESIDENCIA), residencias)]
      output$grafica <- renderPlot(ggplot(data=dfViajesHora, aes(x=PERIODO, y=VIAJES, group=RESIDENCIA, colour = RESIDENCIA)) + 
                                     geom_point(size=3) +
                                     geom_line(size=1) + 
                                     labs(y="Viajes", colour="Residencia") +
                                     scale_x_continuous(name = "Hora", breaks = seq.int(24), limits = c(0,23)) +
                                     theme(legend.position = "left"))
    }
  }
  
  #################################################################################################################################
  ##################################################  D  E  S  C  A  R  G  A  S  ##################################################
  #################################################################################################################################	
  
  output$descargaMatriz <- downloadHandler(
    filename = function() {
      paste("matriz-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      datos <- matrizOD()
      write.csv(datos, file, row.names = TRUE, quote = FALSE)
    }
  )
  
  output$descargaViajes <- downloadHandler(
    filename = function() {
      paste("viajes-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      motOrSelec <- as.integer(input$motivoOrigen)
      motDestSelec <- as.integer(input$motivoDestino)
      longViaje <- as.integer(input$distancia)
      resSelec <- as.integer(input$residencia)
      hourSel <- input$horas
      
      
      trips <- subset(viajes, viajes$ACT_ORIGEN %in% motivos[motOrSelec])
      trips <- subset(trips, trips$ACT_DESTINO %in% motivos[motDestSelec])
      trips <- subset(trips, trips$DISTANCIA %in% distancias[longViaje])
      trips <- subset(trips, trips$RESIDENCIA %in% residencias[resSelec])
      trips <- subset(trips, trips$PERIODO >= hourSel[1] & trips$PERIODO <= hourSel[2])
      
      write.csv(trips, file, row.names = FALSE, quote = FALSE)
    }
  )
  
  output$descargaZonas <- downloadHandler(
    filename = function() {
      "definicionZonas.zip"
    },
    content = function(file) {
      file.copy("data/definicionZonas.zip", file)
    }
  )
  
  #################################################################################################################################
  ################################################   E  V  E  N  T  O  S  #########################################################
  #################################################################################################################################	
  
  # cambio de actividad en origen	
  observeEvent(input$motivoOrigen, {
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
    else if (input$mainTabs == "grafica") presentaGrafico()
  }, ignoreInit = TRUE, once = FALSE)
  
  # cambio de actividad en destino	
  observeEvent(input$motivoDestino, {
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
    else if(input$mainTabs == "grafica") presentaGrafico()
  }, ignoreInit = TRUE, once = FALSE)
  
  # cambio en la residencia habitual de los viajeros	
  observeEvent(input$residencia, {
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
    else if(input$mainTabs == "grafica") presentaGrafico()
  }, ignoreInit = TRUE, once = FALSE)
  
  # cambio en los horarios de viaje
  observeEvent(input$horas, {
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
  }, ignoreInit = TRUE, once = FALSE)
  
  # cambio en los dias (laborable, vispera, festivo)
  observeEvent(input$distancia, {
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
    else if(input$mainTabs == "grafica") presentaGrafico()
  }, ignoreInit = TRUE, once = FALSE)
  
  # cambio en el tipo de gráfico a presentar (en la pestaña grafico de lineas)
  observeEvent(input$tipoGrafico, presentaGrafico())
  
  # cambio en la zona seleccionada
  observeEvent(input$mymap_shape_click, drawMap("viajesGAseleccion"))
  
  # recibe cambios en los mensajes emitidos desde javascript
  observeEvent(input$mensaje, {
    # requiere la existencia de mensaje y actualiza el plano
    req(input$mensaje)
    drawMap(input$mensaje$msg)
    
  })
  
  # cambio en la zonificacion
  observeEvent(input$analisis, ignoreInit = TRUE, {
    drawMap("defZonas")
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
  })
  
  
  observeEvent(input$mainTabs, {
    
    if(input$mainTabs == "diagramaCuerdas") actualizaCuerdas()
    if(input$mainTabs == "grafica") presentaGrafico()
    
  })
  
  
})


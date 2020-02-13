
# Tenerife Island Trip data viewer (data from october 2018)
The application is a Tenerife island laborable day trip data viewer. The data was obtained from mobile phones in october 2018. It allows to visualize the trips generated and attracted by each zone and the trips between zones. Trips can be filtered by distance, activity at origin and destination, place of residence of the traveler and time of the day. Data download is allowed. Analisys is made for three different zone definition. It includes line charts and chord diagrams.  

Data visualization is calculated for three different zoning definitions in "Zonificación" selection box:
* Macrozonas de Transporte.- eleven zones
* Municipios.- 31 municipalities in Tenerife Island
* Zonas de transporte.- 150 transport zones.

Map visualization generates different analyzes:
* If you click on an zone, the number of trips with origin or destination in the selected zone is calculated and displayed in each zone including the zone selected for in-zone trips. Origin or destiny data is switched in the "Generados" or "Atraidos" selection in the "Generación - Atracción de Viajes" selection box.
* If the "Viajes Generados o Atraidos" button is pressed, the total number of trips generated or attracted by each zone is displayed (again depending on the selection of the "Generación - Atracción de Viajes" selection box).
* If the  "Travel Balance (Generated-Attracted)" button is pressed, the difference between generated and attracted trips for each zone is calculated.
* Pressing "Definicion de la zonificacion" button presents the zones with no data.

Filtering includes:
* Distance of trip, with more or less than 1000 meters options for walking trips filtering.
* Origin and Destination activity whith "work" (Trabajo), "home" (casa), "other frequent activities" (otra actividad frecuente), "non frequent activity" (actividad no frecuente) and "Tourists" (Extranjeros) choices.
* Place of residence of travelers with up to 14 different choices incluiding travelers living in Tenerife, rest of Spain and many other countries.
* Hour of trip start selected in "Horas" slider range.

Data can be downloaded:
* Filtered matrix data from "descargar Matriz"
* Filtered trips from "descargar Viajes"
* Zones definition shapefiles from "descargar Zonas"

After filtering, "Recalcular" button should be pressed to recalculate data in map.

In "Gráfica por Horas" tab three line graps can be generated:
* Hourly number of trips filtered by origin activity (Por Actividad en Origen)
* Hourly number of trips filtered by destination activiy (Por Actividad en Destino)
* Hourly number of trips filtered by place of residence (Por Residencia)

In "Diagrama de Cuerdas" tab, chord diagrams are presented. This only works for "Municipios(31)" and "Macrozonas de Transporte(11)" zonifications (not working for "Zonas de transporte(150)") selection in "Zonificación" selection box. 

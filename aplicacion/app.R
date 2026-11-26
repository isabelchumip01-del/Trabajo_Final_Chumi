# ============================================================
# PROYECTO FINAL: MUJERES Y GOBERNANZA DEL AGUA
# Junta de Riego y Drenaje Machángara - Ecuador
#
# Aplicación Shiny
# Autora: Isabel Chumi
#
# Fuente de datos: repositorio público de GitHub
# ============================================================

# ============================================================
# 1. CARGA DE PAQUETES
# ============================================================

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(sf)
library(DT)
library(plotly)
library(leaflet)

# ============================================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# ============================================================

# ------------------------------------------------------------
# 2.1. Direcciones de los archivos en GitHub
# ------------------------------------------------------------

url_datos <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/Datos_Limpios_Machangara.csv"

url_cuenca <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/cuenca_machangara.gpkg"

url_parroquias <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/parroquias_cuenca.gpkg"

# ------------------------------------------------------------
# 2.2. Lectura de la matriz de encuestas
# ------------------------------------------------------------

Datos <- readr::read_csv(
  url_datos,
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2.3. Descarga y lectura de las capas geográficas
# ------------------------------------------------------------

# Se descargan los GeoPackage en archivos temporales.
# Esto permite leerlos con sf sin utilizar rutas locales.

archivo_cuenca <- tempfile(fileext = ".gpkg")
archivo_parroquias <- tempfile(fileext = ".gpkg")

utils::download.file(
  url_cuenca,
  destfile = archivo_cuenca,
  mode = "wb",
  method = "libcurl",
  quiet = TRUE
)

utils::download.file(
  url_parroquias,
  destfile = archivo_parroquias,
  mode = "wb",
  method = "libcurl",
  quiet = TRUE
)

# Lectura de las capas

Cuenca <- sf::st_read(
  archivo_cuenca,
  quiet = TRUE
)

Parroquias <- sf::st_read(
  archivo_parroquias,
  quiet = TRUE
)

# ------------------------------------------------------------
# 2.4. Conversión de las encuestas en objetos espaciales
# ------------------------------------------------------------

# Se crean puntos a partir de las coordenadas de cada encuesta.
# El sistema de referencia original es WGS 84 / UTM zona 17S
# (EPSG:32717).

Encuestas_sf <- Datos |>
  sf::st_as_sf(
    coords = c("Coordenadas X", "Coordenadas Y"),
    crs = 32717,
    remove = FALSE
  )

# ------------------------------------------------------------
# 2.5. Identificación de las tomas de agua
# ------------------------------------------------------------

# Se conservan las ubicaciones únicas de las tomas.

Tomas_sf <- Datos |>
  dplyr::distinct(
    Toma,
    `Coordenadas X`,
    `Coordenadas Y`
  ) |>
  sf::st_as_sf(
    coords = c("Coordenadas X", "Coordenadas Y"),
    crs = 32717,
    remove = FALSE
  )

# ------------------------------------------------------------
# 2.6. Transformación al sistema de referencia WGS 84
# ------------------------------------------------------------
# Se transforma la información espacial a EPSG:4326,
# requerido para su visualización en Leaflet.

Encuestas_sf <- sf::st_transform(
  Encuestas_sf,
  crs = 4326
)

Tomas_sf <- sf::st_transform(
  Tomas_sf,
  crs = 4326
)

Cuenca <- sf::st_transform(
  Cuenca,
  crs = 4326
)

Parroquias <- sf::st_transform(
  Parroquias,
  crs = 4326
)

# ------------------------------------------------------------
# 2.7. Comprobación de la preparación de los datos
# ------------------------------------------------------------

cat("Preparación de datos completada.\n")
cat("Encuestas:", nrow(Datos), "\n")
cat("Puntos de encuestas:", nrow(Encuestas_sf), "\n")
cat("Tomas identificadas:", nrow(Tomas_sf), "\n")
cat("Cuenca:", nrow(Cuenca), "polígono(s)\n")
cat("Parroquias:", nrow(Parroquias), "\n")

# ============================================================
# 3. CONSTRUCCIÓN DE LISTAS DE OPCIONES PARA LOS FILTROS
# ============================================================

# ------------------------------------------------------------
# 3.1. Opciones para el filtro de sexo
# ------------------------------------------------------------
opciones_sexo <- c(
  "Todos",
  sort(unique(stats::na.omit(Datos$Sexo)))
)

# ------------------------------------------------------------
# 3.2. Opciones para el filtro de toma
# ------------------------------------------------------------
opciones_toma <- c(
  "Todas",
  sort(unique(stats::na.omit(as.character(Datos$Toma))))
)

# ============================================================
# 4. CONSTRUCCIÓN DE LA INTERFAZ DE USUARIO (UI)
# ============================================================

ui <- bslib::page_sidebar(
  
  title = "Mujeres y gobernanza del agua",
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
# ----------------------------------------------------------
# 4.1. Panel lateral: filtros
# ----------------------------------------------------------
sidebar = bslib::sidebar(
    
 title = "Filtros de consulta",
    
  selectInput(
      inputId = "sexo",
      label = "Sexo de las personas encuestadas:",
      choices = opciones_sexo,
      selected = "Todos"
    ),
    
  selectInput(
      inputId = "toma",
      label = "Toma de agua:",
      choices = opciones_toma,
      selected = "Todas"
    ),
    
  hr(),
    
  p(
      "Seleccione una o ambas opciones para explorar
      la información de las encuestas."
    )
  ),
  
# ----------------------------------------------------------
# 4.2. Indicadores principales
# ----------------------------------------------------------
  
  uiOutput("resumen"),
  
# ----------------------------------------------------------
# 4.3. Gráfico y mapa
# ----------------------------------------------------------
  
  bslib::layout_columns(
    
    bslib::card(
      bslib::card_header(
        "Participación en la directiva según sexo"
      ),
      plotlyOutput("grafico_directiva", height = "350px")
    ),
    
    bslib::card(
      bslib::card_header(
        "Distribución espacial de las personas encuestadas"
      ),
      leafletOutput("mapa_encuestas", height = "350px")
    ),
    
    col_widths = c(6, 6)
  ),
  
# ----------------------------------------------------------
# 4.4. Tabla de información
# ----------------------------------------------------------
  
  bslib::card(
    bslib::card_header(
      "Registros de las personas encuestadas"
    ),
    DTOutput("tabla_encuestas")
  ),
  
# ----------------------------------------------------------
# 4.5. Fuentes de información
# ----------------------------------------------------------
  
  bslib::card(
    bslib::card_header("Fuentes de información"),
    p(
      "Encuestas del estudio: Factores que inciden en la
      representatividad de las mujeres para la toma de
      decisiones en la gestión del recurso hídrico."
    ),
    p(
      "Capas geográficas: cuenca del río Machángara y
      parroquias del área de estudio."
    ),
    p(
      "Repositorio de datos: ",
      a(
        "GitHub - Trabajo_Final_Chumi",
        href = "https://github.com/isabelchumip01-del/Trabajo_Final_Chumi",
        target = "_blank"
      )
    )
  )
)


# ============================================================
# 5. CONSTRUCCIÓN DEL SERVIDOR (SERVER)
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # 5.1. Expresión reactiva para filtrar los datos
  # ----------------------------------------------------------
  
  # Los filtros seleccionados se aplican a la matriz de
  # encuestas. Los resultados alimentan todos los elementos
  # de la aplicación.
  
  Datos_filtrados <- reactive({
    
    resultado <- Datos
    
    # Filtro por sexo
    
    if (input$sexo != "Todos") {
      
      resultado <- resultado |>
        dplyr::filter(Sexo == input$sexo)
      
    }
    
    # Filtro por toma de agua
    
    if (input$toma != "Todas") {
      
      resultado <- resultado |>
        dplyr::filter(as.character(Toma) == input$toma)
      
    }
    
    resultado
    
  })
  
  
  # ----------------------------------------------------------
  # 5.2. Indicadores principales
  # ----------------------------------------------------------
  
  output$resumen <- renderUI({
    
    datos <- Datos_filtrados()
    
    total_personas <- nrow(datos)
    
    total_mujeres <- sum(
      datos$Sexo == "Femenino",
      na.rm = TRUE
    )
    
    total_directiva <- sum(
      datos$Directiva == "Sí",
      na.rm = TRUE
    )
    
    bslib::layout_columns(
      
      bslib::value_box(
        title = "Personas encuestadas",
        value = total_personas,
        showcase = bsicons::bs_icon("people")
      ),
      
      bslib::value_box(
        title = "Mujeres encuestadas",
        value = total_mujeres,
        showcase = bsicons::bs_icon("person")
      ),
      
      bslib::value_box(
        title = "Personas que pertenecen a una directiva",
        value = total_directiva,
        showcase = bsicons::bs_icon("person-check")
      ),
      
      col_widths = c(4, 4, 4)
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 5.3. Gráfico de participación en las directivas
  # ----------------------------------------------------------
  
  output$grafico_directiva <- renderPlotly({
    
    datos <- Datos_filtrados()
    
    # Se excluyen los registros sin información en las
    # variables necesarias para este gráfico.
    
    datos_grafico <- datos |>
      dplyr::filter(
        !is.na(Sexo),
        !is.na(Directiva)
      ) |>
      dplyr::count(
        Sexo,
        Directiva,
        name = "Personas"
      )
    
    validate(
      need(
        nrow(datos_grafico) > 0,
        "No existen registros disponibles para esta combinación de filtros."
      )
    )
    
    plotly::plot_ly(
      data = datos_grafico,
      x = ~Sexo,
      y = ~Personas,
      color = ~Directiva,
      type = "bar",
      text = ~Personas,
      textposition = "auto"
    ) |>
      plotly::layout(
        barmode = "group",
        xaxis = list(
          title = "Sexo"
        ),
        yaxis = list(
          title = "Número de personas"
        ),
        legend = list(
          title = list(text = "Pertenece a la directiva")
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 5.4. Mapa de distribución espacial
  # ----------------------------------------------------------
  
  output$mapa_encuestas <- renderLeaflet({
    
    datos <- Datos_filtrados()
    
    validate(
      need(
        nrow(datos) > 0,
        "No existen registros para representar en el mapa."
      )
    )
    
    # Se calcula el número de personas encuestadas
    # en cada toma de agua.
    
    Resumen_tomas <- datos |>
      dplyr::count(
        Toma,
        name = "Personas"
      )
    
    # Se relacionan los conteos con las ubicaciones
    # geográficas de las tomas.
    
    Mapa_tomas <- Tomas_sf |>
      dplyr::left_join(
        Resumen_tomas,
        by = "Toma"
      ) |>
      dplyr::filter(!is.na(Personas))
    
    validate(
      need(
        nrow(Mapa_tomas) > 0,
        "No hay ubicaciones disponibles para mostrar."
      )
    )
    
    # Se obtiene la extensión geográfica de la cuenca.
    
    limites <- sf::st_bbox(Cuenca)
    
    # Construcción del mapa.
    
    leaflet::leaflet() |>
      
      leaflet::addProviderTiles(
        providers$CartoDB.Positron
      ) |>
      
      leaflet::addPolygons(
        data = Parroquias,
        fillColor = "#DDE8E5",
        fillOpacity = 0.25,
        color = "#879B91",
        weight = 1,
        group = "Parroquias"
      ) |>
      
      leaflet::addPolygons(
        data = Cuenca,
        fill = FALSE,
        color = "#176B52",
        weight = 3,
        group = "Cuenca"
      ) |>
      
      leaflet::addCircleMarkers(
        data = Mapa_tomas,
        radius = ~pmax(5, sqrt(Personas) * 3),
        color = "#176B52",
        fillColor = "#43A58A",
        fillOpacity = 0.8,
        weight = 1,
        popup = ~paste0(
          "<strong>Toma: </strong>", Toma,
          "<br><strong>Personas encuestadas: </strong>",
          Personas
        )
      ) |>
      
      leaflet::fitBounds(
        lng1 = limites[["xmin"]],
        lat1 = limites[["ymin"]],
        lng2 = limites[["xmax"]],
        lat2 = limites[["ymax"]]
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 5.5. Tabla de registros
  # ----------------------------------------------------------
  
  output$tabla_encuestas <- DT::renderDT({
    
    datos <- Datos_filtrados()
    
    validate(
      need(
        nrow(datos) > 0,
        "No existen registros para mostrar en la tabla."
      )
    )
    
    # Se seleccionan las variables principales para
    # facilitar la consulta de la información.
    
    tabla <- datos |>
      dplyr::select(
        dplyr::any_of(
          c(
            "Toma",
            "Sexo",
            "Edad",
            "Educación",
            "Actividad",
            "Directiva",
            "Cargo_que_ocupa"
          )
        )
      )
    
    DT::datatable(
      tabla,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
        )
      )
    )
    
  })
  
}


# ============================================================
# 6. EJECUCIÓN DE LA APLICACIÓN
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

# ============================================================
# APLICACIÓN SHINY
# Mujeres y gobernanza del agua
# Junta de Riego y Drenaje Machángara - Ecuador
# ============================================================


# 1. PAQUETES -------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(readr)
library(dplyr)
library(sf)
library(DT)
library(plotly)
library(leaflet)
library(tidyr)
library(stringr)


# 2. DESCARGA Y PREPARACIÓN DE DATOS --------------------------

url_datos <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/Datos_Limpios_Machangara.csv"

url_cuenca <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/cuenca_machangara.gpkg"

url_parroquias <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/parroquias_cuenca.gpkg"


# 2.1. Lectura de la matriz de encuestas

Datos <- read_csv(
  url_datos,
  show_col_types = FALSE
)


# 2.2. Descarga de las capas geográficas

archivo_cuenca <- tempfile(fileext = ".gpkg")
archivo_parroquias <- tempfile(fileext = ".gpkg")

download.file(
  url_cuenca,
  archivo_cuenca,
  mode = "wb",
  quiet = TRUE
)

download.file(
  url_parroquias,
  archivo_parroquias,
  mode = "wb",
  quiet = TRUE
)


# 2.3. Lectura de las capas espaciales

Cuenca <- st_read(
  archivo_cuenca,
  quiet = TRUE
)

Parroquias <- st_read(
  archivo_parroquias,
  quiet = TRUE
)


# 2.4. Transformación a WGS84 para Leaflet

Cuenca <- st_transform(Cuenca, 4326)
Parroquias <- st_transform(Parroquias, 4326)


# 2.5. Selección de las mujeres encuestadas

Mujeres <- Datos |>
  filter(Sexo == "Femenino")


# 2.6. Creación de la capa espacial de encuestas

Encuestas_sf <- st_as_sf(
  Datos,
  coords = c("Coordenadas X", "Coordenadas Y"),
  crs = 32717,
  remove = FALSE
)

Encuestas_sf <- st_transform(
  Encuestas_sf,
  4326
)


# 2.7. Identificación de las tomas de agua

Tomas_sf <- Datos |>
  distinct(
    Toma,
    `Coordenadas X`,
    `Coordenadas Y`
  ) |>
  st_as_sf(
    coords = c("Coordenadas X", "Coordenadas Y"),
    crs = 32717,
    remove = FALSE
  )

Tomas_sf <- st_transform(
  Tomas_sf,
  4326
)


# 2.8. FUNCIÓN PARA ORDENAR LAS TOMAS DE AGUA
# Orden natural: 1, 2, 3, 4A, 4B, 5, 6, 7, 8A...
# También conserva nombres compuestos como "2 Y 3".

ordenar_tomas <- function(x) {
  
  x <- unique(
    na.omit(
      str_trim(as.character(x))
    )
  )
  
  x <- x[x != ""]
  
  partes <- str_match(
    x,
    "^\\s*(\\d+)\\s*(.*)$"
  )
  
  numero <- suppressWarnings(
    as.numeric(partes[, 2])
  )
  
  sufijo <- str_to_upper(
    str_squish(partes[, 3])
  )
  
  orden <- order(
    is.na(numero),
    numero,
    sufijo,
    str_to_upper(x),
    na.last = TRUE
  )
  
  x[orden]
}


# 2.9. Opciones de los filtros

opciones_tomas <- ordenar_tomas(Datos$Toma)

opciones_edad <- sort(
  unique(as.character(na.omit(Mujeres$Edad)))
)

opciones_educacion <- sort(
  unique(as.character(na.omit(Mujeres$Educación)))
)


# 2.10. Funciones auxiliares

es_si <- function(x) {
  
  !is.na(x) &
    str_to_lower(str_trim(as.character(x))) %in%
    c("sí", "si", "1", "verdadero", "true")
  
}


# 3. INTERFAZ DE LA APLICACIÓN -------------------------------

ui <- page_sidebar(
  
  title = "Mujeres y gobernanza del agua",
  
  sidebar = sidebar(
    title = "Acerca de la aplicación",
    
    p(
      "Esta aplicación explora la participación de las mujeres ",
      "en la Junta de Riego y Drenaje Machángara."
    ),
    
    p(
      "Utilice el selector de toma de agua de la primera sección ",
      "para actualizar el mapa, los indicadores, el gráfico por ",
      "toma, el gráfico por edad y la tabla."
    ),
    
    p(
      "Los filtros de edad y nivel educativo de la última sección ",
      "se aplican únicamente al gráfico de factores limitantes."
    ),
    
    hr(),
    
    h5("Procedencia de los datos"),
    
    p(
      "La información utilizada en esta aplicación procede de ",
      "la matriz de encuestas aplicada a usuarios y usuarias de ",
      "la Junta de Riego y Drenaje Machángara y de las capas ",
      "geográficas utilizadas para representar el territorio."
    ),
    
    p(
      strong("Matriz de encuestas: "),
      "base de datos depurada de la encuesta."
    ),
    
    p(
      strong("Información espacial: "),
      "capas geográficas de la cuenca del río Machángara ",
      "y de las parroquias incluidas en el área de estudio."
    ),
    
    p(
      a(
        "Consultar los datos y archivos geográficos en GitHub",
        href = "https://github.com/isabelchumip01-del/Trabajo_Final_Chumi",
        target = "_blank"
      )
    )
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  tags$head(
    tags$style(HTML("

      body {
        background-color: #F7F9FA;
      }

      .seccion-row {
        margin-bottom: 1.25rem;
      }

      .seccion-row > [class*='col-'] {
        display: flex;
        min-width: 0;
        margin-bottom: 1rem;
      }

      .seccion-row .card {
        width: 100%;
        min-width: 0;
      }

      .panel-superior {
        min-height: 570px;
      }

      .panel-mapa {
        min-width: 0;
      }

      .indicadores-columna {
        display: flex;
        flex-direction: column;
        gap: 10px;
        width: 100%;
      }

      .indicadores-columna .bslib-value-box {
        min-height: 125px;
      }

      .grafico-principal {
        min-height: 500px;
        margin-bottom: 1.25rem;
      }

      .grafico-secundario {
        min-height: 440px;
      }

      .tabla-resumen {
        min-height: 440px;
      }

      .filtros-card {
        min-height: 440px;
      }

      .factores-card {
        min-height: 440px;
      }

      .plotly,
      .leaflet,
      .dataTables_wrapper {
        max-width: 100%;
      }

      @media (max-width: 991px) {

        .panel-superior,
        .grafico-principal,
        .grafico-secundario,
        .tabla-resumen,
        .filtros-card,
        .factores-card {
          min-height: auto;
        }

        .seccion-row > [class*='col-'] {
          margin-bottom: 1rem;
        }

      }

    "))
  ),
  
  
  # ==========================================================
  # SECCIÓN 1. FILTRO DE TOMAS, MAPA E INDICADORES
  # ==========================================================
  
  div(
    class = "seccion-row",
    
    fluidRow(
      
      column(
        width = 3,
        
        card(
          class = "panel-superior",
          
          card_header(
            "Distribución espacial de las tomas de agua"
          ),
          
          card_body(
            
            selectInput(
              inputId = "toma",
              label = "Seleccione una toma de agua:",
              
              choices = c(
                "Todas las tomas" = "__TODAS__",
                setNames(opciones_tomas, opciones_tomas)
              ),
              
              selected = "__TODAS__",
              width = "100%"
            ),
            
            hr(),
            
            uiOutput("nombre_toma")
            
          )
        )
      ),
      
      
      column(
        width = 6,
        
        card(
          class = "panel-superior panel-mapa",
          
          card_header(
            "Ubicación espacial de las tomas"
          ),
          
          leafletOutput(
            "mapa_tomas",
            height = "520px",
            width = "100%"
          )
        )
      ),
      
      
      column(
        width = 3,
        
        uiOutput("indicadores")
        
      )
      
    )
  ),
  
  
  # ==========================================================
  # SECCIÓN 2. PARTICIPACIÓN EN DIRECTIVAS POR TOMA
  # ==========================================================
  
  card(
    class = "grafico-principal",
    
    card_header(
      "Participación de las mujeres en directivas por toma"
    ),
    
    plotlyOutput(
      "grafico_cargos",
      height = "430px",
      width = "100%"
    )
  ),
  
  
  # ==========================================================
  # SECCIÓN 3. PARTICIPACIÓN SEGÚN EDAD Y TABLA
  # ==========================================================
  
  div(
    class = "seccion-row",
    
    fluidRow(
      
      column(
        width = 6,
        
        card(
          class = "grafico-secundario",
          
          card_header(
            "Participación de las mujeres en directivas según grupo de edad"
          ),
          
          plotlyOutput(
            "grafico_edad",
            height = "390px",
            width = "100%"
          )
        )
      ),
      
      
      column(
        width = 6,
        
        card(
          class = "tabla-resumen",
          
          card_header(
            "Resumen de participación de las mujeres por toma de agua"
          ),
          
          DTOutput(
            "tabla_resumen",
            width = "100%"
          )
        )
      )
      
    )
  ),
  
  
  # ==========================================================
  # SECCIÓN 4. FACTORES QUE LIMITAN LA PARTICIPACIÓN
  # ==========================================================
  
  div(
    class = "seccion-row",
    
    fluidRow(
      
      column(
        width = 3,
        
        card(
          class = "filtros-card",
          
          card_header(
            "Filtros de análisis"
          ),
          
          card_body(
            
            p(
              "Explore los factores que limitan la participación ",
              "de las mujeres según la edad y el nivel educativo."
            ),
            
            selectInput(
              inputId = "edad_analisis",
              label = "Grupo de edad:",
              
              choices = c(
                "Todas las edades",
                opciones_edad
              ),
              
              selected = "Todas las edades",
              width = "100%"
            ),
            
            hr(),
            
            selectInput(
              inputId = "educacion_analisis",
              label = "Nivel educativo:",
              
              choices = c(
                "Todos los niveles",
                opciones_educacion
              ),
              
              selected = "Todos los niveles",
              width = "100%"
            )
            
          )
        )
      ),
      
      
      column(
        width = 9,
        
        card(
          class = "factores-card",
          
          card_header(
            "Factores que limitan la participación de las mujeres"
          ),
          
          plotlyOutput(
            "grafico_factores",
            height = "400px",
            width = "100%"
          )
        )
      )
      
    )
  ),
  
  
  # ==========================================================
  # PROCEDENCIA DE LOS DATOS
  # ==========================================================
  
  card(
    
    card_header(
      "Fuentes y procedencia de la información"
    ),
    
    h6("1. Matriz de encuestas"),
    
    p(
      "Elaboración propia con base en la encuesta aplicada a ",
      "usuarios y usuarias de la Junta de Riego y Drenaje ",
      "Machángara. Para los análisis de participación femenina ",
      "se consideran las respuestas de las mujeres encuestadas."
    ),
    
    h6("2. Información geográfica"),
    
    p(
      "Capas geográficas de la cuenca del río Machángara ",
      "y de las parroquias utilizadas para la representación ",
      "espacial del área de estudio."
    ),
    
    h6("3. Repositorio de datos"),
    
    p(
      a(
        "Consultar matriz de encuestas y archivos geográficos en GitHub",
        href = "https://github.com/isabelchumip01-del/Trabajo_Final_Chumi",
        target = "_blank"
      )
    ),
    
    p(
      class = "text-muted",
      "Elaboración propia."
    )
    
  )
  
)


# 4. SERVIDOR -------------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # 4.1. TOMA SELECCIONADA
  # ----------------------------------------------------------
  
  toma_seleccionada <- reactive({
    
    req(input$toma)
    
    input$toma
    
  })
  
  
  # ----------------------------------------------------------
  # 4.2. FILTRO INDEPENDIENTE PARA EL ANÁLISIS
  # ----------------------------------------------------------
  
  Mujeres_analisis <- reactive({
    
    resultado <- Mujeres
    
    if (
      !is.null(input$edad_analisis) &&
      input$edad_analisis != "Todas las edades"
    ) {
      
      resultado <- resultado |>
        filter(
          as.character(Edad) == input$edad_analisis
        )
      
    }
    
    if (
      !is.null(input$educacion_analisis) &&
      input$educacion_analisis != "Todos los niveles"
    ) {
      
      resultado <- resultado |>
        filter(
          as.character(Educación) == input$educacion_analisis
        )
      
    }
    
    resultado
    
  })
  
  
  # ----------------------------------------------------------
  # 4.3. MUJERES SEGÚN LA TOMA SELECCIONADA
  # ----------------------------------------------------------
  
  Mujeres_toma <- reactive({
    
    if (toma_seleccionada() == "__TODAS__") {
      
      Mujeres
      
    } else {
      
      Mujeres |>
        filter(
          as.character(Toma) == toma_seleccionada()
        )
      
    }
    
  })
  
  
  # ----------------------------------------------------------
  # 4.4. RESUMEN DE PARTICIPACIÓN POR TOMA
  # ----------------------------------------------------------
  
  Resumen_tomas <- reactive({
    
    Mujeres |>
      mutate(
        Toma = as.character(Toma),
        
        Tiene_directiva = es_si(Directiva),
        
        Cargo_limpio = ifelse(
          is.na(Cargo_que_ocupa) |
            trimws(as.character(Cargo_que_ocupa)) == "",
          NA_character_,
          trimws(as.character(Cargo_que_ocupa))
        )
      ) |>
      group_by(Toma) |>
      summarise(
        
        Mujeres_encuestadas = n(),
        
        Mujeres_en_directivas = sum(
          Tiene_directiva,
          na.rm = TRUE
        ),
        
        Cargo_que_ocupa = {
          
          cargos <- unique(
            Cargo_limpio[
              Tiene_directiva &
                !is.na(Cargo_limpio)
            ]
          )
          
          if (length(cargos) == 0) {
            
            "Sin cargos reportados"
            
          } else {
            
            paste(sort(cargos), collapse = "; ")
            
          }
          
        },
        
        .groups = "drop"
        
      ) |>
      mutate(
        Orden_toma = match(Toma, opciones_tomas)
      ) |>
      arrange(Orden_toma) |>
      select(-Orden_toma)
    
  })
  
  
  # ----------------------------------------------------------
  # 4.5. NOMBRE DE LA TOMA SELECCIONADA
  # ----------------------------------------------------------
  
  output$nombre_toma <- renderUI({
    
    if (toma_seleccionada() == "__TODAS__") {
      
      h5("Se muestran todas las tomas de agua.")
      
    } else {
      
      h5(
        paste(
          "Toma seleccionada:",
          toma_seleccionada()
        )
      )
      
    }
    
  })
  
  
  # ----------------------------------------------------------
  # 4.6. INDICADORES SUPERIORES
  # ----------------------------------------------------------
  
  output$indicadores <- renderUI({
    
    datos <- Mujeres_toma()
    
    numero_mujeres <- nrow(datos)
    
    numero_directivas <- sum(
      es_si(datos$Directiva),
      na.rm = TRUE
    )
    
    div(
      class = "indicadores-columna",
      
      value_box(
        
        title = "Mujeres encuestadas",
        
        value = numero_mujeres,
        
        showcase = bs_icon("people-fill"),
        
        theme = "primary"
        
      ),
      
      value_box(
        
        title = "Mujeres que pertenecen a directivas",
        
        value = numero_directivas,
        
        showcase = bs_icon("person-badge-fill"),
        
        theme = "success"
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 4.7. MAPA DE TOMAS DE AGUA
  # ----------------------------------------------------------
  
  output$mapa_tomas <- renderLeaflet({
    
    resumen <- Resumen_tomas()
    
    Mapa_tomas <- Tomas_sf |>
      mutate(
        Toma = as.character(Toma)
      ) |>
      left_join(
        resumen,
        by = "Toma"
      )
    
    Mapa_tomas$Mujeres_encuestadas[
      is.na(Mapa_tomas$Mujeres_encuestadas)
    ] <- 0
    
    Mapa_tomas$Mujeres_en_directivas[
      is.na(Mapa_tomas$Mujeres_en_directivas)
    ] <- 0
    
    
    # Resaltar la toma seleccionada
    
    if (toma_seleccionada() == "__TODAS__") {
      
      Mapa_tomas$color <- "#27AE60"
      
    } else {
      
      Mapa_tomas$color <- ifelse(
        Mapa_tomas$Toma == toma_seleccionada(),
        "#F39C12",
        "#27AE60"
      )
      
    }
    
    
    # Tamaño de los puntos según mujeres encuestadas
    
    Mapa_tomas$radio <- 5 +
      Mapa_tomas$Mujeres_encuestadas * 0.15
    
    
    leaflet() |>
      
      addProviderTiles(
        providers$OpenStreetMap.Mapnik
      ) |>
      
      addPolygons(
        data = Cuenca,
        color = "#34495E",
        weight = 2,
        fillOpacity = 0.05,
        group = "Cuenca"
      ) |>
      
      addPolygons(
        data = Parroquias,
        color = "#7F8C8D",
        weight = 1,
        fillOpacity = 0.05,
        group = "Parroquias"
      ) |>
      
      addCircleMarkers(
        data = Mapa_tomas,
        radius = Mapa_tomas$radio,
        color = Mapa_tomas$color,
        fillColor = Mapa_tomas$color,
        fillOpacity = 0.85,
        weight = 2,
        
        popup = paste0(
          "<b>Toma:</b> ",
          Mapa_tomas$Toma,
          "<br><b>Mujeres encuestadas:</b> ",
          Mapa_tomas$Mujeres_encuestadas,
          "<br><b>Mujeres en directivas:</b> ",
          Mapa_tomas$Mujeres_en_directivas
        ),
        
        group = "Tomas de agua"
      ) |>
      
      addLegend(
        position = "bottomright",
        colors = c("#F39C12", "#27AE60"),
        
        labels = c(
          "Toma seleccionada",
          "Otras tomas"
        ),
        
        title = "Tomas de agua",
        opacity = 1
      ) |>
      
      addScaleBar(
        position = "bottomleft"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "Cuenca",
          "Parroquias",
          "Tomas de agua"
        ),
        
        options = layersControlOptions(
          collapsed = TRUE
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 4.8. GRÁFICO DE PARTICIPACIÓN POR TOMA
  # ----------------------------------------------------------
  
  output$grafico_cargos <- renderPlotly({
    
    resumen <- Resumen_tomas()
    
    if (toma_seleccionada() != "__TODAS__") {
      
      resumen <- resumen |>
        filter(
          Toma == toma_seleccionada()
        )
      
    }
    
    validate(
      need(
        nrow(resumen) > 0,
        "No existen datos para la toma seleccionada."
      )
    )
    
    
    resumen <- resumen |>
      mutate(
        Porcentaje = ifelse(
          Mujeres_encuestadas > 0,
          Mujeres_en_directivas / Mujeres_encuestadas * 100,
          0
        ),
        
        Color = ifelse(
          Toma == toma_seleccionada(),
          "#F39C12",
          "#27AE60"
        )
      )
    
    if (toma_seleccionada() == "__TODAS__") {
      
      resumen$Color <- "#27AE60"
      
    }
    
    
    plot_ly(
      data = resumen,
      x = ~Toma,
      y = ~Mujeres_en_directivas,
      type = "bar",
      
      marker = list(
        color = ~Color
      ),
      
      text = ~Mujeres_en_directivas,
      textposition = "outside",
      
      customdata = ~Porcentaje,
      
      hovertemplate = paste(
        "<b>Toma:</b> %{x}",
        "<br><b>Mujeres en directivas:</b> %{y}",
        "<br><b>Porcentaje:</b> %{customdata:.1f}%",
        "<extra></extra>"
      )
      
    ) |>
      
      layout(
        
        xaxis = list(
          title = "Toma de agua",
          categoryorder = "array",
          categoryarray = opciones_tomas,
          tickangle = -45,
          automargin = TRUE
        ),
        
        yaxis = list(
          title = "Número de mujeres",
          rangemode = "tozero"
        ),
        
        margin = list(
          b = 110,
          t = 30
        )
        
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 4.9. GRÁFICO DE PARTICIPACIÓN SEGÚN GRUPO DE EDAD
  # ----------------------------------------------------------
  
  output$grafico_edad <- renderPlotly({
    
    datos <- Mujeres_toma() |>
      filter(
        !is.na(Edad),
        trimws(as.character(Edad)) != ""
      ) |>
      mutate(
        Edad = as.character(Edad),
        Tiene_directiva = es_si(Directiva)
      ) |>
      group_by(Edad) |>
      summarise(
        Mujeres_encuestadas = n(),
        Mujeres_en_directivas = sum(
          Tiene_directiva,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      arrange(Edad)
    
    validate(
      need(
        nrow(datos) > 0,
        "No existen datos para representar la participación por edad en la toma seleccionada."
      )
    )
    
    plot_ly(
      data = datos,
      x = ~Edad,
      y = ~Mujeres_en_directivas,
      type = "bar",
      
      marker = list(
        color = "#3498DB"
      ),
      
      text = ~Mujeres_en_directivas,
      textposition = "outside",
      
      customdata = ~Mujeres_encuestadas,
      
      hovertemplate = paste(
        "<b>Grupo de edad:</b> %{x}",
        "<br><b>Mujeres en directivas:</b> %{y}",
        "<br><b>Mujeres encuestadas:</b> %{customdata}",
        "<extra></extra>"
      )
      
    ) |>
      
      layout(
        
        xaxis = list(
          title = "Grupo de edad",
          tickangle = -25,
          automargin = TRUE
        ),
        
        yaxis = list(
          title = "Número de mujeres en directivas",
          rangemode = "tozero",
          dtick = 1
        ),
        
        font = list(size = 11),
        
        margin = list(
          b = 80,
          l = 65,
          r = 25,
          t = 25
        )
        
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 4.10. GRÁFICO DE FACTORES LIMITANTES
  # ----------------------------------------------------------
  
  output$grafico_factores <- renderPlotly({
    
    datos <- Mujeres_analisis()
    
    validate(
      need(
        nrow(datos) > 0,
        "No existen mujeres para los filtros seleccionados."
      )
    )
    
    
    factores <- datos |>
      filter(
        !is.na(Factores_limintan_participación_mujeres),
        trimws(Factores_limintan_participación_mujeres) != ""
      ) |>
      select(
        Factores_limintan_participación_mujeres
      ) |>
      separate_rows(
        Factores_limintan_participación_mujeres,
        sep = ","
      ) |>
      mutate(
        Factor = str_trim(
          Factores_limintan_participación_mujeres
        )
      ) |>
      filter(
        Factor != "",
        !str_detect(
          Factor,
          regex(
            "^Más mujeres participan",
            ignore_case = TRUE
          )
        )
      ) |>
      count(
        Factor,
        sort = TRUE
      )
    
    
    validate(
      need(
        nrow(factores) > 0,
        "No se registraron factores limitantes para los filtros seleccionados."
      )
    )
    
    
    plot_ly(
      data = factores,
      
      x = ~n,
      
      y = ~reorder(Factor, n),
      
      type = "bar",
      
      orientation = "h",
      
      marker = list(
        color = "#3498DB"
      ),
      
      text = ~n,
      
      textposition = "outside",
      
      hovertemplate = paste(
        "<b>Factor:</b> %{y}",
        "<br><b>Frecuencia:</b> %{x}",
        "<extra></extra>"
      )
      
    ) |>
      
      layout(
        
        xaxis = list(
          title = "Número de respuestas",
          rangemode = "tozero"
        ),
        
        yaxis = list(
          title = "",
          automargin = TRUE
        ),
        
        font = list(
          size = 10
        ),
        
        margin = list(
          l = 180,
          r = 35
        )
        
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 4.11. TABLA RESUMEN POR TOMA
  # ----------------------------------------------------------
  
  output$tabla_resumen <- renderDT({
    
    resumen_completo <- Resumen_tomas()
    
    if (toma_seleccionada() != "__TODAS__") {
      
      resumen_completo <- resumen_completo |>
        filter(
          Toma == toma_seleccionada()
        )
      
    }
    
    validate(
      need(
        nrow(resumen_completo) > 0,
        "No existen registros para la toma seleccionada."
      )
    )
    
    resumen <- resumen_completo |>
      rename(
        `Toma de agua` = Toma,
        `Mujeres encuestadas` = Mujeres_encuestadas,
        `Mujeres en las directivas` = Mujeres_en_directivas,
        `Cargo que ocupa` = Cargo_que_ocupa
      )
    
    
    datatable(
      
      resumen,
      
      rownames = FALSE,
      
      filter = "top",
      
      options = list(
        
        pageLength = 10,
        
        scrollX = TRUE,
        
        autoWidth = TRUE,
        
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json"
        )
        
      )
      
    )
    
  })
  
}


# 5. EJECUTAR APLICACIÓN -------------------------------------

shinyApp(
  ui = ui,
  server = server
)
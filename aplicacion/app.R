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

# 2.1. Direcciones de los archivos en GitHub

url_datos <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/Datos_Limpios_Machangara.csv"

url_cuenca <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/cuenca_machangara.gpkg"

url_parroquias <- "https://raw.githubusercontent.com/isabelchumip01-del/Trabajo_Final_Chumi/main/parroquias_cuenca.gpkg"


# 2.2. Lectura de la matriz de encuestas

Datos <- read_csv(
  url_datos,
  show_col_types = FALSE
)


# 2.3. Descarga de las capas geográficas

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


# 2.4. Lectura de las capas espaciales

Cuenca <- st_read(
  archivo_cuenca,
  quiet = TRUE
)

Parroquias <- st_read(
  archivo_parroquias,
  quiet = TRUE
)


# 2.5. Transformación a WGS84 para Leaflet

Cuenca <- st_transform(Cuenca, 4326)
Parroquias <- st_transform(Parroquias, 4326)


# 2.6. Selección de las mujeres encuestadas

Mujeres <- Datos |>
  filter(Sexo == "Femenino")


# 2.7. Creación de la capa espacial de encuestas

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


# 2.8. Identificación de las tomas de agua

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


# 2.9. Función para ordenar las tomas de forma natural

ordenar_tomas <- function(x) {
  
  x <- unique(as.character(x))
  
  x <- x[
    !is.na(x) &
      trimws(x) != ""
  ]
  
  # Extraer el componente numérico de cada toma
  
  numero <- suppressWarnings(
    as.numeric(
      str_extract(x, "\\d+")
    )
  )
  
  # Extraer las letras o sufijos de cada toma
  
  sufijo <- str_to_upper(
    str_trim(
      str_remove(x, "\\d+")
    )
  )
  
  # Ordenar primero por número y después por letra
  
  x[
    order(
      is.na(numero),
      numero,
      sufijo,
      x,
      na.last = TRUE
    )
  ]
}


# 2.10. Opciones de los filtros

opciones_tomas <- ordenar_tomas(Datos$Toma)

opciones_edad <- sort(
  unique(
    as.character(
      na.omit(Mujeres$Edad)
    )
  )
)

opciones_educacion <- sort(
  unique(
    as.character(
      na.omit(Mujeres$Educación)
    )
  )
)


# 2.11. Funciones auxiliares

es_si <- function(x) {
  
  !is.na(x) &
    str_to_lower(
      str_trim(as.character(x))
    ) %in%
    c("sí", "si", "1", "verdadero", "true")
  
}


# 3. INTERFAZ DE LA APLICACIÓN -------------------------------

ui <- page_fluid(
  
  title = "Mujeres y gobernanza del agua",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #F7F9FA;
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
      }

      .indicadores-columna .bslib-value-box {
        min-height: 125px;
      }

      .grafico-principal {
        min-height: 390px;
      }

      .grafico-secundario {
        min-height: 390px;
      }

      .fila-analisis {
        display: grid;
        grid-template-columns: 25fr 75fr;
        gap: 1rem;
        align-items: stretch;
      }

      .fila-analisis > * {
        min-width: 0;
      }

      @media (max-width: 900px) {

        .fila-analisis {
          grid-template-columns: 1fr;
        }

        .panel-superior {
          min-height: auto;
        }

      }

    "))
    
  ),
  
  
  # ----------------------------------------------------------
  # SECCIÓN 1. FILTRO DE TOMAS, MAPA E INDICADORES
  # ----------------------------------------------------------
  
  layout_columns(
    
    col_widths = c(3, 6, 3),
    
    gap = "1rem",
    
    
    # FILTRO DE TOMAS
    
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
          selected = "__TODAS__"
        ),
        
        hr(),
        
        uiOutput("nombre_toma")
        
      )
      
    ),
    
    
    # MAPA
    
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
      
    ),
    
    
    # INDICADORES
    
    uiOutput("indicadores")
    
  ),
  
  
  # ----------------------------------------------------------
  # SECCIÓN 2. PARTICIPACIÓN EN DIRECTIVAS
  # ----------------------------------------------------------
  
  layout_columns(
    
    col_widths = c(6, 6),
    
    gap = "1rem",
    
    
    # PARTICIPACIÓN POR TOMA
    
    card(
      
      class = "grafico-principal",
      
      full_screen = TRUE,
      
      card_header(
        "Participación de las mujeres en directivas por toma"
      ),
      
      plotlyOutput(
        "grafico_cargos",
        height = "350px"
      )
      
    ),
    
    
    # PARTICIPACIÓN SEGÚN GRUPO DE EDAD
    
    card(
      
      class = "grafico-principal",
      
      full_screen = TRUE,
      
      card_header(
        "Participación de las mujeres en directivas según grupo de edad"
      ),
      
      plotlyOutput(
        "grafico_edad",
        height = "350px"
      )
      
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # SECCIÓN 3. ANÁLISIS DE FACTORES
  # FILTROS INDEPENDIENTES DEL MAPA Y LAS TOMAS
  # ----------------------------------------------------------
  
  div(
    
    class = "fila-analisis",
    
    
    # FILTROS DE ANÁLISIS
    
    card(
      
      class = "filtros-analisis",
      
      card_header(
        "Filtros de análisis"
      ),
      
      card_body(
        
        p(
          "Explore la relación entre la edad, el nivel educativo y los factores que limitan la participación de las mujeres."
        ),
        
        selectInput(
          inputId = "edad_analisis",
          label = "Grupo de edad:",
          choices = c(
            "Todas las edades",
            opciones_edad
          ),
          selected = "Todas las edades"
        ),
        
        hr(),
        
        selectInput(
          inputId = "educacion_analisis",
          label = "Nivel educativo:",
          choices = c(
            "Todos los niveles",
            opciones_educacion
          ),
          selected = "Todos los niveles"
        )
        
      )
      
    ),
    
    
    # FACTORES LIMITANTES
    
    card(
      
      class = "grafico-analisis",
      
      full_screen = TRUE,
      
      card_header(
        "Factores que limitan la participación de las mujeres"
      ),
      
      plotlyOutput(
        "grafico_factores",
        height = "440px"
      )
      
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # SECCIÓN 4. TABLA RESUMEN
  # ----------------------------------------------------------
  
  card(
    
    full_screen = TRUE,
    
    card_header(
      "Resumen de participación de las mujeres por toma de agua"
    ),
    
    DTOutput("tabla_resumen")
    
  ),
  
  
  # ----------------------------------------------------------
  # FUENTES DE INFORMACIÓN
  # ----------------------------------------------------------
  
  card(
    
    card_header(
      "Fuente de información"
    ),
    
    p(
      "Elaboración propia con base en la encuesta aplicada a usuarios y usuarias de la Junta de Riego y Drenaje Machángara."
    ),
    
    p(
      a(
        "Consultar repositorio de datos en GitHub",
        href = "https://github.com/isabelchumip01-del/Trabajo_Final_Chumi",
        target = "_blank"
      )
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
    
    # Filtro por edad
    
    if (
      !is.null(input$edad_analisis) &&
      input$edad_analisis != "Todas las edades"
    ) {
      
      resultado <- resultado |>
        filter(
          as.character(Edad) == input$edad_analisis
        )
      
    }
    
    
    # Filtro por nivel educativo
    
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
        
        # Número de mujeres encuestadas por toma
        
        Mujeres_encuestadas = n(),
        
        
        # Número de mujeres que pertenecen a directivas
        
        Mujeres_en_directivas = sum(
          Tiene_directiva,
          na.rm = TRUE
        ),
        
        
        # Categorías de edad registradas por toma
        
        Edad = {
          
          edades <- unique(
            trimws(
              as.character(
                Edad[
                  !is.na(Edad) &
                    trimws(as.character(Edad)) != ""
                ]
              )
            )
          )
          
          if (length(edades) == 0) {
            
            "Sin dato"
            
          } else {
            
            paste(
              sort(edades),
              collapse = "; "
            )
            
          }
          
        },
        
        
        # Cargos reportados por las mujeres en directivas
        
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
            
            paste(
              sort(cargos),
              collapse = "; "
            )
            
          }
          
        },
        
        .groups = "drop"
        
      ) |>
      
      # Orden natural de las tomas
      
      arrange(
        match(Toma, opciones_tomas)
      )
    
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
    
    
    # Completar los valores sin información
    
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
    
    
    # Construcción del mapa
    
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
    
    
    # Aplicar el filtro de toma
    
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
    
    
    # Calcular porcentajes y colores
    
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
    
    
    # Construcción del gráfico
    
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
    
    datos <- Mujeres |>
      
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
        
        Porcentaje = ifelse(
          n() > 0,
          sum(Tiene_directiva, na.rm = TRUE) / n() * 100,
          0
        ),
        
        .groups = "drop"
        
      ) |>
      
      arrange(Edad)
    
    
    validate(
      need(
        nrow(datos) > 0,
        "No existen datos para representar la participación por edad."
      )
    )
    
    
    # Construcción del gráfico
    
    plot_ly(
      
      data = datos,
      
      x = ~Edad,
      
      y = ~Porcentaje,
      
      type = "bar",
      
      marker = list(
        color = "#3498DB"
      ),
      
      text = ~paste0(
        round(Porcentaje, 1),
        "%"
      ),
      
      textposition = "outside",
      
      customdata = ~cbind(
        Mujeres_encuestadas,
        Mujeres_en_directivas
      ),
      
      hovertemplate = paste(
        "<b>Grupo de edad:</b> %{x}",
        "<br><b>Porcentaje en directivas:</b> %{y:.1f}%",
        "<br><b>Mujeres encuestadas:</b> %{customdata[0]}",
        "<br><b>Mujeres en directivas:</b> %{customdata[1]}",
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
          title = "Mujeres en directivas (%)",
          rangemode = "tozero",
          range = c(0, 110)
        ),
        
        font = list(
          size = 11
        ),
        
        margin = list(
          b = 80,
          l = 55,
          r = 25,
          t = 25
        )
        
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 4.10. GRÁFICO DE FACTORES LIMITANTES
  # FILTRO INDEPENDIENTE DE LAS TOMAS Y DEL MAPA
  # ----------------------------------------------------------
  
  output$grafico_factores <- renderPlotly({
    
    datos <- Mujeres_analisis()
    
    
    validate(
      need(
        nrow(datos) > 0,
        "No existen mujeres para los filtros seleccionados."
      )
    )
    
    
    # Separar las respuestas cuando una persona registró
    # más de un factor limitante
    
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
    
    
    # Construcción del gráfico horizontal
    
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
    
    resumen <- Resumen_tomas() |>
      
      rename(
        `Toma de agua` = Toma,
        `Mujeres encuestadas` = Mujeres_encuestadas,
        `Mujeres en las directivas` = Mujeres_en_directivas,
        `Edad (categorías registradas)` = Edad,
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

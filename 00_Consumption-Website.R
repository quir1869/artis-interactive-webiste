# Load packages
library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(plotly)


shiny::addResourcePath("images", here::here("data"))

create_map <- function(data, country.col.name = "consumer_iso3c", fill = "prop_missing") {
  world_map   <- ne_countries(scale = "medium", returnclass = "sf") %>%
                 left_join(data, by = c("iso_a3" = country.col.name))
  missing_ctry <- filter(world_map, is.na(!!sym(fill)))
  present_ctry <- filter(world_map, !is.na(!!sym(fill)))
  
  ggplot() +
    geom_sf(data = ocean, fill = "#ebf0f480") +
    
    # 1) Missing: white fill + BLACK border
    geom_sf(
      data      = missing_ctry,
      aes(geometry = geometry),
      fill      = "white",
      color     = "gray80",
      linewidth = 0.1        # thicker so it shows
    ) +
    
    # 2) Present: color fill + WHITE border
    geom_sf(
      data      = present_ctry,
      aes(fill   = !!sym(fill)),
      color     = "white",
      linewidth = 0.1
    ) +
    
    scale_fill_gradient(low = "#b0d2ff", high = "#0d65d7", na.value = NA) +
    coord_sf(crs = "+proj=robin") +
    theme_minimal() +
    labs(fill = "Aquatic animal/protein reliance") +
    theme(
      legend.position   = c(0.25, 0.08),
      legend.direction  = "horizontal",
      legend.background = element_rect(color = "black", fill = "white", size = 0.5),
      legend.key.width  = unit(1.5, "cm"),
      legend.ticks      = element_blank(),
      legend.title      = element_text(size = 5, face = "bold")
    ) +
    guides(fill = guide_colorbar(title.position = "top", barwidth = 5))
}

seafood_types <- read_rds(here("output", "seafood_types.rds"))

### WEBSITE CREATION

# 1) Define UI with navbarPage -------------------------------------
# 1) Define UI with navbarPage -------------------------------------
ui <- navbarPage(
  title = "Seafood Explorer",
  theme = bs_theme(version = 4, bootswatch = "cosmo"),
  
  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = "images/favicon.png"),
    tags$style(HTML("
    /* 1) gradient on the navbar itself */
    .navbar {
      position: relative;  /* so ::after can position itself */
      background: linear-gradient(
        -45deg,
        white 0%,
        white 33%,
        #0d64d7 33%,
        #0d64d7 100%
      ) !important;
    }
  ")),
  ),
  
  tags$img(
    src   = "images/ARTIS_logo.png", 
    style = paste(
      "position: absolute;",
      "top:    10px;",           # flush with top of wrapper
      "right:  20px;",           # flush with right edge
      "height: 30px;    ",    # adjust size
      "z-index: 2000;  ",     # > any navbar z-index
      "pointer-events: none;" # non-clickable
    )
  ),
  
  tabPanel("About",
           div(
             style = "text-align: center; padding: 0px; max-width: 850px; margin: 0 auto;",
             
             img(
               src = "images/SGL_logo.png", 
               style = "max-width: 300px; height: auto; margin-bottom: 0px;"
             ),
             
             h3("About the ARTIS Database"),
             
             p(style = "text-align: center;",
               "Food systems have become increasingly globalized, with over a quarter of all food now traded internationally. Seafood is among the most highly traded foods and it is becoming increasingly globalized, with trade doubling in recent decades. At the same time, seafood is now widely recognized as a critical source of nutrition. Thus, social and environmental threats to local seafood production, including environmental extremes, price impacts of market integration, networked risks, and increased availability of processed foods, must be evaluated in the context of global trade. These issues are paralleled by similar questions for other natural resources and are central to global food systems research. However, our collective understanding of the environmental and human outcomes of food system globalization is limited by a fundamental gap between production and trade data. We bridge this gap in the Aquatic Resource Trade in Species (ARTIS) database by providing the first global estimates of seafood species and nutrient trade flows from 1996–2020."
             )
           )),
  
  tabPanel("Most Consumed",
           fluidRow(
             style = "display: flex; align-items: center; min-height: 70vh;",  # 👈 makes the row vertically centered
             
             column(6,
                    div(
                      style = "text-align: center;",
                      h1("Most consumed seafood"),
                      plotOutput("plot_most_consumed")
                    )
             ),
             
             column(6,
                    div(
                      style = "text-align: center;",
                      h1("Where does our seafood come from?"),
                      plotOutput("plot_source")
                    )
             )
           )
  ),
  
  tabPanel("Global Distributions",
           fluidRow(
             column(12, align = "center",
                    h1("What does global seafood consumption look like?")
             )
           ),
           fluidRow(
             # 4-column wide input panel, flex-centered
             column(
               4,
               style = "
      background-color: #f8f9fa;
      padding: 15px;
      display: flex;
      justify-content: center;  /* horizontal centering */
      align-items: center;      /* vertical centering */
      height: 75vh;             /* match plot height */
    ",
               # you can drop the wellPanel if you like, or keep it for styling
               wellPanel(
                 style = "text-align: center; width: 100%;",
                 h4("Food type:"),
                 selectInput(
                   "sel_food", 
                   label    = NULL,
                   choices  = seafood_types, 
                   selected = "Salmons, trouts, smelts"
                 )
               )
             ),
             
             # 8-column wide plot area
             column(
               8,
               plotOutput(
                 "seafoodPlot",
                 height = "75vh", 
                 width  = "75%"
               )
             )
           )
           
  ),
  tabPanel("Protein reliance",
           # 1) Tighten up the header margins
           div(
             style = "text-align: center; margin: 5px 0 5px 0;",  # top 5px, bottom 5px
             h1(
               "How reliant are countries on seafood for protein?",
               style = "margin: 0; padding: 0;"                     # remove any h1 margin
             )
           ),
           
           # 2) Remove extra padding on the fluidRow
           fluidRow(
             style = "margin-top: 0; padding-top: 0;",
             column(
               12,
               plotlyOutput(
                 "plot_protein_reliance",
                 width  = "100%",
                 height = "80vh"
               )
             )
           )
  )
  
)

# 2) Server stays the same, just hooked to output IDs ---------------
server <- function(input, output, session) {
  
  output$plot_most_consumed <- renderPlot({
    print(readRDS(here("output", "USA_plot_1.rds")))
  })
  
  output$plot_source <- renderPlot({
    readRDS(here("output", "USA_plot_2.rds"))
  })
  
  output$seafoodPlot <- renderPlot({
    readRDS(here("output","worldwide_consumption", paste0(input$sel_food, ".rds")))
  })
  
  output$plot_protein_reliance <- renderPlotly({
    readRDS(here("output", "protein_map.rds"))
  })
}

# 3) Launch the app -------------------------------------------------
shinyApp(ui, server)
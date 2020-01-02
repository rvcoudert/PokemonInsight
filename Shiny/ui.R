library(magrittr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggnetwork)
print(getwd())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ----- preprocess -----


# Build pages by calling another Rscript.
for (script in list.files("pages/")) {
  source(
    file = paste("pages/", script, sep = ""),
    local = TRUE,
    encoding = "UTF-8"
  )
}


pokemon_df <- data.table::fread(
  input = "../data/Pokemon_final_fr.csv",
  encoding = "UTF-8"
)

type_colors <- data.table::fread(
  input = "../data/Type_Colors.csv",
  encoding = "UTF-8"
)

pokemon_melted <- pokemon_df %>%
  data.table::melt(
    measure.vars = c("Type1", "Type2"),
    variable.name = "TypeOrder",
    value.name = "Type",
    na.rm = TRUE
  ) %>%
  dplyr::mutate(
    TypeOrder = factor(
      x = TypeOrder,
      levels = c("Type2", "Type1"),
      ordered = TRUE)) %>%
  dplyr::arrange(TypeOrder)

nb_generations <- pokemon_melted$Generation %>% max

carac_list <-
  c("Total", "HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed")


# ----- dashboardPage -----


dashboardPage(
  # ----- dashboardHeader -----
  header = dashboardHeader(
    title = "ϞϞ(๑⚈ ․̫ ⚈๑)",
    tags$li(
      class = "dropdown",
      tags$li(
        class = "dropdown",
        "Sélection de générations"
      ),
      tags$li(
        class = "dropdown",
        style = "margin : 0px; line-height : 0px; font-size: 2px;",
        pickerInput(
          inputId = "generation_selection",
          choices = 1:nb_generations,
          selected = 1:nb_generations,
          inline = TRUE,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE
          )
        )
      ),
      tags$li(
        class = "dropdown",
        HTML(
          paste(
            textOutput(
              outputId = "nb_pokemons",
              container = tags$span),
            "Pokémons")
        )

      )
    )
  ),
  # ----- dashboardSidebar -----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tab",
      menuItem(
        text = "Accueil",
        tabName = "home",
        icon = icon("home")),
      menuItem(
        text = "Étude des types",
        tabName = "type",
        icon = icon("bolt"),
        menuSubItem(
          text = "Répartition Simple",
          tabName = "typeRep"
        ),
        menuSubItem(
          text = "Répartition Croisée",
          tabName = "typeRep2"
        ),
        menuSubItem(
          text = "Caractéristiques",
          tabName = "typeCarac"
        )),
      menuItem(
        text = "Étude de caractéristiques",
        tabName = "carac",
        icon = icon("chart-bar"),
        menuSubItem(
          text = "Univariée",
          tabName = "caracUniv"
        ),
        menuSubItem(
          text = "Bivariée",
          tabName = "caracBiv"
        )
      ),
      menuItem(
        text = "Accès aux données",
        tabName = "accessData",
        icon = icon("database"),
        selected = TRUE
      )
    )
  ),
  # ----- dashboardBody -----
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css")
    ),
    tabItems(
      # ----- __home -----
      page_home(),
      # ----- __typeRep -----
      page_typeRep(),
      # ----- __typeRep2 -----
      page_typeRep2(),
      # ----- __typeCarac -----
      page_typeCarac(),
      # ----- __caracUniv -----
      page_caracUniv(),
      # ----- __caracBiv -----
      page_caracBiv(),
      # ----- __accessData -----
      page_accessData()
    )
  ),
  skin = "red"
)

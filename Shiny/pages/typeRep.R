page_typeRep <- function()
{
  tabItem(
    tabName = "typeRep",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel(
        titlePanel(
          h1("Étude simple des types"),
        ),
        p("Répartition des Pokémons parmi les ",
          type_colors %>% nrow,
          " différents types."),
        p(style = "font-style: italic", "Options possibles :"),
        tags$ul(
          style = "font-style: italic",
          tags$li("Afficher en transparent les types secondaires."),
          tags$li("Afficher les effectifs en nombre."),
          tags$li("Fixer l'échelle. (Ceci est peut se révéler utile",
                  "pour comparer les générations entre elles.)")
        )
        # verticalTabsetPanel(
        #   # type = "tabs",
        #   id = "generation_selection_panel",
        #   verticalTabPanel(
        #     title = "Sélection multiple",
        #     checkboxGroupInput(
        #       inputId = "generation_selection_1",
        #       label = "",
        #       choiceNames = paste("Génération", 1:nb_generations),
        #       choiceValues = 1:nb_generations,
        #       selected = 1
        #     )
        #   ),
        #   verticalTabPanel(
        #     title = "Timeline par génération",
        #     color = "orange",
        #     sliderInput(
        #       inputId = "generation_selection_2",
        #       label = "",
        #       min = 1,
        #       max = nb_generations,
        #       value = 1,
        #       step = 1,
        #       animate = TRUE
        #     )
        #   )
        # )
        # tabsetPanel(
        #   type = "tabs",
        #   id = "generation_selection_panel",
        #   tabPanel(
        #     title = "Sélection multiple",
        #     # checkboxGroupInput(
        #     #   inputId = "generation_selection_1",
        #     #   label = "",
        #     #   choiceNames = paste("Génération", 1:nb_generations),
        #     #   choiceValues = 1:nb_generations,
        #     #   selected = 1:nb_generations
        #     # )
        #     pickerInput(
        #       inputId = "generation_selection_1",
        #       label = "",
        #       choices = 1:nb_generations,
        #       selected = 1:nb_generations,
        #       options = list(
        #         `actions-box` = TRUE,
        #         size = 10,
        #         `selected-text-format` = "values"
        #       ),
        #       multiple = TRUE
        #     )
        #   ),
        #   tabPanel(
        #     title = "Timeline par génération",
        #     sliderInput(
        #       inputId = "generation_selection_2",
        #       label = "",
        #       min = 1,
        #       max = nb_generations,
        #       value = 1,
        #       step = 1,
        #       animate = TRUE
        #     )
        #   )
        # )
      ),
      # ----- __main -----
      mainPanel(
        h3(
          textOutput(
            outputId = "typeRep_info"
          )
        ),
        plotOutput(
          height = "600px",
          outputId = "typeRep_plot"
        ),
        div(
          checkboxInput(
            inputId = "typeRep_gg_type2",
            label = "Type 2 inclus (en transparent)",
            value = TRUE
          ),
          style = "display: inline-block;"
        ),
        div(
          checkboxInput(
            inputId = "typeRep_gg_numbers",
            label = "Afficher les nombres",
            value = FALSE
          ),
          style = "display: inline-block;"
        ),
        div(
          checkboxInput(
            inputId = "typeRep_gg_fixed_layout",
            label = "Échelle fixe",
            value = TRUE
          ),
          style = "display: inline-block;"
        )
      )
    )
  )
}

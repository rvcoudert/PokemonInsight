page_typeCarac <- function()
{
  tabItem(
    tabName = "typeCarac",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel(
        titlePanel(
          h1("Étude des types"),
        ),
        p("Pour comparer les types entre eux, il est possible de",
          "s'appuyer sur leurs caractéristiques respectives."),
        tabsetPanel(
          type = "tabs",
          id = "typeCarac_panel",
          tabPanel(
            title = "Sélection manuelle",
            value = "unique",
            sliderTextInput(
              inputId = "typeCarac_carac",
              label = "",
              choices = carac_list,
              selected = carac_list[[1]],
              animate = TRUE,
              grid = TRUE
            ),
            p(style = "font-style: italic", "Options possibles :"),
            tags$ul(
              style = "font-style: italic",
              tags$li("Considérer les types secondaires."),
              tags$li("La taille de chaque diagramme varie en fonction",
                      "du nombre de Pokémons dans le type concerné.")
            )
          )
          # tabPanel(
          #   title = "Void",
          #   value = "void",
          #   p("Contenu du tabPanel Void.")
          # )
        )
      ),
      # ----- __main -----
      mainPanel(
        h3(
          textOutput(
            outputId = "typeCarac_info"
          )
        ),
        plotOutput(
          height = "600px",
          outputId = "typeCarac_plot"
        ),
        div(
          checkboxInput(
            inputId = "typeCarac_type2",
            label = "Considérer les types secondaires",
            value = FALSE
          ),
          style = "display: inline-block;"
        ),
        div(
          checkboxInput(
            inputId = "typeCarac_varwidth",
            label = "Taille en fonction du nombre d'entrées",
            value = TRUE
          ),
          style = "display: inline-block;"
        )
        # div(
        #   checkboxInput(
        #     inputId = "gg_fixed_layout",
        #     label = "Échelle fixe",
        #     value = TRUE
        #   ),
        #   style = "display: inline-block;"
        # )
      )
    )
  )
}

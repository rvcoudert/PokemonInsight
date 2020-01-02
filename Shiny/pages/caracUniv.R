page_caracUniv <- function()
{
  tabItem(
    tabName = "caracUniv",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel(
        titlePanel(
          h1("Étude de caractéristiques"),
        ),
        h3("Étude univariée (une dimension)"),
        p("Une seule caractéristique étudiée",
          "via des diagrammes ou un histogramme."),
        # pickerInput(
        #   inputId = "caracUniv_selection",
        #   label = "",
        #   choices = carac_list,
        #   selected = carac_list[[1]],
        #   options = list(
        #     `actions-box` = TRUE,
        #     size = 10,
        #     `selected-text-format` = "values"
        #   )
        # )
        sliderTextInput(
          inputId = "caracUniv_selection",
          label = "",
          choices = carac_list,
          selected = carac_list[[1]],
          animate = TRUE,
          grid = TRUE
        ),
        fluidRow(
          valueBoxOutput("caracUniv_min"),
          valueBoxOutput("caracUniv_mean"),
          valueBoxOutput("caracUniv_max")
        )

        # p("Sélectionner une ou plusieurs générations de Pokémons",
        #   "pour en savoir plus.")
      ),
      # ----- __main -----
      mainPanel(
        h3(
          textOutput(outputId = "caracUniv_info")
        ),
        tabsetPanel(
          tabPanel(
            title = "Diagrammes",
            plotOutput(
              height = "200px",
              outputId = "caracUniv_boxplot"
            ),
            plotOutput(
              height = "200px",
              outputId = "caracUniv_violin"
            ),
            div(
              checkboxInput(
                inputId = "caracUniv_points",
                label = "Afficher les points"
              ),
              style = "display: inline-block;"
            )
          ),
          tabPanel(
            title = "Histogramme",
            plotOutput(
              height = "600px",
              outputId = "caracUniv_hist"
            ),
            div(
              p("Largeur des bins :"),
              style = "display: inline-block;"
            ),
            div(
              sliderInput(
                inputId = "caracUniv_binwidth",
                label = "",
                min = 10,
                max = 50,
                value = 20,
                step = 5,
                round = 0
              ),
              style = "display: inline-block;"
            )
          )
        )
      )
    )
  )
}

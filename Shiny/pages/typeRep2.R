page_typeRep2 <- function()
{
  tabItem(
    tabName = "typeRep2",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel(
        titlePanel(
          h1("Étude croisée des types"),
        ),
        p("Répartition croisée entre les types primaires et secondaires",
          "des Pokémons."),
        p("Sélectionnez un type primaire pour voir les relations",
          "avec les types secondaires."),
        radioButtons(
          inputId = "typeRep2_typeOrder",
          label = "",
          choices = c("Type1", "Type2", "Les deux"),
          selected = "Les deux",
          inline = TRUE
        ),
        radioButtons(
          inputId = "typeRep2_selection_mode",
          label = "",
          choices = c("Unique", "Multiple"),
          selected = "Multiple",
          inline = TRUE
        ),
        uiOutput(outputId = "typeRep2_slider")
      ),
      # ----- __main -----
      mainPanel(
        h3(
          textOutput(
            outputId = "typeRep2_info"
          )
        ),
        plotOutput(
          height = "600px",
          outputId = "typeRep2_plot"
        ),
        checkboxInput(
          inputId = "typeRep2_edgetext",
          label = "Afficher les effectifs des arêtes",
          value = FALSE
        )

      )
    )
  )
}

page_caracBiv <- function()
{
  tabItem(
    tabName = "caracBiv",
    sidebarLayout(
      # ----- __sidebar -----
      sidebarPanel(
        titlePanel(
          h1("Étude de caractéristiques"),
        ),
        h3("Étude bivariée (deux dimensions)"),
        p("Deux caractéristiques étudiées l'une contre l'autre."),
        # pickerInput(
        #   inputId = "caracBiv_selection_1",
        #   label = "",
        #   choices = carac_list,
        #   selected = carac_list[[1]],
        #   options = list(
        #     `actions-box` = TRUE,
        #     size = 10,
        #     `selected-text-format` = "values"
        #   )
        # ),
        # pickerInput(
        #   inputId = "caracBiv_selection_2",
        #   label = "",
        #   choices = carac_list,
        #   selected = carac_list[[2]],
        #   options = list(
        #     `actions-box` = TRUE,
        #     size = 10,
        #     `selected-text-format` = "values"
        #   )
        # )
        sliderTextInput(
          inputId = "caracBiv_selection_1",
          label = "",
          choices = carac_list,
          selected = carac_list[[1]],
          animate = TRUE,
          grid = TRUE
        ),
        sliderTextInput(
          inputId = "caracBiv_selection_2",
          label = "",
          choices = carac_list,
          selected = carac_list[[3]],
          animate = TRUE,
          grid = TRUE
        )

        # p("Sélectionner une ou plusieurs générations de Pokémons",
        #   "pour en savoir plus.")
      ),
      # ----- __main -----
      mainPanel(
        h3(
          textOutput(outputId = "caracBiv_info")
        ),
        plotOutput(
          height = "600px",
          outputId = "caracBiv_plot"
        ),
        div(
          checkboxInput(
            inputId = "caracBiv_rug",
            label = "Afficher les rugs"
          ),
          style = "display: inline-block;"
        )
      )
    )
  )
}

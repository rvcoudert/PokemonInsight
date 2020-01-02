page_home <- function()
{
  tabItem(
    tabName = "home",
    titlePanel(
      h1("Bienvenue dans le fabuleux monde des Pokémons"),
    ),
    p("Cette application propose d'explorer le monde des Pokémons",
      "à travers les statistiques (et non les Gameboys) !"),
    h2("Mode d'emploi."),
    p("Plusieurs modes d'exploration sont disponibles via le menu ci-contre.")
  )
}


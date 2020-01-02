page_accessData <- function()
{
  tabItem(
    tabName = "accessData",
    h1("Accès aux données sur les Pokémons"),
    DT::dataTableOutput(outputId = "accessData_df")
  )
}

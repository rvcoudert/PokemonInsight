library(magrittr)
wd <- "/Users/felix/OneDrive/Documents/OFFICIEL/2019-2020 Barbarossa/Shiny/Pokemon/"
setwd(wd)


# ----- Step 1 -----


# Adding manually French translations.
pokemon_df <- data.table::fread(
  input = "data/Pokemon_rv.csv",
  encoding = "UTF-8"
)

# Changing column name from "#" to "id".
colnames(pokemon_df)[[1]] <- "id"

# Only keep one row per group.
pokemon_df_distinct <- pokemon_df %>%
  dplyr::distinct(id, .keep_all = TRUE)
pokemon_df_dropped <- pokemon_df[pokemon_df$id %>% duplicated, ]

write.table(
  x = pokemon_df_distinct,
  file = "data/Pokemon_rv_distinct.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  fileEncoding = "UTF-8"
)

write.table(
  x = pokemon_df_dropped,
  file = "data/Pokemon_rv_dropped.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  fileEncoding = "UTF-8"
)


# ----- Step 2 -----


# Check manually the name problems.
pokemon_df_distinct %>% 
  dplyr::filter(grepl(
    pattern = " ",
    x = NameFR,
    perl = TRUE
  ))


# ----- Step 3 -----


pokemon_df <- data.table::fread(
  input = "data/Pokemon_rv_cleaned.csv",
  encoding = "UTF-8"
)

translation_types <-  data.table::fread(
  input = "data/Translation_Types.csv",
  encoding = "UTF-8"
)

pokemon_df_final <- pokemon_df %>% 
  dplyr::left_join(
    translation_types,
    c("Type 1" = "TypeENG")
  ) %>% 
  dplyr::rename("Type 1 FR" = "TypeFR") %>% 
  dplyr::left_join(
    translation_types,
    c("Type 2" = "TypeENG")
  ) %>% 
  dplyr::rename("Type 2 FR" = "TypeFR")

pokemon_df_final <- pokemon_df_final %>%
  dplyr::select(1, 2, 3, 4, ncol(.) - 1, 5, ncol(.), dplyr::everything())

write.table(
  x = pokemon_df_final,
  file = "data/Pokemon_final.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  fileEncoding = "UTF-8"
)

write.table(
  x = pokemon_df_final %>%
    dplyr::select(-c(2, 4, 6)) %>% 
    dplyr::rename(
      "Name" = "NameFR",
      "Type1" = "Type 1 FR",
      "Type2" = "Type 2 FR"),
  file = "data/Pokemon_final_fr.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  fileEncoding = "UTF-8"
)


# ----- Type Colors -----


type_colors <- data.table::fread(
  input = "data/Type_Colors.csv",
  encoding = "UTF-8",
  header = FALSE
)

colnames(type_colors) <- c("Type", "Color")

type_colors$Type <- sub(
  pattern = "\\sType:$",
  replacement = "",
  x = type_colors$Type,
  perl = TRUE
)

translation_types <-  data.table::fread(
  input = "data/Translation_Types.csv",
  encoding = "UTF-8"
)

write.table(
  x = type_colors,
  file = "data/Type_Colors.csv",
  sep = ",",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

type_colors_FR <- dplyr::left_join(
  x = translation_types,
  y = type_colors,
  c("TypeENG" = "Type"),
) %>%
  dplyr::select(2, 3) %>% 
  dplyr::rename("Type" = "TypeFR")

type_colors_FR$Color <- sub(
  pattern = "^\\s+",
  replacement = "",
  x = type_colors_FR$Color)

type_colors$Color <- paste("#", type_colors$Color, sep = "")

write.table(
  x = type_colors_FR,
  file = "data/Type_Colors.csv",
  sep = ",",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)



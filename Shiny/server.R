# ----- preprocess -----


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

type_max_y_onegeneration <- pokemon_melted %>%
  dplyr::count(Generation, Type) %>%
  dplyr::select(n) %>%
  max()
type_max_y_fullset <- pokemon_melted$Type %>% table() %>% max()


function(input, output, session) {

  # ----- reac_expr -----

  # Ici, on centralise les appels à certains <input>
  # utilisés à plusieurs endroits.

  # Si jamais la sélection est vide, alors on force à l'ensemble complet.
  get_generation_selection <- reactive({
    generation_selection <- input$generation_selection
    if (is.null(generation_selection)) {
      generation_selection <- 1:nb_generations
    }
    return(generation_selection)
  })

  get_pokemon_df <- reactive({
    pokemon_df %>%
      dplyr::filter(Generation %in% get_generation_selection())
  })

  get_pokemon_melted <- reactive({
    pokemon_melted %>%
      dplyr::filter(Generation %in% get_generation_selection())
  })

  get_nb_pokemons <- reactive({
    get_pokemon_df() %>%
      dplyr::distinct(id) %>%
      nrow
  })

  get_typeOrder <- reactive({
    if (input$typeRep2_typeOrder == "Les deux") {
      typeOrder <- c("Type1", "Type2")
    } else {
      typeOrder <- input$typeRep2_typeOrder
    }

    return(typeOrder)
  })

  get_type_link_table <- reactive({
    type_selection <- input$typeRep2_type_selection
    typeOrder <- get_typeOrder()

    if (identical(typeOrder, c("Type1", "Type2"))) {
      type_link_table <- get_pokemon_df() %>%
        dplyr::filter(Type1 %in% type_selection | Type2 %in% type_selection) %>%
        dplyr::count(Type1, Type2, name = "Freq")
    } else {
      type_link_table <- get_pokemon_df() %>%
        dplyr::filter(get(typeOrder) %in% type_selection) %>%
        dplyr::count(Type1, Type2, name = "Freq")
    }
  })

  # ----- header -----

  output$nb_pokemons <- renderText({
    get_nb_pokemons()
  })

  # ----- typeRep -----

  # ----- __info -----
  output$typeRep_info <- renderText({
    generation_selection <- get_generation_selection()
    # if (input$generation_selection_panel == "Sélection multiple") {
    #   generation_selection <- input$generation_selection_1
    # }
    # if (input$generation_selection_panel == "Timeline par génération") {
    #   generation_selection <- input$generation_selection_2
    # }
    if (is.null(generation_selection)) {
      return(("Aucune génération sélectionnée."))
    }

    paste(
      "Type de ",
      get_nb_pokemons(),
      " Pokémons",
      " des générations : ",
      generation_selection %>% paste(collapse = ", "),
      ".",
      sep = ""
    )
  })

  # ----- __plot -----
  output$typeRep_plot <- renderPlot({
    generation_selection <- get_generation_selection()
    type_max_y <- type_max_y_fullset
    nudge_y_type2 <- 4
    # if (input$generation_selection_panel == "Sélection multiple") {
    # generation_selection <- input$generation_selection_1
    # type_max_y <- type_max_y_fullset
    # nudge_y_type2 <- 4
    # }
    # if (input$generation_selection_panel == "Timeline par génération") {
    #   generation_selection <- input$generation_selection_2
    #   type_max_y <- type_max_y_onegeneration
    #   nudge_y_type2 <- 1
    # }

    gg_type2 <- input$typeRep_gg_type2

    if (gg_type2 == TRUE) {
      type2_alpha <- 0.5
    } else {
      type2_alpha <- 0
    }

    typeRep_gg_fixed_layout <- input$typeRep_gg_fixed_layout

    if (typeRep_gg_fixed_layout == TRUE) {
      myLimits <- c(0, round(1.1 * type_max_y, 0))
    } else {
      myLimits <- NULL
    }

    if (is.null(generation_selection)) {
      return(plot.new())
    }

    pokemon_df_to_plot <- get_pokemon_df() %>%
      dplyr::filter(Generation %in% generation_selection)
    table_Type1 <- pokemon_df_to_plot %>%
      dplyr::count(Type1, name = "Freq") %>%
      dplyr::rename(Type = Type1) %>%
      cbind(TypeOrder = "Type1", .)

    # Si jamais un type est manquant, alors on l'ajoute
    #   pour afficher la colonne vide dans <ggplot2>.
    if (nrow(table_Type1) < nrow(type_colors)) {
      missed_types <- type_colors %>%
        dplyr::select(Type) %>%
        dplyr::filter(!Type %in% table_Type1$Type)
      table_Type1 <- rbind(
        table_Type1,
        cbind(
          TypeOrder = "Type1",
          missed_types,
          Freq = 0
        )
      )
    }

    table_Type2 <- pokemon_df_to_plot %>%
      dplyr::count(Type2, name = "Freq") %>%
      tidyr::drop_na(Type2) %>%
      dplyr::rename(Type = Type2) %>%
      cbind(TypeOrder = "Type2", .)
    table_Types <- rbind(
      table_Type2,
      table_Type1
    )

    type_hist <- table_Types %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(
        ggplot2::aes(
          x = Type,
          y = Freq,
          fill = Type,
          alpha = TypeOrder),
        stat = "identity"
      ) +
      ggplot2::scale_fill_manual(
        name = "Légende des couleurs",
        limits = type_colors$Type,
        values = type_colors$Color,
        breaks = c("Eau", "Feu"),
        labels = c("Feu type principal", "Feu type secondaire"),
        guide = ggplot2::guide_legend(
          override.aes = list(
            fill = c(
              "#EE8130",
              "#EE813080"
            )
          )
        )
      ) +
      ggplot2::scale_alpha_manual(
        limits = c("Type1", "Type2"),
        values = c(1, type2_alpha),
        guide = FALSE
      ) +
      ggplot2::scale_y_continuous(
        limits = myLimits,
        expand = c(0, 0, 0, 0),
        minor_breaks = 0:1000,
        breaks = 0:100 * 5) +
      ggplot2::theme_minimal(base_size = 20) +
      ggplot2::theme(
        plot.margin = ggplot2::unit(rep(0.3, 4), "cm"),
        plot.title = ggplot2::element_text(hjust = .5),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          face = "bold",
          angle = 60,
          hjust = 1,
          vjust = 1
        ),
        legend.position = "bottom"
      )

    if (input$typeRep_gg_numbers == TRUE) {
      table_Type1_label <- table_Type1 %>%
        dplyr::filter(Freq != 0) %>%
        dplyr::mutate(myLabel = Freq) %>%
        dplyr::mutate(Freq = Freq / 2)
      table_Type2_label <- table_Type2 %>%
        dplyr::rename(Freq_2 = Freq) %>%
        dplyr::left_join(
          table_Type1 %>%
            dplyr::select(Type, Freq) %>%
            dplyr::rename(Freq_1 = Freq),
          "Type") %>%
        dplyr::mutate(Freq = Freq_1 + Freq_2 + nudge_y_type2) %>%
        dplyr::mutate(myLabel = Freq_2) %>%
        dplyr::select(TypeOrder, Type, Freq, myLabel)
      if (input$typeRep_gg_type2 == TRUE) {
        table_Types_label <- rbind(
          table_Type1_label,
          table_Type2_label
        )
      } else {
        table_Types_label <- table_Type1_label
      }
      type_hist <- type_hist +
        ggplot2::geom_text(
          data = table_Types_label,
          mapping = ggplot2::aes(
            x = Type,
            y = Freq,
            label = myLabel,
            alpha = TypeOrder
          )
        )
    }
    return(type_hist)
  })

  # ----- typeRep2 -----

  # ----- __info -----

  typeOrder_labels <- c(
    "Type1" = "primaire",
    "Type2" = "secondaire"
  )

  output$typeRep2_info <- renderText({
    type_link_table <- get_type_link_table()

    if (nrow(type_link_table) == 0) {
      return(
        paste(
          "Aucun pokémon dans le type",
          paste(
            typeOrder_labels[get_typeOrder()],
            collapse = " et "
          ),
          "concerné."
        )
      )
    }

    paste(
      "Répartition des Pokémons du type ",
      paste(
        typeOrder_labels[get_typeOrder()],
        collapse = " et "
      ),
      " ",
      paste(input$typeRep2_type_selection, collapse = " "),
      ".",
      " (",
      type_link_table %>%
        dplyr::select(Freq) %>%
        sum(),
      " Pokémons)",
      sep = ""
    )
  })

  # ----- __slider -----


  output$typeRep2_slider <- renderUI({
    mode_selection <- input$typeRep2_selection_mode

    if (mode_selection == "Unique") {
      multiple_selection <- FALSE
    } else {
      multiple_selection <- TRUE
    }

    pickerInput(
      inputId = "typeRep2_type_selection",
      label = "",
      choices = type_colors$Type %>% sort(),
      selected = (type_colors$Type %>% sort())[[1]],
      multiple = multiple_selection,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "values"
      )
    )
  })

  # ----- __plot -----
  output$typeRep2_plot <- renderPlot({
    type_link_table <- get_type_link_table()

    # typeOrder <- "Type1"
    # type_selection <- "Feu"
    #
    # type_link_table <- pokemon_df %>%
    #   dplyr::filter(get(typeOrder) == type_selection) %>%
    #   dplyr::count(Type1, Type2, name = "Freq")

    if (nrow(type_link_table) == 0) {
      return(plot.new())
    }

    # Si le graphe n'a qu'une seule arrête,
    #   alors on en ajoute une artificellement
    #   pour éviter un bug de <ggnetwork>.
    if (nrow(type_link_table) == 1) {
      type_link_table <- rbind(
        type_link_table,
        data.frame(
          Type1 = type_link_table$Type1[[1]],
          Type2 = type_link_table$Type1[[1]],
          Freq = 0
        )
      )
    }

    myNetwork <- type_link_table %>%
      dplyr::mutate(Type2 = tidyr::replace_na(Type2, replace = "Aucun")) %>%
      network::network(
        matrix.type = "edgelist",
        ignore.eval = FALSE,
        loops = TRUE
      )

    if (type_link_table$Type1 %>% unique %>% length <= 1
        | type_link_table$Type2 %>% unique %>% length <= 1) {
      # Sélection unique et les arêtes sont laissées en noires.
      edgeAes <- ggplot2::aes(size = Freq)
      textSize <- 6
      nodeSize <- 40
      myArrowGap <- 0.08
      myCurvature <- 0
    } else {
      # Sélection multiple et les arêtes sont passées en couleur.
      edgeAes <- ggplot2::aes(size = Freq, color = vertex.names)
      textSize <- 4
      nodeSize <- 20
      myArrowGap <- 0.04
      myCurvature <- 0.05
    }

    set.seed(0)

    myggnetwork <- ggnetwork::ggnetwork(myNetwork, arrow.gap = myArrowGap)

    p <- myggnetwork %>%
      ggplot2::ggplot(
        data = .,
        mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend)
      ) + ggnetwork::geom_edges(
        mapping = edgeAes,
        arrow = ggplot2::arrow(
          length = ggplot2::unit(12, "pt")
        ),
        curvature = myCurvature
      ) + ggnetwork::geom_nodes(
        mapping = ggplot2::aes(color = vertex.names),
        size = nodeSize
      ) + ggnetwork::geom_nodetext(
        mapping = ggplot2::aes(
          label = vertex.names),
        fontface = "bold",
        size = textSize
      ) + ggplot2::scale_colour_manual(
        name = "Légende des couleurs",
        limits = type_colors$Type,
        values = type_colors$Color,
        na.value = "grey80",
        guide = FALSE
      ) + ggplot2::scale_size_continuous(
        range = c(1, 4),
        trans = "sqrt",
        guide = FALSE
      ) + ggnetwork::theme_blank(
        base_size = 16
      ) + ggplot2::scale_x_continuous(
        expand = ggplot2::expand_scale(mult = 0.1)
      ) + ggplot2::scale_y_continuous(
        expand = ggplot2::expand_scale(mult = 0.1)
      )

    if (input$typeRep2_edgetext) {
      p <- p + ggnetwork::geom_edgetext(
        mapping = ggplot2::aes(label = Freq),
        color = "white",
        fill = "black",
        size = textSize)
    }

    return(p)
  })

  # ----- typeCarac -----

  # ----- __info -----
  output$typeCarac_info <- renderText({
    # if (input$typeCarac_panel == "unique") {
    carac <- input$typeCarac_carac
    # }

    paste(
      "Une caractéristique sur chaque type : ",
      carac,
      ".",
      sep = ""
    )
  })

  # ----- __plot -----
  output$typeCarac_plot <- renderPlot({
    # if (input$typeCarac_panel == "unique") {
    carac <- input$typeCarac_carac
    # }

    carac_min <- get_pokemon_df()[[carac]] %>% min(na.rm = TRUE)
    carac_max <- get_pokemon_df()[[carac]] %>% max(na.rm = TRUE)

    if (input$typeCarac_type2) {
      df_to_plot <- get_pokemon_melted()
    } else {
      df_to_plot <- get_pokemon_melted() %>%
        dplyr::filter(TypeOrder == "Type1")
    }

    table_Types <- df_to_plot %>%
      dplyr::count(Type, name = "Freq")
    type_info <- dplyr::left_join(
      type_colors,
      table_Types,
      "Type"
    )

    ggplot2::ggplot(
      data = df_to_plot,
      mapping = ggplot2::aes(
        x = Type,
        y = get(carac),
        fill = Type
      )
    ) + ggplot2::geom_boxplot(
      varwidth = input$typeCarac_varwidth
    ) + ggplot2::scale_x_discrete(
      limits = type_info$Type,
      labels = paste(type_info$Type, "/", type_info$Freq)
    ) + ggplot2::scale_y_continuous(
      breaks = floor(carac_min/20):(ceiling(carac_max/20)) * 20,
      minor_breaks = floor(carac_min/10):(ceiling(carac_max/10)) * 10
    ) + ggplot2::coord_flip(
    ) + ggplot2::scale_fill_manual(
      name = "Légende des couleurs",
      limits = type_info$Type,
      values = type_info$Color,
      guide = FALSE
    ) + ggplot2::labs(
      x = "Type / Nombre d'entrées",
      y = carac
    ) + ggplot2::theme_minimal(
      base_size = 16
    ) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(color = "gray85"),
        panel.grid.minor.x = ggplot2::element_line(color = "gray85"),
        panel.grid.minor.y = ggplot2::element_blank()
      )
  })

  # ----- caracUniv -----

  # ----- __info -----
  output$caracUniv_info <- renderText({
    paste(
      "Une caractéristique sur ",
      get_nb_pokemons(),
      " Pokémons : ",
      input$caracUniv_selection,
      ".",
      sep = ""
    )
  })

  # ----- __values -----
  output$caracUniv_min <- renderValueBox({
    carac_data <- get_pokemon_df() %>%
      dplyr::select(input$caracUniv_selection) %>%
      unlist() %>% unname()
    valueBox(
      value = carac_data %>% min() %>% round(0),
      subtitle = "Minimum"
    ) %>% return()
  })
  output$caracUniv_mean <- renderValueBox({
    carac_data <- get_pokemon_df() %>%
      dplyr::select(input$caracUniv_selection) %>%
      unlist() %>% unname()
    valueBox(
      value = carac_data %>% mean() %>% round(0),
      subtitle = "Moyenne"
    ) %>% return()
  })
  output$caracUniv_max <- renderValueBox({
    carac_data <- get_pokemon_df() %>%
      dplyr::select(input$caracUniv_selection) %>%
      unlist() %>% unname()
    valueBox(
      value = carac_data %>% max() %>% round(0),
      subtitle = "Maximum"
    ) %>% return()
  })

  # ----- __boxplot -----
  output$caracUniv_boxplot <- renderPlot({
    carac_selection <- input$caracUniv_selection
    carac_min <- get_pokemon_df()[[carac_selection]] %>% min(na.rm = TRUE)
    carac_max <- get_pokemon_df()[[carac_selection]] %>% max(na.rm = TRUE)

    p <- ggplot2::ggplot(
      data = get_pokemon_df(),
      mapping = ggplot2::aes(
        x = 0,
        y = get(carac_selection))
    ) + ggplot2::geom_boxplot(
      fill = "#dd4b39",
      size = 1,
      outlier.color = "#dd4b39",
      outlier.fill = "#dd4b39"
    ) + ggplot2::coord_flip(
      xlim = c(-0.6, 0.6),
      ylim = c(carac_min, carac_max)
    ) + ggplot2::labs(
      y = carac_selection
    ) + ggplot2::scale_y_continuous(
      breaks = floor(carac_min/20):(ceiling(carac_max/20)) * 20,
      minor_breaks = floor(carac_min/10):(ceiling(carac_max/10)) * 10
    ) + ggplot2::theme_minimal(
      base_size = 16
    ) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(color = "gray85"),
        panel.grid.minor.x = ggplot2::element_line(color = "gray85"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )

    if (input$caracUniv_points) {
      p <- p + ggbeeswarm::geom_quasirandom(method = "tukeyDense")
    }

    return(p)
  })

  # ----- __violin -----
  output$caracUniv_violin <- renderPlot({
    carac_selection <- input$caracUniv_selection
    carac_min <- get_pokemon_df()[[carac_selection]] %>% min(na.rm = TRUE)
    carac_max <- get_pokemon_df()[[carac_selection]] %>% max(na.rm = TRUE)

    p <- ggplot2::ggplot(
      data = get_pokemon_df(),
      mapping = ggplot2::aes(
        x = 0,
        y = get(carac_selection))
    ) + ggplot2::geom_violin(
      fill = "#dd4b39",
      size = 1
    ) + ggplot2::coord_flip(
      xlim = c(-0.6, 0.6),
      ylim = c(carac_min, carac_max)
    ) + ggplot2::labs(
      y = carac_selection
    ) + ggplot2::scale_y_continuous(
      breaks = floor(carac_min/20):(ceiling(carac_max/20)) * 20,
      minor_breaks = floor(carac_min/10):(ceiling(carac_max/10)) * 10
    ) + ggplot2::theme_minimal(
      base_size = 16
    ) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(color = "gray85"),
        panel.grid.minor.x = ggplot2::element_line(color = "gray85"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )

    if (input$caracUniv_points) {
      p <- p + ggbeeswarm::geom_quasirandom(method = "tukeyDense")
    }

    return(p)
  })

  # ----- __hist -----
  output$caracUniv_hist <- renderPlot({
    carac_selection <- input$caracUniv_selection
    myBinwidth <- input$caracUniv_binwidth

    ggplot2::ggplot(
      data = get_pokemon_df(),
      mapping = ggplot2::aes(
        x = get(carac_selection))) +
      ggplot2::geom_histogram(
        binwidth = myBinwidth,
        fill = "#dd4b39"
      ) +
      ggplot2::labs(
        x = carac_selection
      ) +
      ggplot2::theme_minimal(base_size = 16) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank()
      )
  })

  # ----- caracBiv -----

  # ----- __info -----
  output$caracBiv_info <- renderText({
    paste(
      "Deux caractéristiques sur ",
      get_nb_pokemons(),
      " Pokémons : ",
      input$caracBiv_selection_1,
      " VS ",
      input$caracBiv_selection_2,
      ".",
      sep = ""
    )
  })

  # ----- __plot -----
  output$caracBiv_plot <- renderPlot({
    carac_selection_1 <- input$caracBiv_selection_1
    carac_selection_2 <- input$caracBiv_selection_2

    min_1 <- get_pokemon_df()[[carac_selection_1]] %>% min(na.rm = TRUE)
    max_1 <- get_pokemon_df()[[carac_selection_1]] %>% max(na.rm = TRUE)
    min_2 <- get_pokemon_df()[[carac_selection_2]] %>% min(na.rm = TRUE)
    max_2 <- get_pokemon_df()[[carac_selection_2]] %>% max(na.rm = TRUE)

    p <- ggplot2::ggplot(
      data = get_pokemon_df() %>%
        dplyr::arrange(get(carac_selection_1)),
      mapping = ggplot2::aes(
        x = get(carac_selection_1),
        y = get(carac_selection_2)
      )
    ) + ggplot2::labs(
      x = carac_selection_1,
      y = carac_selection_2
    ) + ggplot2::theme_light(
      base_size = 16
    ) + ggplot2::scale_x_continuous(
      expand = ggplot2::expand_scale(0.05)
    ) + ggplot2::scale_y_continuous(
      expand = ggplot2::expand_scale(0.05)
    ) + ggplot2::coord_cartesian(
      xlim = c(min_1, max_1),
      ylim = c(min_2, max_2)
    )

    if (input$caracBiv_rug) {
      p <- p + ggplot2::geom_rug(
        mapping = ggplot2::aes(
          color = get(carac_selection_1)
        ),
        size = 2,
        position = ggplot2::position_jitter(
          width = 10
        )
      ) + ggplot2::scale_color_gradient2(
        low = "darkred",
        mid = "gold2",
        high = "forestgreen",
        limits = c(min_1, max_1),
        midpoint = (min_1 + max_1) / 2,
        guide = FALSE
      )

      p <- p + ggplot2::geom_jitter(
        mapping = ggplot2::aes(color = get(carac_selection_1)),
        size = 2,
        width = 1,
        height = 1
      )
    } else {
      p <- p + ggplot2::geom_jitter(
        size = 2,
        width = 1,
        height = 1
      )
    }

    return(p)
  })

  # ----- accessData -----

  # ----- __dataTable -----

  output$accessData_df <- DT::renderDataTable(
    expr = return(get_pokemon_df()),
    filter = "top"
  )
}

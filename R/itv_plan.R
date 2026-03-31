# Intraspecific Trait Variation (ITV) Analysis Plan

itv_plan <- list(
  # 1. Fit variance partitioning models
  tar_target(
    name = itv_models,
    command = {
      traits |> 
        fancy_trait_name_dictionary() |>
        filter(!is.na(value_trans)) |> 
        group_by(class, figure_names, trait_trans) |> 
        nest() |> 
        mutate(
          model = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(value_trans ~ 1 + (1|region/site) + (1|taxon), data = .x)
            result$result
          })
        )
    }
  ),

  # 2. Extract variance components
  tar_target(
    name = trait_variance_partitioning,
    command = {
      itv_models |> 
        mutate(
          variance_summary = purrr::map(model, ~ {
            if (is.null(.x)) return(NULL)
            
            vc <- as.data.frame(VarCorr(.x))
            total <- sum(vc$vcov)
            
            vc |> 
              mutate(
                total_variance = total,
                percent_explained = (vcov / total) * 100
              ) |> 
              dplyr::select(grp, vcov, percent_explained)
          })
        ) |> 
        dplyr::select(class, figure_names, trait_trans, variance_summary) |> 
        unnest(variance_summary)
    }
  ),

  # 3. Calculate model assumptions checks
  tar_target(
    name = itv_model_checks,
    command = {
      itv_models |>
        rowwise() |>
        mutate(
          model_check = list(performance::check_model(model))
        ) |>
        ungroup() |>
        filter(!is.null(model_check))
    }
  ),
  
  # 4. Create variance partitioning bar plot
  tar_target(
    name = itv_variance_plot,
    command = {
      trait_variance_partitioning |>
        mutate(
          grp = case_when(
            grp == "region" ~ "Region",
            grp == "site:region" ~ "Site",
            grp == "taxon" ~ "Between species",
            grp == "Residual" ~ "Within species",
            TRUE ~ grp
          ),
          grp = factor(grp, levels = c("Within species", "Between species", "Site", "Region")) # reversed so Region is at top/bottom depending on stacking
        ) |>
        ggplot(aes(x = figure_names, y = percent_explained, fill = grp)) +
        geom_col() +
        facet_grid(~class, scales = "free_x", space = "free_x") +
        scale_fill_manual(values = MetBrewer::met.brewer("Ingres", 4)) +
        scale_x_discrete(labels = scales::label_parse()) +
        theme_minimal() +
        labs(
          title = "Intraspecific Trait Variance Partitioning",
          y = "Variance Explained (%)",
          x = "Trait",
          fill = "Variance Component"
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
    }
  ),
  
  # 5. Render HTML report of the model checks
  tarchetypes::tar_quarto(
    name = itv_model_checks_report,
    path = "itv_model_checks.qmd"
  )
)

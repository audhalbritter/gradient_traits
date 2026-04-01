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
            result <- safelmer(value_trans ~ 1 + (1 | region / site) + (1 | taxon), data = .x)
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
            if (is.null(.x)) {
              return(NULL)
            }

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
        theme_bw() +
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

  # 5. Leps 2011 ITV Partitioning (ANOVA)
  tar_target(
    name = leps_itv_output,
    command = {
      trait_mean |>
        filter(!is.na(mean) & !is.na(mean_noitv)) |>
        mutate(diff = mean - mean_noitv) |>
        pivot_longer(cols = c(mean, mean_noitv, diff), names_to = "mean_type", values_to = "value") |>
        group_by(region, class, figure_names, trait_trans, mean_type) |>
        nest() |>
        mutate(estimate = purrr::map(data, ~ {
          mod <- aov(value ~ 1, data = .x)
          broom::tidy(mod)
        })) |>
        select(-data) |>
        unnest(estimate)
    }
  ),

  # 6. Leps Variance Proportions
  tar_target(
    name = leps_variance_partition,
    command = {
      leps_itv_output |>
        select(region, class, figure_names, trait_trans, mean_type, term, sumsq) |>
        pivot_wider(names_from = mean_type, values_from = sumsq) |>
        rename("total_ss" = mean, "turnover_ss" = mean_noitv, "intraspecific_ss" = diff) |>
        mutate(
          covariation_ss = total_ss - turnover_ss - intraspecific_ss,
          total_p = total_ss / total_ss,
          turnover_p = turnover_ss / total_ss,
          intraspecific_p = intraspecific_ss / total_ss,
          covariation_p = covariation_ss / total_ss
        ) |>
        pivot_longer(cols = c(total_ss:covariation_p), names_to = c("process", "variable"), names_sep = "_", values_to = "value") |>
        mutate(variable = recode(variable, "ss" = "sumsq", "p" = "proportion")) |>
        pivot_wider(names_from = variable, values_from = value)
    }
  ),

  # 7. Leps Plot
  tar_target(
    name = leps_itv_plot,
    command = {
      leps_variance_partition |>
        filter(process %in% c("turnover", "intraspecific")) |>
        group_by(region, trait_trans) |>
        mutate(
          sum_val = sum(proportion),
          proportion_standardized = proportion / sum_val
        ) |>
        ungroup() |>
        mutate(
          process = recode(process, intraspecific = "ITV"),
          process = factor(process, levels = c("ITV", "turnover"))
        ) |>
        mutate(proportion_standardized = if_else(process == "turnover", -1 * proportion_standardized, proportion_standardized)) |>
        ggplot(aes(x = figure_names, y = proportion_standardized, fill = process)) +
        geom_col() +
        geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
        coord_flip() +
        scale_fill_manual(name = "Process", values = c("ITV" = "#005BBB", "turnover" = "#FFD500")) +
        scale_x_discrete(limits = rev, labels = scales::label_parse()) +
        lims(y = c(-1, 1)) +
        labs(x = "", y = "Relative contribution to variance") +
        facet_grid(class ~ region, scales = "free", space = "free_y") +
        theme_bw() +
        theme(
          text = element_text(size = 14),
          legend.position = "top",
          strip.text.y = element_blank(),
          panel.spacing = unit(0.3, "cm")
        )
    }
  ),

  # 8. Render HTML report of the model checks
  tarchetypes::tar_quarto(
    name = itv_model_checks_report,
    path = "itv_model_checks.qmd"
  )
)

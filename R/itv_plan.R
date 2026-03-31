# Intraspecific Trait Variation (ITV) Analysis Plan

itv_plan <- list(
  tar_target(
    name = trait_variance_partitioning,
    command = {
      traits |> 
        filter(!is.na(value_trans)) |> 
        group_by(trait_category, trait_trans) |> 
        nest() |> 
        mutate(
          model = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(value_trans ~ 1 + (1|region/site) + (1|taxon), data = .x)
            result$result
          }),
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
        dplyr::select(trait_category, trait_trans, variance_summary) |> 
        unnest(variance_summary)
    }
  )
)

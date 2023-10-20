# Data analysis

analysis_plan <- list(
  
  # diversity across latitude
  tar_target(
    name = diversity_lat_model,
    command = {

      safelm = safely(.f = lm)

      diversity |>
        group_by(diversity_index) |>
        nest() |>
        mutate(model = map(.x = data, .f= ~ safelm(value ~ annual_temperature, data = .)$result),
               result = map(model, tidy),
               # make new data for prediction
               # newdata = map(.x = data, .f = ~. |>
               #                        mutate(min)
               prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
               output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y)))
    }
  ),
  
  # diversity within country model
  tar_target(
    name = diversity_model,
    command = {
      
      safelm = safely(.f = lm)
      
      diversity |> 
        group_by(country, ecosystem, gradient, diversity_index) |> 
        nest() |> 
        mutate(model = map(.x = data, .f= ~ safelm(value ~ annual_temperature, data = .)$result),
               result = map(model, tidy),
               prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
               output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y)))
    }
  ),
  
  # testing across latitude
  tar_target(
    name = latitude_model,
    command = {
      
      safelm = safely(.f = lm)
      
      trait_mean |> 
        group_by(trait_trans, trait_fancy) |> 
        nest() |> 
        mutate(model = map(.x = data, .f= ~ safelm(mean ~ annual_temperature, data = .)$result),
               result = map(model, tidy),
               # make new data for prediction
               # newdata = map(.x = data, .f = ~. |>
               #                        mutate(min)
               prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
               output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y)))
    }
  ),
  
  # testing within country
  tar_target(
    name = trait_mean_model,
    command = {
      
      safelm = safely(.f = lm)
      
      trait_mean |> 
        group_by(country, ecosystem, gradient, trait_trans, trait_fancy) |> 
        nest() |> 
        mutate(model = map(.x = data, .f= ~ safelm(mean ~ annual_temperature, data = .)$result),
               result = map(model, tidy),
               # make new data for prediction
               # newdata = map(.x = data, .f = ~. |>
               #                        mutate(min)
               prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
               output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y)))
    }
  ),
  
  
  # ordination
  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean)
    )
  
)

  
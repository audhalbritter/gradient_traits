# Output plan for QMD files

output_plan <- list(

  # Model assumption checks
  tar_quarto(
    name = model_checks_report,
    path = "model_output.qmd"
  )

)

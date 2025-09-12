#rename traits to fancy names for figures
#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){

  dat <- dat |> 
    # make fancy names for figures
    mutate(trait_fancy = recode(trait_trans,
                              c_percent = "C %",
                              cn_ratio = "CN",
                              dc13_permil = "δC13 ‰",
                              dn15_permil = "δN15 ‰",
                              plant_height_cm_log = "Log(Height) cm",
                              dry_mass_g_log = "Log(Dry mass) g",
                              ldmc = "LDMC",
                              leaf_area_cm2_log = "Log(Area) cm2",
                              n_percent = "N %",
                              np_ratio = "NP",
                              p_percent = "P %",
                              sla_cm2_g = "SLA cm2/g",
                              thickness_mm_log = "Log(Thickness) mm")) |> 
    mutate(figure_names = case_match(trait_trans,
                                     "plant_height_cm_log" ~ "Size~-~Height~cm",
                                     "dry_mass_g_log" ~ "Size~-~Dry~mass~g",
                                     "leaf_area_cm2_log" ~ "Size~-~Area~cm^2",
                                     "thickness_mm_log" ~ "Size~-~Thickness~mm",
                                     "ldmc" ~ "LES~-~LDMC",
                                     "sla_cm2_g" ~ "LES~-~SLA~cm^2*g^{-1}",
                                     "c_percent" ~ "LES~-~C~'%'",
                                     "n_percent" ~ "LES~-~N~'%'",
                                     "cn_ratio" ~ "LES~-~CN",
                                     "p_percent" ~ "LES~-~P~'%'",
                                     "np_ratio" ~ "LES~-~NP",
                                     "dc13_permil" ~ "I~-~δ^{13}~C~'‰'",
                                     "dn15_permil" ~ "I~-~δ^{15}~N~'‰'")) |>

    # add class
    mutate(class = case_when(trait_trans %in% c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log") ~ "Size",
                             trait_trans %in% c("dc13_permil", "dn15_permil") ~ "Isotopes",
                             TRUE ~ "Leaf economics"),
           class = factor(class, levels = c("Size", "Leaf economics", "Isotopes")),
           figure_names = factor(figure_names, levels = c("Size~-~Height~cm",
                                                          "Size~-~Dry~mass~g",
                                                          "Size~-~Area~cm^2",
                                                          "Size~-~Thickness~mm",
                                                          "LES~-~LDMC",
                                                          "LES~-~SLA~cm^2*g^{-1}",
                                                          "LES~-~C~'%'",
                                                          "LES~-~N~'%'",
                                                          "LES~-~CN",
                                                          "LES~-~P~'%'",
                                                          "LES~-~NP",
                                                          "I~-~δ^{13}~C~'‰'",
                                                          "I~-~δ^{15}~N~'‰'")))

  return(dat)
}


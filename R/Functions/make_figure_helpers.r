## Shared figure helpers

# Function to create consistent color mapping for regions sorted by latitude (north to south)
create_region_color_mapping <- function() {
  regions_ordered <- c(
    "Svalbard", "Southern Scandes", "Rocky Mountains",
    "Eastern Himalaya", "Central Andes", "Drakensberg"
  )

  colors <- rev(met.brewer("Archambault", n = length(regions_ordered)))
  names(colors) <- regions_ordered
  colors
}

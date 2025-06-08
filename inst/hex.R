library(hexSticker)
library(ggplot2)
# library(fontawesome) # No longer needed for the icon with emoji approach

# Define colors for the logo
bg_color <- "#2c3e50" # Dark blue-grey for a secure feel
icon_color <- "#f39c12" # A contrasting gold/orange for visibility
text_color <- "#ecf0f1" # Off-white for the text

# Create a ggplot object for the icon using a Unicode emoji
icon_plot <- ggplot() +
  geom_text(aes(x = 0.5, y = 0.5, label = "\U0001F512"), # Lock emoji Unicode
            size = 20, # Adjusted size for emoji
            color = icon_color) + # Color of the lock icon
  theme_void() # Remove all ggplot elements (axes, background)

# Generate the hexagon sticker
sticker(
  subplot = icon_plot,        # Use the ggplot object for the icon
  s_x = 1, s_y = 0.9, s_width = 0.6, s_height = 0.6, # Position and size of the icon
  package = "googlesecretmanager",  # The package name
  p_size = 8,                 # Font size of the package name
  p_x = 1, p_y = 0.4,         # Position of the package name
  p_color = text_color,       # Color of the package name text
  h_fill = bg_color,          # Hexagon fill color
  h_color = "#34495e",        # Hexagon border color (slightly darker than fill)
  url = "gargle.r-lib.org",   # Optional URL for the sticker
  u_color = "#bdc3c7",        # URL text color
  u_size = 1.5,               # URL font size
  dpi = 300,                  # Dots per inch for the output image
  filename = "logo.png" # Output file name
)

cat("Hexagon logo 'logo.png' has been generated in your working directory.\n")


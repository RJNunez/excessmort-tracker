theme_slate = function(base_size = 12, base_family = "") {

  theme_bw(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line         = element_blank(),
      axis.text.x       = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.text.y       = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),
      axis.ticks        = element_line(color = "#272B30", size  =  0.2),
      axis.title.x      = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),
      axis.title.y      = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),

      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#1C1E22"),
      legend.key        = element_rect(color = "#1C1E22",  fill = "#1C1E22"),
      legend.key.size   = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(color = "white"),
      legend.title      = element_text(color = "white"),
      legend.position   = "right",
      legend.text.align = NULL,
      legend.title.align= NULL,
      legend.box        = NULL,

      # Specify panel options
      panel.background = element_rect(fill = "#1C1E22", color  =  NA),
      # panel.border     = element_rect(fill = NA, color = "#1C1E22"),
      panel.border     = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "#272B30"),
      # panel.grid.minor = element_line(color = "#525252"),
      panel.grid.minor = element_blank(),
      panel.margin     = unit(0.5, "lines"),

      # Specify facetting options
      strip.background = element_rect(fill = "#272B30", color = "#1C1E22"),
      strip.text.x     = element_text(size = base_size*0.8, color = "white"),
      strip.text.y     = element_text(size = base_size*0.8, color = "white"),

      # Specify plot options
      plot.background = element_rect(color = "black", fill = "#1C1E22"),
      plot.title      = element_text(size = base_size*1.2, color = "white")
    )
}
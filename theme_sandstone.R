theme_sandstone = function(base_size = 12, base_family = "") {
  
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line         = element_blank(),
      axis.text.x       = element_text(size = base_size, color = "#525252", lineheight = 0.9),
      axis.text.y       = element_text(size = base_size, color = "#525252", lineheight = 0.9),
      axis.ticks        = element_line(color = "#e0cfb6", size  =  0.2),
      axis.title.x      = element_text(face="bold", size = base_size, color = "#525252", margin = margin(0, 10, 0, 0)),
      axis.title.y      = element_text(face="bold", size = base_size, color = "#525252", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#e6dccd"),
      legend.key        = element_rect(color = "#e6dccd",  fill = "#e6dccd"),#"#F8F5F0"
      legend.key.size   = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(color = "#525252"),
      legend.title      = element_text(color = "#525252"),
      legend.position   = "right",
      legend.text.align = NULL,
      legend.title.align= NULL,
      legend.box        = NULL,
      
      # Specify panel options
      panel.background = element_rect(fill = "#e6dccd", color = NA),
      # panel.border     = element_rect(fill = NA, color = "#3E3F3A", size=1),
      panel.border     = element_rect(fill = NA, color = NA, size=1),
      panel.grid.major = element_line(color = "#e0cfb6"),
      # panel.grid.minor = element_line(color = "#525252"),
      panel.grid.minor = element_blank(),
      panel.margin     = unit(0.5, "lines"),
      
      # Specify facetting options
      strip.background = element_rect(fill = "#e0cfb6", color = "#e6dccd"),
      strip.text.x     = element_text(size = base_size, face = "bold", color = "#525252"),
      strip.text.y     = element_text(size = base_size, face = "bold", color = "#525252"),
      
      # Specify plot options
      plot.background = element_rect(color = "#525252", fill = "#e6dccd"),
      plot.title      = element_text(size = base_size*1.2, color = "#525252")
    )
}
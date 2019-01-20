# ggplot2 theme to use later

theme_chris <- function (base_size = 12, base_family = "serif", ticks = TRUE) 
{
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(legend.background = element_blank(), legend.key = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          panel.background = element_rect(fill = "#94B1C533", colour = NA),
          plot.background = element_rect(fill = "#ffffff"),
          axis.line = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_text(colour = "#2a3132"),
          axis.title.x = element_text(colour = "#2a3132"),
          axis.title.y = element_text(colour="#2a3132"),
          axis.text.y = element_text(colour="#2a3132"),
          axis.title = element_text(colour = "#2a3132"),
          plot.title = element_text(colour = "#2a3132", 
                                    margin = margin(0,0,10,0)),
          plot.subtitle = element_text(colour = "#2a3132"),
          plot.caption = element_text(colour = "#2a3132"),
          legend.title = element_text(colour = "#2a3132"),
          legend.text = element_text(colour = "#2a3132"))
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}

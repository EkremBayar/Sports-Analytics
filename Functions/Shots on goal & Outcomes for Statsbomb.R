# Shots on goal & Outcomes for Statsbomb ----------------------------------


# Package
library(ggplot2)


# Post Function -----------------------------------------------------------


# Green Background:      "seagreen"
# Dark BackGround:       "#202020"
# Dark Blue Background:  "#224C56"
# Light Blue Background: "steelblue"


post <- function(fill_background = "white"){
  
  
  
  ggplot()+
    # Ground
    geom_line(aes(x = c(32, 48), y = c(0,0)))+
    # Post
    geom_rect(aes(xmin = 35.9, xmax = 44.1, ymin = 0, ymax = 2.75), fill = "#D3D3D3", color = "black")+
    geom_rect(aes(xmin = 36, xmax = 44, ymin = 0, ymax = 2.67), fill = fill_background, 
              color = "black", alpha = 0.7)+
    # Lines
    geom_line(aes(x = c(36, 36.3), y = c(2.67,2.58)), color = "gray")+ # Left Lines
    geom_line(aes(x = c(36.3, 36.7), y = c(2.58, 0.8)), color = "gray")+
    geom_line(aes(x = c(36.7, 36), y = c(0.8, 0)), color = "gray")+
    geom_line(aes(x = c(44, 43.7), y = c(2.67, 2.58)), color = "gray")+ # Right Lines
    geom_line(aes(x = c(43.7, 43.3), y = c(2.58, 0.8)), color = "gray")+
    geom_line(aes(x = c(43.3, 44), y = c(0.8, 0)), color = "gray")+
    geom_line(aes(x = c(36.7, 43.3), y = c(0.8,0.8)), color = "gray")+ # Ground Line
    # Theme
    theme(
      panel.background = element_rect(fill = fill_background),
      plot.background = element_rect(fill = fill_background),
      legend.background = element_rect(fill = fill_background),
      legend.key = element_rect(fill = fill_background,colour = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
      
    )
  
}



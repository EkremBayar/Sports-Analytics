pitch <- function(goaltype = c("box", "line", "barca_numbers"), pitch_theme = c("green","blue","night")){
  
  # Argüman Kontrolleri 
  if(is.null(goaltype) | missing("goaltype")){goaltype <- "box"}
  if(is.null(pitch_theme) | missing("pitch_theme")){pitch_theme <- "green"}
  
  # Saha Rengi
  if(pitch_theme == "green"){
    # Green
    background_color = "#77BD77"
    grass_color = "#77BD77"
    # Dark Green
    #background_color = "#538032"
    #grass_color = "#538032"
    line_color =  "#ffffff"
    goal_color = "#000000"
    
  }else if(pitch_theme == "blue"){
    
    # Blue
    grass_color = "#224C56" 
    line_color =  "#B3CED9" 
    background_color = "#224C56" 
    goal_color = "#15393D"
    
  }else if(pitch_theme == "night"){
    
    # Night
    grass_color = "#202020"
    line_color =  "#797876"
    background_color = "#202020"
    goal_color = "#131313"
  }else{
    return(NULL)
  }
  
  # Tüm Parametreler
  # Sahanın Boyutu
  ymin <- 0    # Minimum En
  ymax <- 80   # Maksimum En
  xmin <- 0    # Minimum Uzunluk
  xmax <- 120  # Maksimum Uzunluk
  
  # Saha Dışındaki Alan
  padding <- 5 # Default: 5
  
  # Ceza Sahasının Çizilmesi
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  
  # Yarı Saha Çizgisi
  halfwayline <- 60
  
  # Ceza Sahası İçerisinde Kale Önü Dikdörtgeni
  sixYardDef <- 6
  sixYardOff <- 114
  sixYardLeft <- 30
  sixYardRight <- 50
  
  # Yarı Saha Çemberi Parametreleri
  CentreSpot <- 40
  centreCirle_d <- 20
  
  # Yarı Saha Çember Fonksiyonu
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  # Yarı Saha Çemberinin Oluşturulması
  center_circle <- circleFun(c(halfwayline,CentreSpot),centreCirle_d,npoints = 100)
  
  # Penaltı Noktası
  penSpotDef <- 12
  penSpotOff <- 108
  
  # Tema Argümanı size: 12
  size <- 12
  
  # Kale-Gol Çizgisi
  goalPostLeft <- 36
  goalPostRight <- 44
  
  
  # SAHANIN ÇİZDİRİLMESİ
  
  plot <- ggplot()+
    
    # 1. Futbol Sahasının Sınırları
    xlim(c(xmin-padding,xmax+padding)) + ylim(c(ymin-padding,ymax+padding))+
    
    # 2. Temanın Ayarlanması
    theme(
      # 2.1. Eksenlerdeki Yazıların ve Çizgilerin Kaldırılması 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      
      # 2.2. Lejant Ayarlamaları
      legend.background = element_rect(fill = background_color, colour = NA), # Lejant Arka Planı (Saha Rengi ile Aynı Olmalı!)
      legend.key = element_rect(fill = background_color), # Lejant Arka Planı (Saha Rengi ile Aynı Olmalı!)
      legend.key.size = unit(1.2,"lines"), # Lejant Kutusunun Boyutu
      legend.text = element_text(size = size), # Lejant Yazısının Boyutu
      legend.title=element_text(size=size, face="bold",hjust=0), # Lejant Başlık Yazısının boyutu
      
      # 2.3. Facet Ayarlamaları - Arka Plan ve Yazıların Düzenlenmesi
      strip.background = element_rect(colour = background_color, fill = background_color, size = .5),
      strip.text.y=element_text(colour=background_color,size = size, angle=270),
      strip.text.x=element_text(size=size*1),
      
      # 2.4. Panel Ayarlamaları
      panel.background=element_rect(fill=background_color,colour=background_color), # Arka Plan 
      panel.grid = element_blank(), # Koordinat Çizgilerinin Silinmesi
      panel.spacing = element_blank(), # Facet İçin Birim Ayarlaması
      
      # 2.5. Plot Ayarlamaları
      plot.background=element_rect(background_color), # Tüm Görselin Arka Planı | element rect ile arkaplanı renk verilebilir
      plot.margin=unit(c(0, 0, 0, 0), "lines"), # Marjin Ayarlaması
      plot.title=element_text(size=size*1.2) # Plot Başlık Yazısının Boyutu
    )+
    
    # 3. Saha Çizgisi: Dikdörtgenin çizilmesi 
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = NA, colour = line_color)+
    
    # 4. Ceza Sahası: Dikdörtgenin Çizilmesi
    geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_color, colour = line_color)+ # Sol Ceza Sahası
    geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_color, colour = line_color)+ # Sağ Ceza Sahası
    
    # 5. Yarı Saha Çizgisi
    geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_color)+
    
    # 6. Ceza Sahası İçerisinde Kale Önü Dikdörtgeni
    geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_color, colour = line_color)+ # Sol
    geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_color, colour = line_color)+ # Sağ
    
    # 7. Yarı Saha Çemberi
    geom_path(data=center_circle, aes(x=x,y=y), colour = line_color)+
    
    # 8. Penaltı Noktalarının Çizilmesi
    geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_color, size = 0.9)+ # Sol
    geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_color, size = 0.9)+ # Sağ
    
    # 9. Başlangıç Noktasının Çizilmesi
    geom_point(aes(x = halfwayline , y = CentreSpot), colour = line_color)+
    
    # Ceza Yayının Çizilmesi
    
    annotate("path",
             x = 12 + 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
             y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
             col = line_color) +
    annotate("path",
             x = (120-12) - 10 * cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
             y = 40 + 10 * sin(seq(-0.3*pi, 0.3*pi, length.out = 30)),
             col = line_color)
  
  # 10. Kalenin Çizilmesi: Üç çeşit şekilde kale çeşitliliği yapılabilir.
  if(goaltype == "box"){
    
    plot <- plot+
      geom_rect(aes(xmin = xmin - 2 , ymin = goalPostLeft, xmax = xmin, ymax = goalPostRight), fill = grass_color, colour = line_color)+
      geom_rect(aes(xmin = xmax, ymin = goalPostLeft, xmax = xmax + 2, ymax = goalPostRight), fill = grass_color, colour = line_color)
    
  }else if(goaltype == "line"){
    
    plot <- plot+
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_color, size = 1) + # Sol
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_color, size = 1) # Sağ
    
  }else if(goaltype == "barca_numbers"){
    
    plot <- plot+
      geom_segment(aes(x = xmin - 0.75, y = goalPostLeft, xend = xmin - 0.75, yend = goalPostRight),colour = line_color, size = 0.75)+
      geom_segment(aes(x = xmax + 0.75, y = goalPostLeft, xend = xmax + 0.75, yend = goalPostRight),colour = line_color, size = 0.75)
    
  }else{
    return(NULL)
  }
  
  
  # 11. Korner Çizgileri
  TopLeftCorner <- circleFun(c(xmin,ymax),2.27,npoints = 50)
  TopRightCorner <- circleFun(c(xmax,ymax),2.27,npoints = 50)
  BottomLeftCorner <- circleFun(c(xmin,ymin),2.27,npoints = 50)
  BottomRightCorner <- circleFun(c(xmax,ymin),2.27,npoints = 50)
  
  plot <- plot+
    geom_path(data=TopLeftCorner %>% filter(x > 0, y < 80) , aes(x=x,y=y), colour = line_color)+
    geom_path(data=BottomLeftCorner %>% filter(x > 0, y > 0) , aes(x=x,y=y), colour = line_color)+
    geom_path(data=TopRightCorner %>% filter(x < 120, y < 80) , aes(x=x,y=y), colour = line_color) +
    geom_path(data=BottomRightCorner %>% filter(x < 120, y > 0) , aes(x=x,y=y), colour = line_color)
  
  
  
  
  return(plot)
}

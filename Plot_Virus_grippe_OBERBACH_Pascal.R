# Charger les données
proxy <- read.csv("ILILAB.csv")

###########################  PREPROCESSING  ####################################

# Ajouter une colonne GPrate
proxy$GPrate <- proxy$GPratePer1000/1000

# Ajouter une colonne H3N2.prox
proxy$H3N2.prox <- proxy$A.H3N2/proxy$Specimens.tested * proxy$GPrate

# Ajouter une colonne index
proxy$index <- 1:nrow(proxy)

# Filtrer les lignes de 20 à 313
proxy <- proxy[20:313,]

# Remplacer les valeurs de H3N2.prox où l'index est supérieur ou égal à 310 par 0
proxy$H3N2.prox[proxy$index >= 310] <- 0

#########################       PLOT       #####################################

par(mar=c(2,4,0,1))
plot(NA, xlim=c(20,318), ylim=c(0,0.06), ylab="Activité du virus", xlab="", axes=F)

# Inclusion des données H3N2.prox
polygon(proxy$index[c(1,1:nrow(proxy),nrow(proxy),1)], c(0,proxy$H3N2.prox,0,0),
        col=rgb(0,0,0.9,0.5), border=F) 

# Configurer les axes
axis(1, at=c(20,cumsum(table(proxy$Year))+19), labels=NA, tck=0)
axis(1, at=cumsum(table(proxy$Year))[1:6]+19, labels=NA)
mtext(2009, side=1, at=34, line=0.5)
for(i in 1:5){mtext(2009+i, side=1, at=cumsum(table(proxy$Year))[i]+19+26, line=0.5)}
axis(2, at=0:2*0.02, las=1)

# Titre
title(main="Activité du virus de la grippe A(H3N2) au cours du temps", cex.main=1.2)

# Légende
legend(255, 0.059, legend="Virus de la grippe A(H3N2)",border=NA, bty="n")
legend(258, 0.0572, legend="", pch=22, bg='blue', cex=0.3)
legend(255, 0.055, legend="Collecte de sang", border=NA, bty="n", pch="R")
legend(255, 0.051, legend="Epidémies principales", border=NA, bty="n")
legend(249, 0.0535, legend="", border=NA, bty="n", pch=1, cex = 2)



#function pour ajouter les informations sur la collecte de sang
add_rect_line <- function(start, end, label) {
  lines(c(start, start, end, end), c(0.057, 0.057, 0.057, 0.057)-0.015)
  segments(x0= start, y0=0.00, y1= 0.042, lwd = 2, col = "gray", lty = 2)
  segments(x0= end, y0=0.00, y1= 0.042, lwd = 2, col = "gray", lty = 2)
  polygon(c(start, end, end, start), c(0.042, 0.042, 0.042-0.1, 0.042-0.1), col = rgb(0.8, 0.8, 0.8, 0.2), border = NA)
  text((start+end)/2, 0.044, label, cex = 0.8)
}

# ajout des information pour chaque collecte de sang 
add_rect_line(23, 35, "R1")
add_rect_line(45, 57, "R2")
add_rect_line(102, 115, "R3")
add_rect_line(128, 155, "R4")
add_rect_line(200, 219, "R5")
add_rect_line(257, 273, "R6")
add_rect_line(302, 317, "R7")


# Ajouter les numéros des épidémies principales 
points(c(90,178,245,288), c(0.023,0.029,0.007,0.006), pch=1, cex=2.5)  # cercles
points(c(90,178,245,288), c(0.023,0.029,0.007,0.006), pch=c("1","2","3","4"), cex=1)








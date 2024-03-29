###
#
# Dieser Code macht alle Grafen f�r die Vorlesung und die Regressionsanalysen (weiter unten)
#
###
# by Alan Tump 
 


#For data wrangling 
library(dplyr) 

#For plotting
library(ggplot2)
library(cowplot)

#Loading the data:
df_eval <- read.csv( "evaluation.csv")







dd <- data.frame(x=c(-1,0),y=c(-1.5,-0.5),xend=c(0,1),yend=c(0,1),g = c("Ja","Nein"))
examp <- ggplot(data=dd,aes(x=x,y=y,group=g)) + 
   geom_segment(aes(x = x, y = y, xend = xend, yend = yend, colour=g),size=1.5) +
   theme_cowplot()  +    geom_vline(xintercept = 0, color="red") +
   theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
   theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +xlab("Zuteilungsvariable") + ylab("Abh�ngige Variable") +
   scale_colour_viridis_d("Teilnahme:", end = 0.7) + geom_text(aes(y= 2,x=-0.4,label= "Schwellenwert"), colour= "red",size=5.2)


  
 
 p_dichte <- df_eval %>% 
   mutate(r_armuts_index = floor(armuts_index)+0.5) %>%
   group_by(r_armuts_index, teilnahme_fak) %>% 
   summarise(Dichte = length(r_armuts_index)) %>%
 ggplot(aes(y = Dichte, x = r_armuts_index, group = teilnahme_fak, colour = teilnahme_fak, fill = teilnahme_fak)) +
   geom_vline(xintercept = 50) +
   geom_point() +
   geom_smooth() +
   labs(x = "Armutsindex")  + 
   scale_colour_viridis_d("Teilnahme:", end = 0.7) +
   scale_fill_viridis_d("Teilnahme:", end = 0.7) + theme_bw()#
 
 
 p_teilnahme <- df_eval %>% 
   ggplot(aes(y = teilnahme_fak, x = armuts_index)) +
   geom_vline(xintercept = 50) +
   geom_point() +
   labs(x = "Armutsindex",
        y = "Teilnahme") +
   theme_bw()
  
 
 
 
 p_rda <- df_eval %>%
   ggplot(aes(x = armuts_index , y = gesundh_ausgaben ,
              group = teilnahme_fak, colour = teilnahme_fak, fill = teilnahme_fak)) +
   geom_point(alpha = 0.03) +
   geom_smooth(method = "lm") +
   labs(x = "Armutsindex", y = "Gesundheitsausgaben (Tsd. Euro)") +
   scale_colour_viridis_d("Teilnahme:", end = 0.7) +
   scale_fill_viridis_d("Teilnahme:", end = 0.7)  + theme_bw()
 
 
 
 p_rda_raw <- df_eval %>%
   ggplot(aes(x = armuts_index , y = gesundh_ausgaben ,
              group = teilnahme_fak, colour = teilnahme_fak, fill = teilnahme_fak)) +
   geom_point(alpha = 0.03) +
   #geom_smooth(method = "lm") +
   labs(x = "Armutsindex", y = "Gesundheitsausgaben (Tsd. Euro)") +
   scale_colour_viridis_d("Teilnahme:", end = 0.7) +
   scale_fill_viridis_d("Teilnahme:", end = 0.7)  + theme_bw()
 
 
p_alter <-  df_eval %>%
   ggplot(aes(x = armuts_index, y = alter,
              group = teilnahme_fak, colour = teilnahme_fak, fill = teilnahme_fak)) +
   geom_point(alpha = 0.03) +
   geom_smooth(method = "loess") +
   labs(x = "Armutsindex", y = "Alter (Jahren)") +
   scale_colour_viridis_d("Teilnahme:", end = 0.7) +
   scale_fill_viridis_d("Teilnahme:", end = 0.7)  + theme_bw()




p_krank <-  df_eval %>%
   ggplot(aes(x = armuts_index, y = alter,
              group = teilnahme_fak, colour = teilnahme_fak, fill = teilnahme_fak)) +
   geom_point(alpha = 0.03) +
   geom_smooth(method = "loess") +
   labs(x = "Armutsindex", y = "Entf. vom Krankhaus (km)") +
   scale_colour_viridis_d("Teilnahme:", end = 0.7) +
   scale_fill_viridis_d("Teilnahme:", end = 0.7)  + theme_bw()



p_rda <- df_eval %>%
   ggplot(aes(x = armuts_index , y = gesundh_ausgaben ,
              group = teilnahme_fak, colour = teilnahme_fak, fill = teilnahme_fak)) +
   geom_point(alpha = 0.03) +
   geom_smooth(method = "lm") +
   labs(x = "Armutsindex", y = "Gesundheitsausgaben (Tsd. Euro)") +
   scale_colour_viridis_d("Teilnahme:", end = 0.7) +
   scale_fill_viridis_d("Teilnahme:", end = 0.7)  + theme_bw()



####
# Regressions analysen
###

#eine Steigung
summary(
  lm(
    gesundh_ausgaben ~ teilnahme + I(armuts_index - 50)  + I(armuts_index - 50),
      data = df_eval)) 



# 2 Steigungen
summary(
  lm(
     gesundh_ausgaben ~ teilnahme + I(armuts_index - 50)  + I(armuts_index - 50):teilnahme,
    data = df_eval)) 



# Bayesiansiche Statistik
library(brms)
brm( gesundh_ausgaben ~ teilnahme + I(armuts_index - 50)  + I(armuts_index - 50):teilnahme,
     data = df_eval)




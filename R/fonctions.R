
################################
# Fonction : centre_segment ----
################################

#' centre_segment 
#' @description Cette fonction calcule le centre d'un segment 
#' @param x_a Coordonnées x d'un premier point 
#' @param y_a Coordonnées y d'un premier point 
#' @param x_b Coordonnées x d'un second point 
#' @param y_b Coordonnées y d'un second point 
#' @return Retourne un vecteur (x,y) des coordonnnées du centre d'un segment
#' @noRd
#' @examples
centre_segment <- function(x_a,y_a,x_b,y_b){ 
  return(c((x_a+x_b)/2,(y_a+y_b)/2))
}

################################
# Fonction : divide_triangle ----
################################

#' divide_triangle 
#' @description Cette fonction utilise l'algorithme de  Sierpiński pour 
#' diviser un triangle en 4 triangles  à l'aide du centre de chacun des segments. 
#' 
#' @param vect vecteurs des coordonnées du triangle 
#'
#' @return une liste des coordonnées des trois points des 4 triangles
#' @export
divide_triangle <-
  function(vect_coord = c(x_a, y_a, x_b, y_b, x_c, y_c)) {
    # Extraire les coordonnées du milieu des segments reliant les sommets entre eux.
    c_ab <-
      centre_segment(vect_coord[1], vect_coord[2], vect_coord[3], vect_coord[4])
    c_ac <-
      centre_segment(vect_coord[1], vect_coord[2], vect_coord[5], vect_coord[6])
    c_bc <-
      centre_segment(vect_coord[3], vect_coord[4], vect_coord[5], vect_coord[6])
    # Créer les nouveaux triangles
    t_1 <-
      c(vect_coord[1], vect_coord[2], c_ab[1], c_ab[2], c_ac[1], c_ac[2])
    t_2 <-
      c(vect_coord[3], vect_coord[4], c_ab[1], c_ab[2], c_bc[1], c_bc[2])
    t_3 <-
      c(vect_coord[5], vect_coord[6], c_ac[1], c_ac[2], c_bc[1], c_bc[2])
    return(list(t_1, t_2, t_3))
  }


################################
# Fonction : divide_liste_triangle  ----
################################

#' divide_liste_triangle 
#' @description Cette fonciton prend une liste de triangle et renvoie une liste 
#' composé des 4 triangles issus de la décomposition de Sierpinski. 
#'
#' @param liste_triangle Une liste de coordonées de triangles 
#'
#' @return liste 4 fois plus grandes, on applique l'algorithme de Sierpinski aux 
#' triangles donnés en entrée. 
#' @export
#'
divide_list_triangle <- function(liste_triangle){ 
  sous_liste <- list()
  for (i in liste_triangle){ 
    sous_liste <- append(sous_liste,divide_triangle(i))
  }
  return(sous_liste)
}

################################
# Fonction : distance_cart ----
################################

#' distance_cart : distance cartèsienne 
#' @description Cette fonction calcul la distance cartésienne entre deux points 
#' @param x1 coordonnée x du premier point 
#' @param y1 coordonnée y du premier point 
#' @param x2 coordonnée x du second point
#' @param y2 coordonnée y du second point 
#'
#' @return Elle renvoie la distance sous forme d'entier
#' @export 
distance_cart <- function(x1, y1, x2, y2) {
  return(sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2))
}


################################
# Fonction : heron_liste ----
################################

#' heron_liste 
#' 
#' @description Cette fonction prend en entrée une liste de triangle et 
#' renvoie la somme des aires de ces triangles. 
#'
#' @param liste_triangle C'est une liste de triangle du type : 
#' liste_triangle = list([x11,y11,x21,y21,x31,y31],[x12,y12,x22,y22,x32,y32] ... )
#'
#' @return Elle renvoie l'air de ce triangle 
#' @import heron
#' @export
#' 
heron_liste <- function(liste_triangle) {
  sum_aire <- 0
  # On extrait les distances entre les sommets 
  for (i in seq_along(liste_triangle)) {
    A_B <-
      distance_cart(liste_triangle[[i]][1],
                    liste_triangle[[i]][2],
                    liste_triangle[[i]][3],
                    liste_triangle[[i]][4])
    B_C <-
      distance_cart(liste_triangle[[i]][3],
                    liste_triangle[[i]][4],
                    liste_triangle[[i]][5],
                    liste_triangle[[i]][6])
    C_A <-
      distance_cart(liste_triangle[[i]][5],
                    liste_triangle[[i]][6],
                    liste_triangle[[i]][1],
                    liste_triangle[[i]][2])
    
    # On calcule l'air des triangles 
    sum_aire <- sum_aire + heron(A_B, B_C, C_A)
  }
  return(sum_aire)
}


################################
# Fonction : plot_triangles
################################

#' plot_triangles 
#' @description #' Ainsi cette fonction permet de tracer tous les triangles dont les coordonnées sont rentrées dans 
#' une liste. 
#' @import ggplot2 
#' @param liste_triangle le premier paramètres est une liste de triangles ordonnés de cette manière : 
#' liste_triangle = list([x11,y11,x21,y21,x31,y31],[x12,y12,x22,y22,x32,y32] ... ). 
#'
#' @return Cette fonction affiche sur un graphique tous les triangles. 
#' @export 
plot_triangles <- function(liste_triangle) {
  df <- data.frame()
  
  #Construction d'un data frame avec les coordonnées des triangles
  for (i in seq_along(liste_triangle)) {
    df <- rbind(df, data.frame(
      x = c(liste_triangle[[i]][1], liste_triangle[[i]][3], liste_triangle[[i]][5]),
      y = c(liste_triangle[[i]][2], liste_triangle[[i]][4], liste_triangle[[i]][6]),
      triangle_id = i
    ))
  }
  
  #On trace les triangles
  plot <- ggplot(df, aes(x, y, fill = factor(triangle_id))) +
    geom_polygon(color = "black") +
    scale_fill_manual(values = rep("black", length(liste_triangle)), guide = "none") +
    theme_void()
  
  return(plot)
}


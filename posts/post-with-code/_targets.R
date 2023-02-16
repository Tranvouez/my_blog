library(targets)
library(heron)
library(ggplot2)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.
tar_source()

# Triangle initial ayant pour coordonnées : 
# A = (0,0)   B = (0,1)   C = (0.5,sqrt(3/2))
triangle_initial <- c(0,1,0,0,0.5,sqrt(3)/2) #c(x1,y1,x2,y2,x3,y3)

  list(
    tar_target("Iteration_1", divide_triangle(triangle_initial)),
    tar_target("plot_1", plot_triangles(Iteration_1)),
    tar_target("Aire_1", heron_liste(Iteration_1)),
    tar_target("Iteration_2", divide_list_triangle(Iteration_1)),
    tar_target("plot_2", plot_triangles(Iteration_2)),
    tar_target("Aire_2", heron_liste(Iteration_2)),
    tar_target("Iteration_3", divide_list_triangle(Iteration_2)),
    tar_target("plot_3", plot_triangles(Iteration_3)),
    tar_target("Aire_3", heron_liste(Iteration_3)),
    tar_target("Iteration_4", divide_list_triangle(Iteration_3)),
    tar_target("plot_4", plot_triangles(Iteration_4)),
    tar_target("Aire_4", heron_liste(Iteration_4)),
    tar_target("Render_post", tar_quarto(post.qmd))
  )
  
# Rendering

list.files("vignettes/articles/")


rmarkdown::render("vignettes/articles/aggregate-indicators-to-regions.Rmd", 
                  output_format = "html_document",
                  output_dir = "temp")


rmarkdown::render("vignettes/articles/normalise-condition-variable.Rmd", 
                  output_format = "html_document",
                  output_dir = "temp")

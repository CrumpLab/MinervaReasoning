# Run all of this to run the experiment in a browser

library(xprmntr)

rmarkdown::render("MinervaReasoning/experiment/index.Rmd", "html_document")

run_locally(path="MinervaReasoning",
            show_in = "browser",
            xprmntr_host = "127.0.0.1",
            xprmntr_port = 8000)
 


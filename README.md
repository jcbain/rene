# `R`ené

An `R` package for manipulating data simulated using (`SLiM`)[https://messerlab.org/slim/].

## Using Docker

This package gives users the opportunity to run these scripts using a Docker image for consistent results across operating systems. To start using Docker visit (here)[https://www.docker.com/].

For setup, in your terminal run:
```bash
docker build --tag tidyverse .
```

*For future steps, you can skip this initial setup step*

Then you can run 
```bash
docker run -e PASSWORD=password -p 8787:8787 -v $(pwd):/home/rstudio/work tidyverse
```

Then navigate to http://localhost:8787/ and your username should be `rstudio` and password should be `password`.

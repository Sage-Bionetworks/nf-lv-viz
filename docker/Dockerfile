FROM rocker/tidyverse:3.6.1

## install synapser reqs
RUN apt-get update -y
RUN apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev
RUN apt-get install -y curl libcurl4-openssl-dev

## install synapser
RUN R -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"
RUN R -e "install.packages('synapserutils', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"

## install bioconductor packages
RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install(c('tximport', 'org.Hs.eg.db'))"

## install cran packages
RUN R -e "install.packages('feather')"

## install cran packages
RUN R -e "devtools::install_github('wgmao/PLIER')"

COPY setup.R /home/rstudio/setup.R
COPY generate_data.Rmd /home/rstudio/generate_data.Rmd

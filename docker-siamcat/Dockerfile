#Dockerfile for Nextflow-powered SIAMCAT
#author: Alexander Mattausch, EMBL, Zeller Team 2017
FROM ubuntu:latest
MAINTAINER Alex Mattausch <alexander.mattausch@embl.de>

#Install r-base
RUN apt-get update && apt-get install -y r-base

#Install required R packages
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.uni-muenster.de'; options(repos = r);" > ~/.Rprofile
RUN Rscript -e "install.packages(c('optparse', 'LiblineaR', 'pROC', 'colorRamps', 'RColorBrewer', 'beanplot'))"

#copy SIAMCAT source files
RUN mkdir /opt/SIAMCAT
COPY ./srcdir/* /opt/SIAMCAT/

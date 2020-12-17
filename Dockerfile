FROM rocker/tidyverse

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxt6 \
  qpdf \
  && apt-get clean

WORKDIR /censusnz

RUN R -e "remotes::install_github('harmonic-analytics/db-censusnz')"

COPY DESCRIPTION /censusnz/

RUN R -e "remotes::install_deps(dependencies = TRUE)"

COPY . /censusnz/

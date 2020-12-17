FROM rocker/tidyverse

RUN apt-get update -qq && apt-get install -y --no-install-recommends libxt6 \
  && apt-get clean

WORKDIR /censusnz

RUN R -e "remotes::install_github('harmonic-analytics/db-censusnz')"

COPY DESCRIPTION /censusnz/

RUN R -e "remotes::install_deps(dependencies = TRUE)"

COPY . /censusnz/

# syntax=docker/dockerfile:1
FROM rocker/r-ver:4.3.2

ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libxml2-dev \
    libgit2-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    gfortran \
    make \
    cmake \
    libgsl-dev \
    libgmp-dev \
    openmpi-bin \
    libopenmpi-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

COPY . /workspace

RUN R -e "install.packages('renv', repos = 'https://packagemanager.posit.co/cran/latest')" \
 && R -e "renv::restore(project = 'reproduced', lockfile = 'reproduced/renv.lock', library = renv::paths[['library']](project = 'reproduced'), prompt = FALSE)" \
 && cd reproduced \
 && Rscript scripts/00_setup/docker_smoke_test.R

CMD ["bash"]

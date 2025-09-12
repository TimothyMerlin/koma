FROM rocker/r-ver:4.4.1

# Set working dir
WORKDIR /workdir

# Install devtools and necessary R packages
RUN apt-get update && apt-get install -y \
    libarmadillo-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    g++ \
    liblapack-dev \
    libblas-dev \
    libgfortran5 \
    build-essential \
    libxml2-dev \
    libssl-dev \
    pandoc \
    qpdf \
    && apt-get clean

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

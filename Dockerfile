FROM rocker/r-ver:4.4.0

LABEL maintainer="Pascal Obstetar <pascal.obstetar@gmail.com>"
LABEL description="Nemeton - Plateforme d'analyse forestiere systemique"

# Dependances systeme pour sf, terra, arrow, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libsqlite3-dev \
    pandoc \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Toolchain Rust (noyau cable de foretaccess via extendr). Requis pour compiler
# foretaccess depuis les sources (Remotes: pobsteta/foretaccess@*release) a
# l'etape devtools::install_deps ci-dessous. rustup fournit une version recente
# (celle d'apt est trop ancienne pour extendr). Installe dans /opt/rust, ajoute
# au PATH pour toutes les etapes suivantes.
ENV RUSTUP_HOME=/opt/rust \
    CARGO_HOME=/opt/rust
ENV PATH=/opt/rust/bin:${PATH}
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
      | sh -s -- -y --default-toolchain stable --profile minimal \
    && rustc --version && cargo --version

# Installer les packages R de base en premier (cache Docker)
RUN install2.r --error --skipinstalled \
    shiny \
    bslib \
    golem \
    sf \
    terra \
    arrow \
    leaflet \
    ggplot2 \
    cli \
    rlang \
    glue \
    jsonlite \
    httr2 \
    yaml \
    htmltools \
    DT \
    promises \
    future \
    ellmer \
    shinyOAuth

# Copier le package nemetonshiny
WORKDIR /app
COPY . /app

# Installer le package nemetonshiny et ses dependances restantes
RUN R -e "devtools::install_deps('.', dependencies = TRUE, upgrade = 'never')" \
    && R -e "devtools::install('.', upgrade = 'never')"

EXPOSE 3838

# Variables d'environnement par defaut
ENV NEMETON_LANG=fr
ENV NEMETON_PAYS=FR

CMD ["R", "-e", "nemetonshiny::run_app(options = list(port = 3838, host = '0.0.0.0', launch.browser = FALSE))"]

# hash:sha256:19cd656315d30a1c8f35131bc49328a73c033474d729e17640e40cc035dddaac
FROM registry.codeocean.com/codeocean/py-r:python3.12.8-R4.4.2-JupyterLab4.3.5-RStudiorstudio-server-2024.12.1-563-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN pip install -U --no-cache-dir \
    pandas \
    pyam-iamc

RUN Rscript -e 'remotes::install_version("tidyverse", "2.0.0")' \
    && Rscript -e 'remotes::install_version("doParallel", "1.0.17")' \
    && Rscript -e 'remotes::install_version("ggforce", "0.5.0")' \
    && Rscript -e 'remotes::install_version("ggh4x", "0.3.1")' \
    && Rscript -e 'remotes::install_version("ggrepel", "0.9.8")' \
    && Rscript -e 'remotes::install_version("legendry", "0.3.0")' \
    && Rscript -e 'remotes::install_version("openxlsx", "4.2.9")' \
    && Rscript -e 'remotes::install_version("scico", "1.5.0")' \
    && Rscript -e 'remotes::install_version("wbstats", "1.2")' \
    && Rscript -e 'remotes::install_version("readxl")' \
    && Rscript -e 'remotes::install_version("httr2")'

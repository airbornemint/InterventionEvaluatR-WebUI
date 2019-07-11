FROM rocker/shiny:3.6.0
LABEL maintainer="Ben Artin <ben@artins.org>"

### Setup apt packages needed to build the image
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y -qq

# These are only needed to build the image (and will not be present in the final image)
# ARG BUILD_APT_DEPENDENCIES="sudo"
# RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}
# RUN apt-mark auto ${BUILD_APT_DEPENDENCIES}

# These are included in rocker/shiny, but aren't needed to run our shiny app
# This doesn't do anything for us now, but when docker build --squash is released it will
ARG UNNEEDED_APT_DEPENDENCIES="g++ gfortran libblas-dev libcairo2-dev libcurl4-gnutls-dev libopenblas-dev libxt-dev pandoc pandoc-citeproc"
RUN apt-mark auto ${UNNEEDED_APT_DEPENDENCIES}
# These are needed, and rocker/shiny gets them as deps of what we just marked as unneeded, so we have to make sure to keep them
ARG NEEDED_APT_DEPENDENCIES="libblas3 libcairo2 libcurl3-gnutls libopenblas-base libxt6"
RUN apt-mark manual ${NEEDED_APT_DEPENDENCIES}

### Setup R package dependencies
ARG R_BUILD_APT_DEPENDENCIES="libcurl4-gnutls-dev gnutls-dev libssh2-1-dev libxml2-dev zlib1g-dev libgit2-dev libssl-dev"
ARG R_RUN_APT_DEPENDENCIES="libcurl3-gnutls libssh2-1 libxml2 zlib1g libgit2-24 libssl1.1"
RUN apt-get install -y -qq --no-install-recommends ${R_BUILD_APT_DEPENDENCIES}

### Setup misc other packages needed to run our build
ARG BUILD_APT_DEPENDENCIES="tzdata locales"
#ARG BUILD_APT_DEPENDENCIES="tzdata locales qpdf pandoc pandoc-citeproc"
RUN apt-get install -y -qq --no-install-recommends ${BUILD_APT_DEPENDENCIES}

### Install required R packages. Doing it this way allows docker to cache packrat state and not rebuild everything when app source changes in a way that doesn't affect package dependencies
COPY --chown=shiny packrat/packrat.lock packrat/packrat.opts /srv/shiny-server/WebUI/packrat/
RUN install2.r packrat
RUN ( cd /srv/shiny-server/WebUI ; sudo -u shiny Rscript -e 'packrat::restore()' )
### Install Shiny app
COPY --chown=shiny . /srv/shiny-server/WebUI
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Finish up the configuration
RUN echo tzdata tzdata/Areas select America >> /tmp/debconf.txt
RUN echo tzdata tzdata/Zones/America select New_York >> /tmp/debconf.txt
RUN debconf-set-selections /tmp/debconf.txt
RUN rm /etc/timezone
RUN rm /etc/localtime
RUN dpkg-reconfigure --frontend noninteractive tzdata
RUN update-locale --reset LANG=en_US.utf8 LANGUAGE=en_US LC_ALL=en_US.utf8 LC_CTYPE=en_US.utf8

### Cleanup
RUN apt-get autoremove -y
RUN apt-get clean
RUN rm -rf /tmp/*

USER shiny
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"] 


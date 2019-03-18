FROM rocker/shiny

MAINTAINER vnevyhosteny

RUN apt-get update && \
    apt-get install -y git libxml2-dev libssl-dev ghostscript apt-utils libjpeg-dev libgeos-dev sqlite3

COPY PERMON_v2 /srv/shiny-server
WORKDIR /srv/shiny-server

RUN Rscript -e 'install.packages("packrat"); \
                packrat::restore()'

RUN chown shiny:shiny -R .

EXPOSE 3838
CMD /usr/bin/shiny-server.sh

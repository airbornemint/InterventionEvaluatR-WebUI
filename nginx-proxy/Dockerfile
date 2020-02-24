FROM jwilder/nginx-proxy
MAINTAINER ben@artins.org
RUN mkdir -p /etc/nginx/vhost.d
# This sets up the permissions for the session data volume so that it can be written from the shiny container
RUN mkdir /mnt/session-data && chown 999:999 /mnt/session-data
COPY dev.interventionevaluatr.com /etc/nginx/vhost.d

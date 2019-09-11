FROM openjdk:8-jre

RUN mkdir -p /opt/shinyproxy/
RUN wget https://www.shinyproxy.io/downloads/shinyproxy-2.3.0.jar -O /opt/shinyproxy/shinyproxy.jar
COPY application.yml /opt/shinyproxy/application.yml
# See https://unix.stackexchange.com/questions/294835/replace-environment-variables-in-a-file-with-their-actual-values
ARG APP_IMAGE
RUN perl -p -i -e 's{\$(\{)?(\w+)(?(1)\})}{$ENV{$2} // $&}ge' /opt/shinyproxy/application.yml

WORKDIR /opt/shinyproxy/
CMD ["java", "-jar", "/opt/shinyproxy/shinyproxy.jar"]

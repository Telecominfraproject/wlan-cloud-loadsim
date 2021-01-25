#Build stage 0

FROM erlang:alpine
RUN apk update && \
    apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache bash util-linux coreutils curl make cmake gcc g++ libstdc++ libgcc git zlib-dev && \
    touch /usr/include/sys/vtimes.h
RUN git clone https://github.com/telecominfraproject/wlan-cloud-loadsim /owls
WORKDIR /owls
RUN make
RUN mkdir /app_data
RUN mkdir /app_data/mnesia
RUN mkdir /app_data/logs

EXPOSE 9090
EXPOSE 4369

ENTRYPOINT /owls/docker_start.sh



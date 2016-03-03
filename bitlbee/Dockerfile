FROM alpine
MAINTAINER Vincent Ambo <tazjin@gmail.com>

# Install bitlbee packages
RUN apk update && \
    apk add bitlbee bitlbee-otr bitlbee-doc ca-certificates curl gnutls

# Install stunnel
RUN echo "http://dl-3.alpinelinux.org/alpine/edge/testing/" >> /etc/apk/repositories
RUN apk update && apk add stunnel

# Add a user for bitlbee
RUN adduser -D bitlbee

# Add bitlbee configuration
ADD bitlbee.conf /etc/bitlbee/bitlbee.conf
ADD motd.txt /etc/bitlbee/motd.txt

# Add stunnel configuration
ADD stunnel.conf /etc/bitlbee/stunnel.conf

EXPOSE 6697

CMD bitlbee -F && stunnel /etc/bitlbee/stunnel.conf

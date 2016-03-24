FROM alpine
MAINTAINER Vincent Ambo <tazjin@gmail.com>

# Install Quassel server packages
RUN apk update && apk add quassel-core qt-sqlite icu-libs

# Location for mounting Quassel state and configuration volume
VOLUME /var/lib/quassel

EXPOSE 4242
USER quassel

CMD /usr/bin/quasselcore

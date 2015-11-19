FROM fpco/stack-build
MAINTAINER Vincent Ambo <dev@tazj.in>

# Base setup
VOLUME /var/tazblog
EXPOSE 8000

# Build blog
ADD . /opt/tazblog/src
WORKDIR /opt/tazblog/src
RUN stack build && cp .stack-work/

# Done!
CMD /usr/bin/tazblog

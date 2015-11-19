FROM fpco/stack-build
MAINTAINER Vincent Ambo <dev@tazj.in>

# Base setup
VOLUME /var/tazblog
EXPOSE 8000 8070
ENV PATH /root/.local/bin:$PATH

# Build blog
ADD . /opt/tazblog
WORKDIR /opt/tazblog
RUN stack install && cp /root/.local/bin/tazblog /usr/bin/tazblog

# Done!
CMD tazblog

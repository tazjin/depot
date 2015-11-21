FROM centos:7
MAINTAINER Vincent Ambo <hej@tazj.in>

EXPOSE 6081 6082 6083

RUN yum install -y epel-release && \
    rpm --nosignature -i https://repo.varnish-cache.org/redhat/varnish-4.1.el7.rpm && \
    yum install -y varnish

ADD default.vcl /etc/varnish/default.vcl

CMD ulimit -n 131072 && \
    /usr/sbin/varnishd -F -f /etc/varnish/default.vcl \
                       -a :6081 -T :6082 -a :6083,PROXY -t 120

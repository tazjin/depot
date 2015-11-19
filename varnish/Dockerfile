FROM centos:7
MAINTAINER Vincent Ambo <hej@tazj.in>

EXPOSE 6081 6082

RUN yum install -y epel-release && yum install -y varnish

ADD default.vcl /etc/varnish/default.vcl

CMD ulimit -n 131072 && \
    /usr/sbin/varnishd -F -f /etc/varnish/default.vcl -a :6081 -T :6082 -t 120

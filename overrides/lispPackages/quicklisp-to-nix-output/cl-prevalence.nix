args @ { fetchurl, ... }:
rec {
  baseName = ''cl-prevalence'';
  version = ''20130720-hg'';

  description = ''Common Lisp Prevalence Package'';

  deps = [ args."s-sysdeps" args."s-xml" ];

  src = fetchurl {
    url = ''http://beta.quicklisp.org/archive/cl-prevalence/2013-07-20/cl-prevalence-20130720-hg.tgz'';
    sha256 = ''09pqbw6xcgy0242npiqw7sd8jwwjc0kz7m0sas48jjr0zgnnmi89'';
  };

  packageName = "cl-prevalence";

  asdFilesToKeep = ["cl-prevalence.asd"];
  overrides = x: x;
}
/* (SYSTEM cl-prevalence DESCRIPTION Common Lisp Prevalence Package SHA256
    09pqbw6xcgy0242npiqw7sd8jwwjc0kz7m0sas48jjr0zgnnmi89 URL
    http://beta.quicklisp.org/archive/cl-prevalence/2013-07-20/cl-prevalence-20130720-hg.tgz
    MD5 6176c34b8e1621b65906b1575d9fa20d NAME cl-prevalence FILENAME
    cl-prevalence DEPS
    ((NAME s-sysdeps FILENAME s-sysdeps) (NAME s-xml FILENAME s-xml))
    DEPENDENCIES (s-sysdeps s-xml) VERSION 20130720-hg SIBLINGS
    (cl-prevalence-test) PARASITES NIL) */

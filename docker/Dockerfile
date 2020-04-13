FROM lokedhs/sbcl-quicklisp

RUN apt-get update                              \
  && RUNLEVEL=1 DEBIAN_FRONTEND=noninteractive  \
     apt-get install -y --no-install-recommends \
       default-jdk \
       clojure \
       leiningen \
       csh \
  && apt-get autoremove                         \
  && apt-get clean

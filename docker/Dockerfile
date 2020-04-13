FROM clojure

RUN apt-get update                              \
  && RUNLEVEL=1 DEBIAN_FRONTEND=noninteractive  \
     apt-get install -y --no-install-recommends \
       csh \
  && apt-get autoremove                         \
  && apt-get clean

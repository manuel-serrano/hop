#! /bin/sh

. /etc/conf.d/hop 

if [ "$3 " = "--kill " ]; then
  /bin/sh -c "/usr/bin/hop $* ${DAEMONARGS}"
else
  /bin/sh -c "/usr/bin/hop $* ${DAEMONARGS} &"
fi


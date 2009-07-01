#!/bin/sh
#
# bee init.d script.
# based on kestrel's
#

BEE_HOME="/usr/local/bee"
AS_USER="daemon"
VERSION="0.1"
DAEMON="/usr/local/bin/daemon"

daemon_args="--name bee --pidfile /var/run/bee.pid"
HEAP_OPTS="-Xmx2048m -Xms1024m -XX:NewSize=256m"
JMX_OPTS="-Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.port=22134 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false"
# add JMX_OPTS below if you want jmx support.
JAVA_OPTS="-server -verbosegc -XX:+PrintGCDetails -XX:+UseConcMarkSweepGC -XX:+UseParNewGC $HEAP_OPTS"


function running() {
  $DAEMON $daemon_args --running
}

function find_java() {
  if [ ! -z "$JAVA_HOME" ]; then
    return
  fi
  potential=$(ls -r1d /opt/jdk /System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home /usr/java/default /usr/java/j* 2>/dev/null)
  for p in $potential; do
    if [ -x $p/bin/java ]; then
      JAVA_HOME=$p
      break
    fi
  done
}


# dirs under /var/run can go away between reboots.
for p in /var/run/bee /var/log/bee $QUEUE_PATH; do
  if [ ! -d $p ]; then
    mkdir -p $p
    chmod 775 $p
    chown $AS_USER $p >/dev/null 2>&1 || true
  fi
done

find_java


case "$1" in
  start)
    echo -n "Starting bee... "

    if [ ! -r $BEE_HOME/bee-$VERSION.jar ]; then
      echo "FAIL"
      echo "*** bee jar missing - not starting"
      exit 1
    fi
    if [ ! -x $JAVA_HOME/bin/java ]; then
      echo "FAIL"
      echo "*** $JAVA_HOME/bin/java doesn't exist -- check JAVA_HOME?"
      exit 1
    fi
    if running; then
      echo "already running."
      exit 0
    fi
    
    ulimit -n 8192 || echo -n " (no ulimit)"
    $DAEMON $daemon_args --user $AS_USER --stdout=/var/log/bee/stdout --stderr=/var/log/bee/error -- ${JAVA_HOME}/bin/java ${JAVA_OPTS} -cp ${BEE_HOME}/bee-${VERSION}.jar net.jpbougie.bee.Bee
    tries=0
    while ! running; do
      tries=$((tries + 1))
      if [ $tries -ge 5 ]; then
        echo "FAIL"
        exit 1
      fi
      sleep 1
    done
    echo "done."
  ;;

  stop)
    echo -n "Stopping bee... "
    if ! running; then
      echo "wasn't running."
      exit 0
    fi
    
    $DAEMON $daemon_args --stop
    
    tries=0
    while running; do
      tries=$((tries + 1))
      if [ $tries -ge 5 ]; then
        echo "FAIL"
        exit 1
      fi
      sleep 1
    done
    echo "done."
  ;;
  
  status)
    if running; then
      echo "bee is running."
    else
      echo "bee is NOT running."
    fi
  ;;

  restart)
    $0 stop
    sleep 2
    $0 start
  ;;

  *)
    echo "Usage: /etc/init.d/bee {start|stop|restart|status}"
    exit 1
  ;;
esac

exit 0

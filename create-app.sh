#!/bin/sh

mkdir -p $1/data/mnesia $1/deps $1/include $1/src

APPNAME=`basename $1`
cat > $1/Makefile <<EOF
REBAR := /opt/local/bin/rebar

.PHONY: all deps doc test clean release start

all: deps
	\$(REBAR) compile

deps:
	\$(REBAR) get-deps

doc:
	\$(REBAR) doc skip_deps=true

test:
	\$(REBAR) eunit skip_deps=true

clean:
	\$(REBAR) clean

release: all test
	dialyzer --src src/*.erl deps/*/src/*.erl
	
start: all
	./start.sh
EOF

cat > $1/start.sh <<EOF
#!/bin/sh
PWD="\$(pwd)"
APPNAME="\$(basename \$PWD)"

. \$APPNAME.conf
. \$APPNAME-local.conf

export datadir NODEMANE

[ "x" = "x\$ELMAR_LOGS" ] && ELMAR_LOGS="\${datadir}/\${NODENAME}.log"
[ "x" = "x\$ELMAR_SASL_LOGS" ] && ELMAR_SASL_LOGS="\${datadir}/\${NODENAME}-sasl.log"

erl -pa "\$PWD/ebin" deps/*/ebin -boot start_sasl \\
    -name \$NODENAME \\
    -s mnesia \\
    -s reloader \\
    -s \$APPNAME \\
    -env ERL_CRASH_DUMP "\${datadir}/erlang_crash_\$\$.dump" \\
    -sasl errlog_type error \\
    -kernel error_logger '{file,"'\${ELMAR_LOGS}'"}' \\
    -sasl sasl_error_logger '{file,"'\${ELMAR_SASL_LOGS}'"}' \\
    -os_mon start_cpu_sup true \\
    -os_mon start_disksup false \\
    -os_mon start_memsup false \\
    -setcookie random \\
    -mnesia dir "\"\${datadir}/mnesia\"" \\
    +K true \\
    +P 65536 
EOF
chmod +x $1/start.sh

cat > $1/$APPNAME.conf <<EOF
#Global Application setting
datadir=data
EOF

cat > $1/$APPNAME-local.conf <<EOF
#local node setting
NODENAME="$APPNAME@\`hostname -s\`"
EOF

cat > $1/src/$APPNAME.erl <<EOF
-module($APPNAME).
-export([start/0]).

start()->
    application:start(?MODULE).
EOF

cat > $1/.gitignore <<EOF
deps/*
ebin/*
EOF

cat > $1/rebar.config <<EOF
{deps, [
    {'erltookit', ".*", {git, "git://github.com/flaboy/erltookit.git", "master"}}
]}.
EOF

(cd $1;rebar create-app appid=$APPNAME )

(cd $1;. $APPNAME-local.conf; export NODENAME;
erl -noinput -mnesia dir "\"data/mnesia\"" -s mnesia create_schema $NODENAME \
    -s init stop -name $NODENAME)

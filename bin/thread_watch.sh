#!/bin/bash 

##############################################################################
# Include 'cmdarg', a command line argument parsing lib for bash scripts
# see: https://github.com/akesterson/cmdarg
script_dir="$(dirname "$0")"
source "$script_dir/cmdarg.sh"

##############################################################################
# Add some meta data into the command line interface

cmdarg_info "header"    "Shell script for watching a java thread's stack trace over time"
cmdarg_info "author"    "matias@iteego.com, marcus@iteego.com"
cmdarg_info "copyright" "(C) Iteego 2017"

##############################################################################
# Build the command line interface and parse the command line

# pre-declare the web servers array so that cmdarg can use it
# according to docs, this should be 'declare -A web_servers' but that does not 
# seem to work on my system or the target systems
web_servers=()

cmdarg 'p:'   'pid' 'The process id of the java os process.  Example: 1234'
cmdarg 't:'   'tid' 'The thread id of a JVM thread. You need to run jstack once on the process to get this.'
# parse command line, exit on errors, parse will print out usage 
# and errors if it fails
cmdarg_parse "$@" || { exit 1; }

# send in a pid and tid as arguments
# you can find the tid by issuing one jstack before this invocation 
watch -n 1 "jstack ${cmdarg_cfg['pid']} | java -jar ../target/thread-watch-0.1.0-SNAPSHOT-standalone.jar -t ${cmdarg_cfg['tid']}"

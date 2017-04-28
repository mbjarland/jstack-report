#!/bin/bash 

# send in a pid and tid as arguments
# you can find the tid by issuing one jstack before this invocation 
watch -n 1 "jstack $1 | java -jar thread-watch-0.1.0-SNAPSHOT-standalone.jar -t $2"

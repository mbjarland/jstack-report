#!/bin/bash 

watch -n 1 "jstack $1 | java -jar ./thread-watch.jar -t $2"

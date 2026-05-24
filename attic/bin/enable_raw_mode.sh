#!/bin/bash

save_state=$(stty -g)
stty raw

java -jar target/thread-watch-0.1.0-SNAPSHOT-standalone.jar -p 1

stty "$save_state"


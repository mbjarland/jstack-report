#!/bin/bash 

cp src/thread_watch/ansi.clj ./jstack-report
cat src/thread_watch/core.clj >> ./jstack-report

chmod +x jstack-report 
sed -i 's/ns thread-watch.core/ns user/g' jstack-report
echo "(report (dump *input*))" >> jstack-report

echo "babashka executable created at ./jstack-report"
echo ""
echo "usage: cat thread-dump.txt | jstack-report"

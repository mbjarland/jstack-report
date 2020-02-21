#!/bin/bash 

SCRIPT="jstack-report"

rm "$SCRIPT"
echo "#!/usr/bin/env -S bb -i -o" >> "$SCRIPT"
cat src/thread_watch/ansi.clj src/thread_watch/core.clj >> "$SCRIPT"

chmod +x "$SCRIPT" 
sed -i 's/ns thread-watch.core/ns user/g' "$SCRIPT"
echo "(report (dump *input*))" >> "$SCRIPT"

echo "babashka executable created at $SCRIPT"
echo ""
echo "usage: cat thread-dump.txt | $SCRIPT"

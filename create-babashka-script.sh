#!/bin/bash 

SCRIPT="jstack-report"

rm "$SCRIPT"
echo "#!/usr/bin/env -S bb -i -o" >> "$SCRIPT"
cat src/jstack_report/ansi.clj src/jstack_report/core.clj >> "$SCRIPT"

chmod +x "$SCRIPT" 
echo "(report (dump user/*input*))" >> "$SCRIPT"

echo ""
echo "babashka executable created at $SCRIPT"
echo ""
echo "usage: cat thread-dump.txt | $SCRIPT"
echo ""
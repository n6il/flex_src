#!/bin/bash -x
~/src/multicomp6809/flex/flex_vfs <<EOF
new DBASIC14.DSK t35,s18
mount 0 DBASIC14.DSK
mount 1 FL_504F.DSK
mount 2 FLXFH504.DSK
copy 1.DBASIC.CMD 0
import 0 DBASIC.SYS xxx=raw
copy 2.RTF.BAS 0
dir 0
quit
EOF

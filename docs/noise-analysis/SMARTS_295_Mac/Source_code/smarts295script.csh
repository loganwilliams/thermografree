#!/bin/csh 
# Use "source smarts295script.csh" to execute this file
# or 
# "chmod u+x smarts295script.csh"
# and then
# "./smarts295script.csh"
#
foreach i (01 02 03 04 june july 20050914)
cp -f smarts_$i.inp.txt smarts295.inp.txt
./smarts295bat
mv smarts295.out.txt smarts_$i.out.txt
mv smarts295.scn.txt smarts_$i.scn.txt
mv smarts295.ext.txt smarts_$i.ext.txt
end

#!/usr/bin/env bash
# Many branches - perform client role, selecting from the branches
# Usage: mbc_profile.sh | tee results.csv

FN="Large.scr"
TEMPLATE="ManyBranchesClient.purs"
IFS=

echo "Branches, Wall clock time, User time, System time, Memory (kbytes)"

for ((s=1; s<=3; s++))
do
  for ((i=2; i<=15; i+=1))
  do
     rm -f $FN
     rm -rf src
     mkdir src

     echo $'module Large;\n\nglobal protocol ManyBranches(role Client, role Server) {' > $FN
     cat $TEMPLATE > src/Main.purs

     echo $'  choice at Client {\n    B1() from Client to Server;' >> $FN
     for ((j=2; j<=$i; j++))
     do
       echo $'  } or {\n    B'$j'() from Client to Server;' >> $FN
     done

     echo $'  }\n}' >> $FN

     docker run -v $(PWD):/gen/ --entrypoint "./scribblec.sh" scribble-server -d ./scribblec.sh -d /gen/src /gen/Large.scr -api-ps ManyBranches Client > /dev/null 2>&1
     RESULTS=$( { gtime -o /dev/fd/3 -v pulp build 2> /dev/null > /dev/null; } 2>&1 3>&1)
     WCTIME=$(echo $RESULTS | grep "Elapsed (wall clock) time" | awk 'NF>1{print $NF}')
     UTIME=$(echo $RESULTS | grep "User time" | awk 'NF>1{print $NF}')
     STIME=$(echo $RESULTS | grep "System time" | awk 'NF>1{print $NF}')
     MEM=$(echo $RESULTS | grep "Maximum resident set size" | awk 'NF>1{print $NF}')
     echo $i, $WCTIME, $UTIME, $STIME, $MEM
  done
done

#! /bin/sh
#$ -S /bin/sh
#$ -N case2
#$ -cwd
#$ -j y
#$ -q ss
#$ -l h_vmem=64G
#$ -pe smp 16
#$ -V


export OMP_STACKSIZE=2097152
export OMP_NUM_THREADS=$NSLOTS

ulimit -s unlimited

for prog in ./*.out
do
for i in 1 2 3 4 5 6 7 8 9 
do
./$prog > ./${prog%.out}_$i.data
sleep 5
done
done

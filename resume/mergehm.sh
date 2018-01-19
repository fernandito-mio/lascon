#!/bin/bash
## declare an array variable
# hm_RANDOM__BOOLEAN__2_001
#declare -a search=("hm_IES_SFS_dgi" "hm_SBS_dgi")
declare -a degree=("2_0" "4_0" "6_0")
#declare -a ntimes=("20" "50")
#declare -a top=("SCALE_FREE" "RANDOM")
declare -a top=("RANDOM")
#declare -a dyn=("BOOLEAN" "PROBABILISTIC_BOOLEAN")
declare -a dyn=("BOOLEAN")

for g in "${degree[@]}"
do
	for t in "${top[@]}"
	do
		for d in "${dyn[@]}"
		do
			#for nt in "${ntimes[@]}"
			#do
				#montage "hm_"$t"__"$d"__"$g"__"$nt"_*.png" -tile 8x4 -geometry +2+2 "hm_dgi__"$t"__"$d"__"$g"__"$nt".png"
				#rm "hm_dgi__"$t"__"$d"__"$g"__"$nt"_*.png"
				montage "hm_"$t"__"$d"__"$g"*.png" -tile 8x4 -geometry +2+2 "hm_dgi__"$t"__"$d"__"$g".png"
				rm "hm_"$t"__"$d"__"$g"*.png"
			#done
		done
	done
done

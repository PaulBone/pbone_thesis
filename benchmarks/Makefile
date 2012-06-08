
.PHONY: all
all : tags report \
	bench_right_vs_left \
	bench_nolimit

report : *.m
	mmc --make $@

bench_right_vs_left : *.m
	mmc --make $@

bench_nolimit : *.m
	mmc --make $@

tags : *.m
	mtags *.m


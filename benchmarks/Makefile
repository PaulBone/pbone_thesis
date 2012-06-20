
.PHONY: all

TARGETS = tags report \
	bench_right_vs_left \
	bench_nolimit \
	bench_gc \
    bench_gc_ts

all : $(TARGETS)

report : *.m
	mmc --make $@

bench_right_vs_left : *.m
	mmc --make $@

bench_nolimit : *.m
	mmc --make $@

bench_gc : *.m
	mmc --make $@

bench_gc_ts : *.m
	mmc --make $@

tags : *.m
	mtags *.m

.PHONY : clean
clean:
	rm -rf Mercury $(TARGETS) 


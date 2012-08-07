
.PHONY: all

TARGETS = tags report \
	bench_right_vs_left \
	bench_right_vs_left_2010 \
    bench_work_stealing \
	bench_nolimit \
	bench_gc \
    bench_gc_extra \
    bench_gc_ts

all : $(TARGETS)

report : *.m
	mmc --make $@

bench_right_vs_left : *.m
	mmc --make $@

bench_right_vs_left_2010 : *.m
	mmc --make $@

bench_work_stealing : *.m
	mmc --make $@

bench_nolimit : *.m
	mmc --make $@

bench_gc : *.m
	mmc --make $@

bench_gc_extra : *.m
	mmc --make $@

bench_gc_ts : *.m
	mmc --make $@

tags : *.m
	mtags *.m

.PHONY : clean
clean:
	rm -rf Mercury $(TARGETS) *.err *.mh


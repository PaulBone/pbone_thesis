#!/usr/bin/env perl
#
# Cropping a portion of a postscript file
# Postscript magic and instructions provided by Chapman Flack.
# Script implemented by Diego Zamboni <zamboni@cerias.purdue.edu>
#
# 1.  View the page in gv.  If it's a multipage document, mark the pag
# +e and
#     hit "save marked" so you have a small file to deal with.
# 2.  Move the mouse pointer to the lower left corner of the area you 
# +want.
#     Record the gv displayed coordinates as X1 and Y1.
# 3.  Point to the upper right corner and record X2 and Y2.
# 4.  Run "crop-ps X1 Y1 X2 Y2 file.ps > cropped_file.ps"
#
# This will not work for postscript files that do not have the appropr
# +iate
# structuring comments (%%EndSetup, %%Page, etc.)

$usage="Usage: $0 x1 y1 x2 y2 file.ps";

$x1=shift @ARGV;
$y1=shift @ARGV;
$x2=shift @ARGV;
$y2=shift @ARGV;

#($x1 && $y1 && $x2 && $y2 ) or
#        die "$usage\n";

$w=$x2-$x1;
$h=$y2-$y1;

$flag=0;

while (<>) {
  if (/^%%BoundingBox:/) {
    print "%%BoundingBox: 0 0 $w $h\n";
  }
  elsif (!$flag && (/^%%EndSetup/ || /^%%Page:/)) {
    print;
    print "-$x1 -$y1 translate\n";
    print "$x1 $y1 moveto $x1 $y2 lineto $x2 $y2 lineto $x2 $y1 lineto closepath\n";
    print "clip\n";
    $flag=1;
  }
  else {
    print;
  }
}


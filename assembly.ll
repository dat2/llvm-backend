; ModuleID = 'assembly.ll'
source_filename = "source.silver"

define float @main() {
entry:
  %0 = fadd nnan ninf nsz arcp float 3.000000e+00, 2.000000e+00
  ret float %0
}

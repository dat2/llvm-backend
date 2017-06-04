; ModuleID = 'assembly.ll'
source_filename = "source.silver"

define float @fadd(float %x, float %y) {
entry:
  %0 = fadd nnan ninf nsz arcp float 3.000000e+00, 2.000000e+00
  ret float %0
}

define i32 @iadd(i32 %x, i32 %y) {
entry:
  %0 = add nuw i32 3, 2
  ret i32 %0
}

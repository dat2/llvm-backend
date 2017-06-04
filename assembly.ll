; ModuleID = 'assembly'
source_filename = "source.silver"

define float @fadd(float %x, float %y) {
entry:
  ret float 5.000000e+00
}

define i32 @iadd(i32 %x, i32 %y) {
entry:
  %0 = sub nuw i32 %y, 3
  %1 = add nuw i32 %x, %0
  ret i32 %1
}

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

define i32 @inc(i32 %x) {
entry:
  %0 = add nuw i32 %x, 1
  ret i32 %0
}

define i32 @doubleInc(i32 %x) {
entry:
  %0 = call i32 @inc(i32 %x)
  %1 = call i32 @inc(i32 %0)
  ret i32 %1
}

define i32 @main() {
entry:
  %0 = call i32 @doubleInc(i32 -2)
  ret i32 %0
}

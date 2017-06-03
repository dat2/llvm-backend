; ModuleID = 'assembly.ll'
source_filename = "source.silver"

define i32 @main() {
entry:
  %0 = add i32 3, 2
  ret i32 %0
}

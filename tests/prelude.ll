%struct.String = type { ptr, i32 }
%struct.FatPtr = type { ptr, i32 }

; void string_as_str(struct FatPtr *ptr, String *self) {
;   ptr->data = self->data;
;   ptr->length = self->length;
; }
define void @String.as_str (ptr %0, ptr %1) {
    %3 = alloca ptr
    %4 = alloca ptr
    store ptr %0, ptr %3
    store ptr %1, ptr %4
    %5 = load ptr, ptr %4
    %6 = getelementptr %struct.String, ptr %5, i32 0, i32 0
    %7 = load ptr, ptr %6
    %8 = load ptr, ptr %3
    %9 = getelementptr %struct.FatPtr, ptr %8, i32 0, i32 0
    store ptr %7, ptr %9
    %10 = load ptr, ptr %4
    %11 = getelementptr %struct.String, ptr %10, i32 0, i32 1
    %12 = load i32, ptr %11
    %13 = load ptr, ptr %3
    %14 = getelementptr %struct.FatPtr, ptr %13, i32 0, i32 1
    store i32 %12, ptr %14
    ret void
}


; uint32_t string_len(String *self) { return self->length; }
define i32 @String.len(ptr %0) {
  %2 = alloca ptr
  store ptr %0, ptr %2
  %3 = load ptr, ptr %2
  %4 = getelementptr %struct.String, ptr %3, i32 0, i32 1
  %5 = load i32, ptr %4
  ret i32 %5
}
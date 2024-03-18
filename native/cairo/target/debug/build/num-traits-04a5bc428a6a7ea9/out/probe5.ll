; ModuleID = 'probe5.c85dd862f7b9a3d0-cgu.0'
source_filename = "probe5.c85dd862f7b9a3d0-cgu.0"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; core::f64::<impl f64>::is_subnormal
; Function Attrs: inlinehint nonlazybind uwtable
define internal zeroext i1 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$12is_subnormal17hf0b4c7246cdcecbdE"(double %self) unnamed_addr #0 {
start:
  %_2 = alloca i8, align 1
  %_4 = fcmp une double %self, %self
  br i1 %_4, label %bb1, label %bb2

bb2:                                              ; preds = %start
  %b = bitcast double %self to i64
  %_6 = and i64 %b, 4503599627370495
  %_7 = and i64 %b, 9218868437227405312
  %0 = icmp eq i64 %_6, 0
  br i1 %0, label %bb5, label %bb6

bb1:                                              ; preds = %start
  store i8 0, ptr %_2, align 1
  br label %bb3

bb5:                                              ; preds = %bb2
  switch i64 %_7, label %bb6 [
    i64 9218868437227405312, label %bb8
    i64 0, label %bb9
  ]

bb6:                                              ; preds = %bb5, %bb2
  %1 = icmp eq i64 %_7, 0
  br i1 %1, label %bb10, label %bb7

bb8:                                              ; preds = %bb5
  store i8 1, ptr %_2, align 1
  br label %bb4

bb9:                                              ; preds = %bb5
  store i8 2, ptr %_2, align 1
  br label %bb4

bb4:                                              ; preds = %bb7, %bb10, %bb9, %bb8
  br label %bb3

bb10:                                             ; preds = %bb6
  store i8 3, ptr %_2, align 1
  br label %bb4

bb7:                                              ; preds = %bb6
  store i8 4, ptr %_2, align 1
  br label %bb4

bb3:                                              ; preds = %bb1, %bb4
  %2 = load i8, ptr %_2, align 1, !range !3, !noundef !4
  %_3 = zext i8 %2 to i64
  %_0 = icmp eq i64 %_3, 3
  ret i1 %_0
}

; probe5::probe
; Function Attrs: nonlazybind uwtable
define void @_ZN6probe55probe17h6b735cd786f2cfc0E() unnamed_addr #1 {
start:
; call core::f64::<impl f64>::is_subnormal
  %_1 = call zeroext i1 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$12is_subnormal17hf0b4c7246cdcecbdE"(double 1.000000e+00)
  ret void
}

attributes #0 = { inlinehint nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }
attributes #1 = { nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="x86-64" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 2, !"RtLibUseGOT", i32 1}
!2 = !{!"rustc version 1.76.0 (07dca489a 2024-02-04)"}
!3 = !{i8 0, i8 5}
!4 = !{}

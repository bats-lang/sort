(* sort -- in-place sorting on arrays *)
(* Insertion sort. No $UNSAFE, no assume. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR

(* ============================================================
   API
   ============================================================ *)

#pub fun
sort_int
  {l:agz}{n:pos}
  (arr: !$A.arr(int, l, n), len: int n)
  : void

#pub fun{a:t@ype}
sort_with
  {l:agz}{n:pos}
  (arr: !$A.arr(a, l, n), len: int n, cmp: (a, a) -<cloref1> int)
  : void

(* ============================================================
   Implementation -- insertion sort with custom comparator
   ============================================================ *)

implement{a}
sort_with{l}{n}(arr, len, cmp) = let
  (*
   * Inner loop: shift elements right while arr[j] > key.
   * j ranges from n-2 down to -1.
   * Metric: j + 1 (always non-negative, decreases each step).
   *)
  fun loop_j
    {j:int | j >= ~1; j < n} .<j + 1>.
    (arr: !$A.arr(a, l, n), j: int j, key: a, len: int n)
    : void =
    if j >= 0 then let
      val cur = $A.get<a>(arr, $AR.checked_idx(j, len))
      val c = cmp(cur, key)
    in
      if c > 0 then let
        val () = $A.set<a>(arr, $AR.checked_idx(j + 1, len), cur)
      in loop_j(arr, j - 1, key, len) end
      else
        $A.set<a>(arr, $AR.checked_idx(j + 1, len), key)
    end
    else
      $A.set<a>(arr, $AR.checked_idx(j + 1, len), key)

  (*
   * Outer loop: iterate i from 1 to n-1.
   * Metric: n - i (decreases each step).
   *)
  fun loop_i
    {i:nat | i <= n} .<n - i>.
    (arr: !$A.arr(a, l, n), i: int i, len: int n)
    : void =
    if i < len then let
      val key = $A.get<a>(arr, i)
    in
      loop_j(arr, i - 1, key, len);
      loop_i(arr, i + 1, len)
    end
    else ()
in
  loop_i(arr, 1, len)
end

(* ============================================================
   Implementation -- sort_int via sort_with
   ============================================================ *)

implement
sort_int{l}{n}(arr, len) =
  sort_with<int>(arr, len, lam (a: int, b: int): int =<cloref1> a - b)

(* ============================================================
   Static tests
   ============================================================ *)

fn _test_sort_int(): void = let
  val arr = $A.alloc<int>(5)
  val () = $A.set<int>(arr, 0, 5)
  val () = $A.set<int>(arr, 1, 3)
  val () = $A.set<int>(arr, 2, 1)
  val () = $A.set<int>(arr, 3, 4)
  val () = $A.set<int>(arr, 4, 2)
  val () = sort_int(arr, 5)
  val () = $A.free<int>(arr)
in () end

fn _test_sort_with(): void = let
  val arr = $A.alloc<int>(4)
  val () = $A.set<int>(arr, 0, 10)
  val () = $A.set<int>(arr, 1, 30)
  val () = $A.set<int>(arr, 2, 20)
  val () = $A.set<int>(arr, 3, 40)
  val () = sort_with<int>(arr, 4,
    lam (a: int, b: int): int =<cloref1> b - a)
  val () = $A.free<int>(arr)
in () end

fn _test_sort_single(): void = let
  val arr = $A.alloc<int>(1)
  val () = $A.set<int>(arr, 0, 42)
  val () = sort_int(arr, 1)
  val () = $A.free<int>(arr)
in () end

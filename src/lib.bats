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
      val j1 = g1ofg0(j)
    in
      if j1 >= 0 then
        if j1 < len then let
          val cur = $A.get<a>(arr, j1)
          val c = cmp(cur, key)
        in
          if c > 0 then let
            val j1p1 = j1 + 1
          in
            if j1p1 < len then let
              val () = $A.set<a>(arr, j1p1, cur)
            in
              loop_j(arr, j - 1, key, len)
            end
            else ()
          end
          else let
            val jp1 = j + 1
            val jp1g = g1ofg0(jp1)
          in
            if jp1g >= 0 then
              if jp1g < len then
                $A.set<a>(arr, jp1g, key)
              else ()
            else ()
          end
        end
        else ()
      else ()
    end
    else let
      (* j = -1, place key at position 0 *)
      val jp1 = j + 1
      val jp1g = g1ofg0(jp1)
    in
      if jp1g >= 0 then
        if jp1g < len then
          $A.set<a>(arr, jp1g, key)
        else ()
      else ()
    end

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

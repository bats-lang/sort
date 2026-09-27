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
sort_by
  {l:agz}{n:pos}
  (arr: !$A.arr(a, l, n), len: int n, cmp: (a, a) -<fun1> int)
  : void

(* Sorts by cmp, a plain function given env, what it needs (the values
   a closure would capture): nothing is allocated, where a closure would
   be allocated by every caller and never freed (there is no GC) *)
#pub fun{a:t@ype}{e:t@ype}
sort_env
  {l:agz}{n:pos}
  (arr: !$A.arr(a, l, n), len: int n, env: e, cmp: (e, a, a) -<fun1> int)
  : void

(* ============================================================
   Implementation -- insertion sort with a comparator and its
   environment.
   ============================================================ *)

implement{a}{e}
sort_env{l}{n}(arr, len, env, cmp) = let
  (*
   * Inner loop: shift elements right while arr[j] > key.
   * j ranges from n-2 down to -1, so j + 1 < n: both j (once j >= 0)
   * and j + 1 are proven indices.
   * Metric: j + 1 (always non-negative, decreases each step).
   *)
  fun loop_j
    {j:int | j >= ~1; j + 1 < n} .<j + 1>.
    (arr: !$A.arr(a, l, n), j: int j, key: a, len: int n)
    : void =
    if j >= 0 then let
      val cur = $A.get<a>(arr, j)
      val c = cmp(env, cur, key)
    in
      if c > 0 then let
        val () = $A.set<a>(arr, j + 1, cur)
      in loop_j(arr, j - 1, key, len) end
      else
        $A.set<a>(arr, j + 1, key)
    end
    else
      $A.set<a>(arr, j + 1, key)

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

implement{a}
sort_by{l}{n}(arr, len, cmp) =
  sort_env<a><(a, a) -<fun1> int>(arr, len, cmp,
    lam (f: (a, a) -<fun1> int, x: a, y: a): int =<fun1> f(x, y))

(* ============================================================
   Implementation -- sort_int via sort_by
   ============================================================ *)

(* Three-way comparison: a - b would overflow for large differences
   (e.g. the minimum int against any positive value). A plain
   function, not a closure: a closure made on each call would be
   allocated and never freed. *)
fn _cmp_int (a: int, b: int):<fun1> int =
  if a < b then ~1 else if a > b then 1 else 0

implement
sort_int{l}{n}(arr, len) = sort_by<int>(arr, len, _cmp_int)

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

fn _test_sort_env(): void = let
  val arr = $A.alloc<int>(4)
  val () = $A.set<int>(arr, 0, 10)
  val () = $A.set<int>(arr, 1, 30)
  val () = $A.set<int>(arr, 2, 20)
  val () = $A.set<int>(arr, 3, 40)
  val () = sort_env<int><int>(arr, 4, ~1,
    lam (s: int, a: int, b: int): int =<fun1>
      if a < b then ~s else if a > b then s else 0)
  val () = $A.free<int>(arr)
in () end

fn _test_sort_single(): void = let
  val arr = $A.alloc<int>(1)
  val () = $A.set<int>(arr, 0, 42)
  val () = sort_int(arr, 1)
  val () = $A.free<int>(arr)
in () end

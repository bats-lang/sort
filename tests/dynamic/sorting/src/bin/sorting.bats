#include "share/atspre_staload.hats"
#use array as A
#use sort as S

(* Sorts 5-element arrays and checks the exact result.
   Exits 1 on any mismatch. *)
fn fill {l:agz} (arr: !$A.arr(int, l, 5), a: int, b: int, c: int, d: int, e: int): void = let
  val () = $A.set<int>(arr, 0, a) val () = $A.set<int>(arr, 1, b)
  val () = $A.set<int>(arr, 2, c) val () = $A.set<int>(arr, 3, d)
in $A.set<int>(arr, 4, e) end

fn is {l:agz} (name: string, arr: !$A.arr(int, l, 5), a: int, b: int, c: int, d: int, e: int): bool = let
  val ok = $A.get<int>(arr, 0) = a && $A.get<int>(arr, 1) = b && $A.get<int>(arr, 2) = c
        && $A.get<int>(arr, 3) = d && $A.get<int>(arr, 4) = e
  val () = (if ok then () else println! ("FAIL ", name, ": ",
    $A.get<int>(arr, 0), " ", $A.get<int>(arr, 1), " ", $A.get<int>(arr, 2), " ",
    $A.get<int>(arr, 3), " ", $A.get<int>(arr, 4)))
in ok end

implement main0 () = let
  val arr = $A.alloc<int>(5)
  val () = fill(arr, 5, 3, 1, 4, 2) val () = $S.sort_int(arr, 5)
  val r1 = is("shuffled", arr, 1, 2, 3, 4, 5)
  val () = fill(arr, 1, 2, 3, 4, 5) val () = $S.sort_int(arr, 5)
  val r2 = is("sorted", arr, 1, 2, 3, 4, 5)
  val () = fill(arr, 5, 4, 3, 2, 1) val () = $S.sort_int(arr, 5)
  val r3 = is("reversed", arr, 1, 2, 3, 4, 5)
  val () = fill(arr, 2, 1, 2, 1, 2) val () = $S.sort_int(arr, 5)
  val r4 = is("duplicates", arr, 1, 1, 2, 2, 2)
  (* Differences here overflow int, so a - b would misorder them. *)
  val () = fill(arr, 2147483647, ~2147483647 - 1, 0, ~5, 7) val () = $S.sort_int(arr, 5)
  val r7 = is("extremes", arr, ~2147483647 - 1, ~5, 0, 7, 2147483647)
  val () = fill(arr, 10, 30, 20, 50, 40)
  val () = $S.sort_with<int>(arr, 5, lam (a: int, b: int): int =<cloref1> b - a)
  val r5 = is("descending", arr, 50, 40, 30, 20, 10)
  val () = $A.free<int>(arr)
  val one = $A.alloc<int>(1)
  val () = $A.set<int>(one, 0, 42) val () = $S.sort_int(one, 1)
  val r6 = ($A.get<int>(one, 0) = 42)
  val () = (if r6 then () else println! ("FAIL single"))
  val () = $A.free<int>(one)
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 then println! ("sorting: all cases pass")
  else exit_void(1)
end

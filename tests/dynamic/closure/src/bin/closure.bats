#include "share/atspre_staload.hats"
#use array as A
#use sort as S

(* sort_with sorts by a closure. It has no no-leaks file: sort_with
   takes a (a, a) -<cloref1> int, which the caller allocates and can
   never free, so the closure made here is lost (sort_with itself
   allocates nothing; sort_by takes a plain function and leaks nothing).
   Exits 1 on a mismatch. *)
implement main0 () = let
  val arr = $A.alloc<int>(5)
  val () = $A.set<int>(arr, 0, 10) val () = $A.set<int>(arr, 1, 30)
  val () = $A.set<int>(arr, 2, 20) val () = $A.set<int>(arr, 3, 50)
  val () = $A.set<int>(arr, 4, 40)
  val () = $S.sort_with<int>(arr, 5, lam (a: int, b: int): int =<cloref1> b - a)
  val a0 = $A.get<int>(arr, 0) val a1 = $A.get<int>(arr, 1) val a2 = $A.get<int>(arr, 2)
  val a3 = $A.get<int>(arr, 3) val a4 = $A.get<int>(arr, 4)
  val ok = a0 = 50 && a1 = 40 && a2 = 30 && a3 = 20 && a4 = 10
  val () = $A.free<int>(arr)
in
  if ok then println! ("closure: descending passes")
  else (println! ("FAIL descending"); exit_void(1))
end

#include "share/atspre_staload.hats"
#use array as A
#use sort as S

(* sort_env sorts by a plain function given an environment (here the
   direction, ~1 for descending); nothing is allocated for it. Exits 1 on
   a mismatch. *)
fn _cmp (s: int, a: int, b: int):<fun1> int =
  if a < b then ~s else if a > b then s else 0

implement main0 () = let
  val arr = $A.alloc<int>(5)
  val () = $A.set<int>(arr, 0, 10) val () = $A.set<int>(arr, 1, 30)
  val () = $A.set<int>(arr, 2, 20) val () = $A.set<int>(arr, 3, 50)
  val () = $A.set<int>(arr, 4, 40)
  val () = $S.sort_env<int><int>(arr, 5, ~1, _cmp)
  val a0 = $A.get<int>(arr, 0) val a1 = $A.get<int>(arr, 1) val a2 = $A.get<int>(arr, 2)
  val a3 = $A.get<int>(arr, 3) val a4 = $A.get<int>(arr, 4)
  val ok = a0 = 50 && a1 = 40 && a2 = 30 && a3 = 20 && a4 = 10
  val () = $A.free<int>(arr)
in
  if ok then println! ("env: descending passes")
  else (println! ("FAIL descending"); exit_void(1))
end

signature TACTIC =
sig
  include REFINEMENT
  type tactic = goal -> subgoals * validation

  val then_ : tactic * tactic -> tactic
  val thenl : tactic * tactic list -> tactic

  (* a backtracking tactical to try two tactics *)
  val orelse_ : tactic * tactic -> tactic

  (* the identity tactic *)
  val id : tactic

  (* the tactic that always fails *)
  val fail : tactic

  (* ensure that a tactic completes the goal *)
  val complete : tactic -> tactic

  (* for when you want to print out the current goal; behaves like the identity tactic *)
  val hole : string -> tactic
end
signature TACTIC =
sig
  include REFINEMENT

  type 'a seq
  type tactic = goal -> state seq

  val then_ : tactic * tactic -> tactic
  val thenl : tactic * tactic list -> tactic
  val orelse_ : tactic * tactic -> tactic
end
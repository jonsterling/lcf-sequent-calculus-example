signature REFINER = 
sig
  structure K : KERNEL

  (* The "refiner" implements backward/bottom-up inference, which is best captured by the sequent calculus. *)
  type goal = K.S.sequent
  type subgoals = goal list
  type validation = K.proof list -> K.proof
  type state = subgoals * validation
  type tactic = goal -> state

  type hyp = K.S.hyp

  val trueR : tactic
  val falseL : hyp -> tactic

  val conjR : tactic
  val conjL1 : hyp -> tactic
  val conjL2 : hyp -> tactic

  val disjR1 : tactic
  val disjR2 : tactic
  val disjL : hyp -> tactic

  val implR : tactic
  val implL : hyp -> tactic
end
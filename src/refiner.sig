signature REFINER = 
sig
  structure K : KERNEL

  include REFINEMENT
    where type goal = K.S.sequent
    where type proof = K.proof

  (* The "refiner" implements backward/bottom-up inference, which is best captured by the sequent calculus.
     Refinement rules consist in a decomposition of a goal into subgoals, together with a "validation",
     which is a procedure that combines evidence of the subgoals into a forward proof of the main 
     goal. *)

  type hyp = K.S.hyp

  val trueR : rule
  val falseL : hyp -> rule

  val conjR : rule
  val conjL1 : hyp -> rule
  val conjL2 : hyp -> rule

  val disjR1 : rule
  val disjR2 : rule
  val disjL : hyp -> rule

  val implR : rule
  val implL : hyp -> rule
end
signature REFINEMENT =
sig
  type goal
  type proof

  val prettyGoal : goal -> string

  type subgoals = goal list
  type validation = proof list -> proof

  type rule = goal -> subgoals * validation
end
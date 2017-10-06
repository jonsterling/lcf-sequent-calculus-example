signature REFINEMENT =
sig
  type goal
  type proof

  type subgoals = goal list
  type validation = proof list -> proof
  type state = subgoals * validation

  type rule = goal -> state
end
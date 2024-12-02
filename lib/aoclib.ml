module type Types = sig
  type input
  type output

  val pp_input : Format.formatter -> input -> unit
  val pp_output : Format.formatter -> output -> unit
end

module type Parsing = sig
  type input

  val input : input CCParse.t
end

module type Solving = sig
  type input
  type output

  val part1 : input -> output
  val part2 : input -> output
end

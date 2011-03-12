

module type STREAM = sig

  type 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t

  val put: string -> unit t

end

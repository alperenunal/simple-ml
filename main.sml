fun doIt (args: string list) =
  let
    val file = hd args
    val () =
      Interpreter.interpret file
      handle e =>
        let
          val history = MLton.Exn.history e
          val () = List.app (fn s => print (s ^ "\n")) history
        in
          print (exnMessage e ^ "\n")
        end
  in
    ()
  end

val () = doIt (CommandLine.arguments ())

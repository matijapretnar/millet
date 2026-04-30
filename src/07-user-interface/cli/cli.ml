open Utils
module Unix = Unix
module Ast = Language.Ast
module Backend = CliInterpreter
module Loader = Loader.Loader (Backend)
module Comp = Compiler

type config = {
  filenames : string list;
  use_stdlib : bool;
  compile : bool;
  time : bool;
  benchmark : bool;
}

let parse_args_to_config () =
  let filenames = ref []
  and use_stdlib = ref true
  and compile = ref false
  and time = ref false
  and benchmark = ref false in
  let usage = "Run Millet as '" ^ Sys.argv.(0) ^ " [filename.mlt] ...'"
  and anonymous filename = filenames := filename :: !filenames
  and options =
    Arg.align
      [
        ( "--no-stdlib",
          Arg.Clear use_stdlib,
          " Do not load the standard library" );
        ("--compile", Arg.Set compile, " Compile programs");
        ("--time", Arg.Set time, "Measure execution time");
        ("--bench", Arg.Set benchmark, "Benchmark the program");
      ]
  in
  Arg.parse options anonymous usage;
  {
    filenames = List.rev !filenames;
    use_stdlib = !use_stdlib;
    compile = !compile;
    time = !time;
    benchmark = !benchmark;
  }

let rec run (state : Backend.run_state) dry =
  Backend.view_run_state state dry;
  match Backend.steps state with
  | [] -> ()
  | steps ->
      let i = Random.int (List.length steps) in
      let step = List.nth steps i in
      let state' = step.next_state () in
      run state' dry

let time_function should_time f =
  if should_time then (
    let start = Unix.gettimeofday () in
    f ();
    let stop = Unix.gettimeofday () in
    let elapsed = stop -. start in
    Printf.printf "Execution time: %.6f seconds\n" elapsed)
  else f ()

let mean arr =
  let sum = Array.fold_left ( +. ) 0. arr in
  sum /. float_of_int (Array.length arr)

let standard_deviation arr =
  let avg = mean arr in
  let variance =
    Array.fold_left (fun acc x -> acc +. ((x -. avg) ** 2.0)) 0. arr
  in
  sqrt (variance /. float_of_int (Array.length arr))

let benchmark trials f =
  let times = Array.make trials 0. in
  let results = Array.make trials None in
  for i = 0 to trials - 1 do
    let start = Unix.gettimeofday () in
    let result = f () in
    let stop = Unix.gettimeofday () in
    times.(i) <- stop -. start;
    results.(i) <- Some result
  done;
  let avg_time = mean times *. 1000. in
  let std_dev = standard_deviation times *. 1000. in
  Printf.printf "Trials: %d\n" trials;
  Printf.printf "Mean execution time: %.4f ms\n" avg_time;
  Printf.printf "Standard deviation: %.4f ms\n" std_dev

let main () =
  let config = parse_args_to_config () in
  try
    Random.self_init ();
    let final_state =
      match config.use_stdlib with
      | true ->
          let state =
            Loader.load_source Loader.initial_state Loader.stdlib_source
          in
          List.fold_left Loader.load_file state config.filenames
      | false ->
          List.fold_left Loader.load_file Loader.initial_state config.filenames
    in
    let tf =
      match config.benchmark with
      | true -> benchmark 100
      | false -> time_function config.time
    in
    tf (fun () ->
        match config.compile with
        | false ->
            let run_state = Backend.run final_state.backend in
            run run_state config.benchmark
        | true ->
            let out_str =
              Compiler.compile_prog final_state.compiler
                final_state.typechecker.type_definitions
            in
            Loader.write_to_file (List.hd config.filenames) out_str)
  with Error.Error error ->
    Error.print error;
    exit 1

let _ = main ()

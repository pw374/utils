(* Output on stdout the contents of the last modified file in the given directory *)
(* Licence: ISC *)
(* (c) 2014 Philippe Wang <philippe.wang@gmail.com> *)
open Printf

open LastModifiedFileInDir

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let f = Sys.argv.(i) in
    let input = open_in (find_latest_file f) in
      (try while true do
          output_char stdout (input_char input)
      done with End_of_file -> flush stdout)
  done

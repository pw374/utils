(* Output on stdout the contents of the last modified file *)
(* Licence: ISC *)
(* (c) 2014 Philippe Wang <philippe.wang@gmail.com> *)
open Printf

let older f1 f2 =
  let open Unix in
  (lstat f1).st_mtime > (lstat f2).st_mtime

let _ =
  let oldest = ref Sys.argv.(1) in
  let oldest_mtime = Unix.(ref (lstat !oldest).st_mtime) in
  let rec loop i =
    if i = Array.length Sys.argv - 1 then
      let input = open_in !oldest in
      try
        while true do
          output_char stdout (input_char input)
        done
      with End_of_file -> flush stdout
    else
      let open Unix in
      let f = Sys.argv.(i) in
      let sf = lstat f in
      (match sf.st_kind with
       | S_REG when sf.st_mtime > !oldest_mtime ->
         oldest := f;
         oldest_mtime := sf.st_mtime
       | S_DIR ->
         (match LastModifiedFileInDir.find_latest_file f with
          | None -> ()
          | Some f ->
            if older f !oldest then
              (oldest := f;
               oldest_mtime := (lstat f).st_mtime)
            else
              ())
       | _ -> ());
      loop (i+1)
  in
  loop 1

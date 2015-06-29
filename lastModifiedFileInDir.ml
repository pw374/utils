(* Last modified file in directory *)
(* Licence: ISC *)
(* (c) 2014 Philippe Wang <philippe.wang@gmail.com> *)
open Printf

let rec find_latest_file (dir:string) : string option =
  let open Unix in
  if (lstat dir).st_kind <> S_DIR then
    Some dir
  else
    let opendir d : Unix.dir_handle option = (**)
      try Some(opendir d) with _ -> None
    in
    let readdir : Unix.dir_handle option -> string option = function
      | None ->
        (* In this case, the dir was never opened, so no need to close it. *)
        None
      | Some d ->
        (* All open dirs are eventually closed. *)
        try Some(readdir d) with End_of_file -> closedir d; None in
    let stat_dummy : Unix.stats =
      { (lstat Sys.argv.(0)) with st_kind = S_BLK; st_mtime = 0.0; } in
    let d = opendir dir in
    let rec loop (((latest : string option), (latest_date : float)) as r) =
      match readdir d with
      | Some ("." | "..") -> (* just skip them *)
        loop (latest, latest_date)
      | None -> (* no more entries in directory [d] *)
        latest
      | Some x ->
        (* We have an entry in directory [d],
           and it will be explored by [aux]. *)
        let rec aux : string option -> (string option * float) = function
          (* In this function, recursion is used only to avoid code duplication,
             and not to iterate an unknown number of times. *)
          | None -> r
          | Some x ->
            let sx = try lstat x with _ -> stat_dummy in
            match sx.st_kind with
            | S_REG when sx.st_mtime > latest_date ->
              Some x, sx.st_mtime
            | S_DIR ->
              (* The result of [find_latest_file] is either [None]
                 or [Some f] where [f] is a regular file, so this call to [aux]
                 will not generate any additional recursive call to [aux]. *)
              aux (find_latest_file x)
            | _ ->
              (* Not a regular file nor a directory, and we don't follow
                 symbolic links (because it could lead to infinite loops).
                 It could be proposed as an option, some day. *)
              r
        in
        loop (aux (Some (Filename.concat dir x)))
    in
    loop (None, 0.0)


(* let _ = *)
(*   match find_latest_file Sys.argv.(1) with *)
(*   | Some f -> print_endline f *)
(*   | None -> () *)

module type Readable = sig
  type t
  type arg
  val begin_read : arg -> t
  val end_read : t -> unit
  val at_end : t -> bool
  val read_line : t -> (t * string)
  end


  module ReadableString
  (* : Readable with type arg = string *) = struct
  type t = string list (* 1P types *)
  type arg = string
  let begin_read s = String.split_on_char '\n' s (* 1P *)
  let end_read l = () (* 0.5P *)
  let at_end l = l = [] (* 0.5P *)
  let read_line l = List.tl l, List.hd l (* 1P *)
  end


  module ReadableFile
  (* : Readable with type arg = string *) = struct
  type t = in_channel * string option (* 1P types *)
  type arg = string
  let at_end (_,s) = s = None (* 1P *)
  let read_line ((c,Some s) : t) =
  (try c, Some (input_line c) (* 1P input_line *)
  with End_of_file -> c, None), s (* 1P EoF handling *)
  let begin_read n =
  let file = open_in n in (* 1P open_in *)
  fst (read_line (file, Some "")) (* 1P read_line *)
  let end_read (c,_) = close_in c (* 1P close_in *)
  end

  module Reader (R : Readable) (* 1P functor *) = struct
    include R (* 1P *)
    let read_all r =
      let rec impl r =
      if at_end r then r,[] (* 1P term. condition *)
      else
      let r,line = R.read_line r in (* 1P R.read_line *)
      let r,rest = impl r in
      r,line::rest (* 1P rec call *)
      in
      let r,content = impl r in (* 1P helper fun + call *)
      r, String.concat "\n" content (* 1P merge stuff *)
      end


      module RemoteReader (R : Readable) (* 1P functor *)
      (* : Readable with type arg = R.arg *) = struct
      type msg = End | AtEndReq | AtEndAnswer of bool
      | ReadReq | ReadAnswer of string (* 2P types *)
      type t = msg channel
      type arg = R.arg
      let begin_read a =
      let c = new_channel () in (* 1P new_channel *)
      let task a =
      let r = R.begin_read a in (* 1P R.begin_read *)
      let rec loop r =
      match sync (receive c) with (* 1P *)
      | End -> R.end_read r (* 1P *)
      | AtEndReq -> let e = R.at_end r in (* 1P *)
      sync (send c (AtEndAnswer e)); (* 1P *)
      loop r (* 0.5P *)
      | ReadReq -> let r, s = R.read_line r in (* 1P *)
      sync (send c (ReadAnswer s)); (* 1P *)
      loop r (* 0.5P *)
      in
      loop r
      in
      let _ = create task a in (* 1P *)
      c
      let end_read c =
      sync (send c End) (* 1P *)
      let at_end c =
      sync (send c AtEndReq); (* 1P *)
      match sync (receive c) with AtEndAnswer b -> b (* 1P *)
      let read_line c =
      sync (send c ReadReq); (* 1P *)
      match sync (receive c) with ReadAnswer s -> c,s (* 1P *)
      end


let rs = ReadableString.begin_read "A multiline\ntext"
let e = ReadableString.at_end rs (* e = false *)
let rs,l1 = ReadableString.read_line rs (* l1 = "A multiline" *)
let rs,l2 = ReadableString.read_line rs (* l2 = "text" *)
let e = ReadableString.at_end rs (* e = true *)
let _ = ReadableString.end_read rs
(* similarly for ReadableFile and RemoteReader(ReadableString) *)
(* module R = Reader(ReadableString)
let r = R.begin_read "A multiline\ntext"
let r,t = R.read_all r (* t = "A multiline\ntext" *)
let _ = R.end_read r *)
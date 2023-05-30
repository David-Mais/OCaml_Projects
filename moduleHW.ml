module type Readable = sig
  type t
  type arg
  val begin_read : arg -> t
  val end_read : t -> unit
  val at_end : t -> bool
  val read_line : t -> (t * string)
end;;

module ReadableString : Readable = struct
  type t = {
    string_data: string;
    mutable position: int;
  }

  type arg = string

  let begin_read str =
    { string_data = str; position = 0 }

  let end_read _ =
    ()

  let at_end state =
    state.position >= String.length state.string_data

  let read_line state =
    let remaining_length = String.length state.string_data - state.position in
    if remaining_length <= 0 then
      state, ""
    else
      let line_end = min (state.position + remaining_length) (String.index_from state.string_data state.position '\n') in
      let line = String.sub state.string_data state.position (line_end - state.position) in
      state.position <- line_end + 1;
      state, line
end


module ReadableFile : Readable = struct
  type t = in_channel

  type arg = string

  let begin_read file_name =
    let channel = open_in file_name in
    channel

  let end_read channel =
    close_in channel

  let at_end channel =
    try
      input_line channel |> ignore;
      false
    with
      End_of_file -> true

  let read_line channel =
    let line = input_line channel in
    channel, line
end

module Reader (R : Readable) : sig
  include Readable
  val read_all : t -> (t * string)
end = struct
  include R

  let rec read_all_helper acc state =
    if at_end state then
      state, String.concat "\n" (List.rev acc)
    else
      let state', line = read_line state in
      read_all_helper (line :: acc) state'

  let read_all state =
    read_all_helper [] state
end
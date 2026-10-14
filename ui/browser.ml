open! Core
open! Async_kernel
open Js_of_ocaml

let now_ms () = Js.to_float (new%js Js.date_now)##getTime

let random_id () =
  let bytes : Typed_array.uint8Array Js.t = new%js Typed_array.uint8Array 16 in
  let crypto = Js.Unsafe.global##.crypto in
  ignore (Js.Unsafe.meth_call crypto "getRandomValues" [| Js.Unsafe.inject bytes |] : Js.Unsafe.any);
  String.concat
    (List.init 16 ~f:(fun i -> sprintf "%02x" (Typed_array.unsafe_get bytes i)))
;;

let download ~file_name ~mime contents =
  let blob = File.blob_from_string ~contentType:mime contents in
  let url = Dom_html.window##._URL##createObjectURL blob in
  let a = Dom_html.createA Dom_html.document in
  a##.href := url;
  a##setAttribute (Js.string "download") (Js.string file_name);
  Dom.appendChild Dom_html.document##.body a;
  a##click;
  Dom.removeChild Dom_html.document##.body a;
  Dom_html.window##._URL##revokeObjectURL url
;;

let scripts : unit Or_error.t Ivar.t String.Table.t = String.Table.create ()

let load_script url =
  match Hashtbl.find scripts url with
  | Some ivar -> Ivar.read ivar
  | None ->
    let ivar = Ivar.create () in
    Hashtbl.set scripts ~key:url ~data:ivar;
    let script = Dom_html.createScript Dom_html.document in
    script##.src := Js.string url;
    script##.async := Js._true;
    let listen event f =
      ignore
        (Dom_html.addEventListener
           script
           event
           (Dom_html.handler (fun _ ->
              f ();
              Js._true))
           Js._false
         : Dom_html.event_listener_id)
    in
    listen Dom_html.Event.load (fun () -> Ivar.fill_if_empty ivar (Ok ()));
    listen Dom_html.Event.error (fun () ->
      (* Forget the failure so a later visit can try again. *)
      Hashtbl.remove scripts url;
      Ivar.fill_if_empty ivar (Or_error.error_s [%message "could not load script" url]));
    Dom.appendChild Dom_html.document##.head script;
    Ivar.read ivar
;;

let storage () =
  try Js.Optdef.to_option Dom_html.window##.sessionStorage with
  | _ -> None
;;

let session_get key =
  match storage () with
  | None -> None
  | Some s ->
    (try Js.Opt.to_option (s##getItem (Js.string key)) |> Option.map ~f:Js.to_string with
     | _ -> None)
;;

let session_set key value =
  match storage () with
  | None -> ()
  | Some s ->
    (try
       match value with
       | Some v -> s##setItem (Js.string key) (Js.string v)
       | None -> s##removeItem (Js.string key)
     with
     | _ -> ())
;;

let page_base () =
  let l = Dom_html.window##.location in
  Js.to_string l##.origin ^ Js.to_string l##.pathname
;;

open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Popover = Bonsai_web_ui_popover
module Form = Bonsai_web_ui_form

module Addable_deletable_map = struct
  type t =
    { state : String.Set.t
    ; add : string -> unit Effect.t
    ; remove : string -> unit Effect.t
    }

  let component =
    let module Action = struct
      type t =
        | Add of string
        | Remove of string
    end
    in
    let%sub state, inject =
      Bonsai.state_machine0
        ()
        ~default_model:String.Set.empty
        ~apply_action:(fun _ model -> function
        | Action.Add key -> Set.add model key
        | Remove key -> Set.remove model key)
    in
    let%arr state = state
    and inject = inject in
    { state
    ; add = (fun x -> inject (Add x))
    ; remove = (fun x -> inject (Remove x))
    }
  ;;
end

let dialog_button
  (text : string)
  (on_submit : (string -> unit Effect.t) Value.t)
  : Vdom.Node.t Computation.t
  =
  let%sub theme = View.Theme.current in
  let%sub { Popover.Result.wrap; open_; _ } =
    Popover.component
      ()
      ~close_when_clicked_outside:true
      ~direction:(Value.return Popover.Direction.Right)
      ~alignment:(Value.return Popover.Alignment.Center)
      ~popover:(fun ~close ->
        let%sub form = Form.Elements.Textbox.string () in
        let%sub new_name =
          let%arr form = form in
          Form.value form
        in
        let%arr new_name = new_name
        and theme = theme
        and on_submit = on_submit
        and form = form
        and close = close in
        let on_click =
          match new_name with
          | Error _ -> Effect.Ignore
          | Ok new_name ->
            let%bind.Effect () = on_submit new_name in
            close
        in
        View.hbox
          ~gap:(`Rem 0.5)
          [ Form.view_as_vdom form
          ; View.button
              ~disabled:(Or_error.is_error new_name)
              theme
              ~on_click
              text
          ])
  in
  let%arr wrap = wrap
  and open_ = open_
  and theme = theme in
  wrap (View.button theme ~on_click:open_ text)
;;

let file ~name =
  let%arr name = name in
  View.text name
;;

module Style =
  [%css
  stylesheet {|
.directory {
  padding-left: 2rem;
}
          |}]

let rec directory ~(name : string Value.t) : Vdom.Node.t Computation.t =
  Bonsai.lazy_
    (lazy
      (let%sub collapsed, toggle_collapsed =
         Bonsai.toggle ~default_model:false
       in
       let%sub { state = files; add = add_file; remove = remove_file } =
         Addable_deletable_map.component
       in
       let%sub { state = subdirectories
               ; add = add_subdirectory
               ; remove = remove_subdirectory
               }
         =
         Addable_deletable_map.component
       in
       let%sub collapsed_icon =
         match%arr collapsed with true -> "[+]" | false -> "[-]"
       in
       let%sub theme = View.Theme.current in
       let%sub subdirectories =
         Bonsai.assoc_set
           (module String)
           subdirectories
           ~f:(fun name ->
             let%sub directory = directory ~name in
             let%arr directory = directory
             and theme = theme
             and remove_subdirectory = remove_subdirectory
             and name = name in
             View.hbox
               ~gap:(`Rem 0.5)
               [ directory
               ; View.button
                   ~intent:Error
                   theme
                   "Delete directory"
                   ~on_click:(remove_subdirectory name)
               ])
       in
       let%sub files =
         Bonsai.assoc_set
           (module String)
           files
           ~f:(fun name ->
             let%sub file = file ~name in
             let%arr file = file
             and theme = theme
             and remove_file = remove_file
             and name = name in
             View.hbox
               ~gap:(`Rem 0.5)
               [ file
               ; View.button
                   ~intent:Error
                   theme
                   "Delete file"
                   ~on_click:(remove_file name)
               ])
       in
       let%sub add_file = dialog_button "Create file" add_file in
       let%sub add_subdirectory =
         dialog_button "Create directory" add_subdirectory
       in
       let%arr collapsed_icon = collapsed_icon
       and toggle_collapsed = toggle_collapsed
       and name = name
       and theme = theme
       and add_file = add_file
       and add_subdirectory = add_subdirectory
       and subdirectories = subdirectories
       and files = files in
       View.vbox
         ~attrs:[ Style.directory ]
         [ View.hbox
             ~gap:(`Rem 0.5)
             [ View.button theme ~on_click:toggle_collapsed collapsed_icon
             ; Vdom.Node.text name
             ; add_file
             ; add_subdirectory
             ]
         ; View.vbox (Map.data subdirectories)
         ; View.vbox (Map.data files)
         ]))
;;

let app =
  View.Theme.set_for_app
    (Value.return (Kado.theme ~style:Dark ~version:Bleeding ()))
  @@
  let%sub directory = directory ~name:(Value.return "/") in
  let%arr directory = directory in
  View.vbox
    [ Vdom.Node.h3 [ Vdom.Node.text "Bonsai Filesystem Demo" ]; directory ]
;;

open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open Js_of_ocaml
open Async_kernel
open! Bonsai.Let_syntax

let () = Random.self_init ()

module Multiplayer = struct
  type document_fetch = Game_state.t

  let firebase_base_url = "https://crazy-eights-85663-default-rtdb.firebaseio.com"

  let escape_for_json str =
    let buf = Buffer.create (String.length str) in
    String.iter str ~f:(fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c
    );
    Buffer.contents buf

  let unescape_from_json str =
    let len = String.length str in
    let buf = Buffer.create len in
    let rec aux i =
      if i >= len then ()
      else
        match str.[i] with
        | '\\' when i + 1 < len -> (
          match str.[i + 1] with
          | '"' -> Buffer.add_char buf '"'; aux (i + 2)
          | '\\' -> Buffer.add_char buf '\\'; aux (i + 2)
          | 'n' -> Buffer.add_char buf '\n'; aux (i + 2)
          | 'r' -> Buffer.add_char buf '\r'; aux (i + 2)
          | 't' -> Buffer.add_char buf '\t'; aux (i + 2)
          | _ -> Buffer.add_char buf str.[i]; aux (i + 1)
        )
        | c -> Buffer.add_char buf c; aux (i + 1)
    in
    aux 0;
    Buffer.contents buf

  let game_state_to_json (state : Game_state.t) : string =
    let sexp_string = state |> Game_state.sexp_of_t |> Sexp.to_string in
    "{\"data\":\"" ^ (escape_for_json sexp_string) ^ "\"}"

  let game_state_of_json (json : string) : Game_state.t =
    try
      let pattern = Str.regexp "\"data\":\"\\(\\(.\\|\n\\)*\\)\"" in
      let _ = Str.search_forward pattern json 0 in
      let escaped_data = Str.matched_group 1 json in
      let sexp_string = unescape_from_json escaped_data in
      sexp_string |> Sexp.of_string |> Game_state.t_of_sexp
    with _ -> failwith "Failed to parse JSON"

  let create_room ~(room_id : string) ~(player_id : string) : document_fetch Deferred.t =
    let initial_state = Game_state.create_random_initial_state () in
    let (_player_id : string) = player_id in
    let body = game_state_to_json initial_state in
    let url = firebase_base_url ^ "/rooms/" ^ room_id ^ ".json" in
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PUT") (Js.string url) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let status = xhr##.status in
        if status >= 200 && status < 300 then
          Ivar.fill ivar initial_state
        else
          Ivar.fill ivar (failwith (Printf.sprintf "Failed to create room: %d" status))
      | _ -> ()
    );
    ignore (xhr##send (Js.some (Js.string body)));
    Ivar.read ivar

  let join_room ~(room_id : string) ~(player_id : string) : document_fetch Deferred.t =
    let url = firebase_base_url ^ "/rooms/" ^ room_id ^ ".json" in
    let (_player_id : string) = player_id in
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "GET") (Js.string url) Js._true;
    xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let status = xhr##.status in
        if status >= 200 && status < 300 then (
          let response_text =
            Js.Opt.get xhr##.responseText (fun () -> Js.string "{}") |> Js.to_string
          in
          let state = game_state_of_json response_text in
          Ivar.fill ivar state
        ) else if status = 404 then
          Ivar.fill ivar (failwith "Room not found")
        else
          Ivar.fill ivar (failwith (Printf.sprintf "Failed to join room: %d" status))
      | _ -> ()
    );
    ignore (xhr##send Js.null);
    Ivar.read ivar

  let send_move ~(room_id : string) ~(player_id : string) (new_state : Game_state.t) : document_fetch Deferred.t =
    let url = firebase_base_url ^ "/rooms/" ^ room_id ^ ".json" in
    let (_player_id : string) = player_id in
    let body = game_state_to_json new_state in
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PUT") (Js.string url) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let status = xhr##.status in
        if status >= 200 && status < 300 then
          Ivar.fill ivar new_state
        else
          Ivar.fill ivar (failwith (Printf.sprintf "Failed to send move: %d" status))
      | _ -> ()
    );
    ignore (xhr##send (Js.some (Js.string body)));
    Ivar.read ivar

  let poll_room ~(room_id : string) : document_fetch Deferred.t =
    let url = firebase_base_url ^ "/rooms/" ^ room_id ^ ".json" in
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "GET") (Js.string url) Js._true;
    xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
        let status = xhr##.status in
        if status >= 200 && status < 300 then (
          let response_text =
            Js.Opt.get xhr##.responseText (fun () -> Js.string "{}") |> Js.to_string
          in
          let state = game_state_of_json response_text in
          Ivar.fill ivar state
        ) else
          Ivar.fill ivar (failwith (Printf.sprintf "Failed to poll room: %d" status))
      | _ -> ()
    );
    ignore (xhr##send Js.null);
    Ivar.read ivar
end


module Card_selection = struct
  type t = Card.t list
  [@@deriving sexp, equal]
end

let initial_selection : Card_selection.t = []

module Draw_state = struct
  type t =
    | No_draw
    | Just_drawn of Card.t
  [@@deriving sexp, equal]
end

let initial_draw_state = Draw_state.No_draw

let render_card ~card ~clickable ~on_click ~is_selected ~player =
  let img_src =
    match card with
    | `Face_down ->
      "hw5_html_css/cards/1B.svg"
    | `Card card_obj ->
      let rank =
        match card_obj.Card.rank with
        | Rank.Two -> "2" | Rank.Three -> "3" | Rank.Four -> "4" | Rank.Five -> "5"
        | Rank.Six -> "6" | Rank.Seven -> "7" | Rank.Eight -> "8" | Rank.Nine -> "9"
        | Rank.Ten -> "T" | Rank.Jack -> "J" | Rank.Queen -> "Q" | Rank.King -> "K" | Rank.Ace -> "A"
      in
      let suit =
        match card_obj.Card.suit with
        | Suit.Hearts -> "H" | Suit.Diamonds -> "D" | Suit.Clubs -> "C" | Suit.Spades -> "S"
      in
      "hw5_html_css/cards/" ^ rank ^ suit ^ ".svg"
  in

  let player_class =
    match player with
    | Player_kind.P1 -> "player1-card"
    | Player_kind.P2 -> "player2-card"
  in

  let attrs =
    let base = [
      Vdom.Attr.class_ "player-card hoverable z-depth-2";
      Vdom.Attr.class_ player_class;
    ] in
    let base = if is_selected then base @ [ Vdom.Attr.class_ "selected gray lighten-4" ] else base in
    if clickable
    then base @ [ Vdom.Attr.on_click (fun _ -> on_click card) ]
    else base
  in

  Vdom.Node.img ~attrs:(attrs @ [ Vdom.Attr.src img_src ]) ()
;;

let render_hand ~cards ~on_card_click ~selected_cards ~player =
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand" ]
    (List.mapi cards ~f:(fun _ c -> 
      match c with
      | `Card card_obj ->
        let is_selected = List.mem selected_cards card_obj ~equal:Card.equal in
        render_card ~card:c ~clickable:true ~on_click:on_card_click ~is_selected ~player
      | `Face_down ->
        render_card ~card:c ~clickable:false ~on_click:(fun _ -> Vdom.Effect.Ignore) ~is_selected:false ~player))
;;

let render_piles ~deck ~discard ~on_draw =
  let make_pile img_src clickable =
    let attrs = [
      Vdom.Attr.class_ "player-card hoverable z-depth-2 pile";
      Vdom.Attr.src img_src;
    ] in
    let attrs = if clickable then
      attrs @ [ Vdom.Attr.on_click (fun _ -> 
        Vdom.Effect.bind (on_draw ()) ~f:(fun effect -> effect)) ]
    else attrs
    in
    Vdom.Node.img ~attrs ()
  in

  let deck_img =
    if List.is_empty deck then
      make_pile "hw5_html_css/cards/1B.svg" false
    else
      make_pile "hw5_html_css/cards/1B.svg" true
  in

  let discard_img =
    match List.hd discard with
    | None -> make_pile "hw5_html_css/cards/1B.svg" false
    | Some card ->
      let rank =
        match card.Card.rank with
        | Rank.Two -> "2" | Rank.Three -> "3" | Rank.Four -> "4" | Rank.Five -> "5"
        | Rank.Six -> "6" | Rank.Seven -> "7" | Rank.Eight -> "8" | Rank.Nine -> "9"
        | Rank.Ten -> "T" | Rank.Jack -> "J" | Rank.Queen -> "Q" | Rank.King -> "K" | Rank.Ace -> "A"
      in
      let suit =
        match card.Card.suit with
        | Suit.Hearts -> "H" | Suit.Diamonds -> "D" | Suit.Clubs -> "C" | Suit.Spades -> "S"
      in
      let img_src = "hw5_html_css/cards/" ^ rank ^ suit ^ ".svg" in
      make_pile img_src false
  in

  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "piles" ] [ deck_img; discard_img ]
;;

let render_turn ~decision =
  match decision with
  | Decision.Winner winner -> 
    let player_num = match winner with Player_kind.P1 -> 1 | Player_kind.P2 -> 2 in
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "turn-display" ]
      [ Vdom.Node.text (Printf.sprintf "Player %d Wins!" player_num) ]
  | Decision.In_progress { whose_turn; _ } ->
    let player_num = match whose_turn with Player_kind.P1 -> 1 | Player_kind.P2 -> 2 in
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "turn-display" ]
      [ Vdom.Node.text (Printf.sprintf "Player %d's Turn" player_num) ]
;;

let crazy_eights_board 
  ~(game_state : Game_state.t) 
  ~set_game_state 
  ~selected_cards 
  ~set_selected_cards
  ~draw_state
  ~set_draw_state
  ~message
  ~set_message
  ~player_id
  ~room_id
  ~set_room_id
  ~generated_room_id
  ~is_multiplayer
  ~set_is_multiplayer
  ~(player_slot : string)
  ~set_player_slot
  ~multiplayer_state
  ~set_multiplayer_state

  =

  let _ = multiplayer_state in

  let on_card_click card =
    match draw_state with
    | Draw_state.Just_drawn _ -> Vdom.Effect.Ignore
    | Draw_state.No_draw ->
      match card with
      | `Face_down -> set_message "Can't play opponent's card!"
      | `Card card_obj ->
        if List.mem selected_cards card_obj ~equal:Card.equal then
          Vdom.Effect.Many [
            set_message "";
            set_selected_cards []
          ]
        else
          Vdom.Effect.Many [
            set_message "";
            set_selected_cards [ card_obj ]
          ]
  in
  
  let on_play_selected () =
    let is_my_turn =
      match game_state.decision with
      | Decision.Winner _ -> false
      | Decision.In_progress { whose_turn; _ } ->
        (Player_kind.equal whose_turn Player_kind.P1 && String.equal player_slot "P1") ||
        (Player_kind.equal whose_turn Player_kind.P2 && String.equal player_slot "P2")
    in
    if is_multiplayer then
      if not is_my_turn then
        Vdom.Effect.return (set_message "Not your turn!")
      else
        match Game_state.make_move game_state (Move.Play selected_cards) with
        | Core.Result.Ok new_state ->
          Bonsai_web.Effect.of_deferred_fun
            (fun () -> Multiplayer.send_move ~room_id ~player_id new_state) ()
          |> Bonsai_web.Effect.map ~f:(fun _ ->
              Vdom.Effect.Many [
                set_game_state new_state;
                set_selected_cards [];
                set_draw_state Draw_state.No_draw;
                set_message ""
              ])
        | Core.Result.Error _ -> 
          Vdom.Effect.return (set_message "Can't play that card")
    else
      match draw_state with
      | Draw_state.Just_drawn _ -> Vdom.Effect.return Vdom.Effect.Ignore
      | Draw_state.No_draw ->
        if not (List.is_empty selected_cards) then
          match Game_state.make_move game_state (Move.Play selected_cards) with
          | Core.Result.Ok new_state -> 
            Vdom.Effect.return
              (Vdom.Effect.Many [
                set_game_state new_state;
                set_selected_cards [];
                set_draw_state Draw_state.No_draw;
                set_message ""
              ])
          | Core.Result.Error _ -> 
            Vdom.Effect.return (set_message "Can't play that card: it doesn't match the rank or suit!")
        else
          Vdom.Effect.return Vdom.Effect.Ignore
  in

  let on_draw () =
    let is_my_turn =
      match game_state.decision with
      | Decision.Winner _ -> false
      | Decision.In_progress { whose_turn; _ } ->
        (Player_kind.equal whose_turn Player_kind.P1 && String.equal player_slot "P1") ||
        (Player_kind.equal whose_turn Player_kind.P2 && String.equal player_slot "P2")
    in
    if is_multiplayer then
      if not is_my_turn then
        Vdom.Effect.return (set_message "Not your turn!")
      else
        match Game_state.make_move game_state (Move.Draw_and_maybe_play None) with
        | Core.Result.Ok new_state ->
          Bonsai_web.Effect.of_deferred_fun
            (fun () -> Multiplayer.send_move ~room_id ~player_id new_state) ()
          |> Bonsai_web.Effect.map ~f:(fun _ ->
              Vdom.Effect.Many [
                set_game_state new_state;
                set_draw_state Draw_state.No_draw;
                set_message ""
              ])
        | Core.Result.Error _ -> 
          Vdom.Effect.return (set_message "Can't draw, you have playable cards!")
    else
      match draw_state with
      | Draw_state.Just_drawn _ -> Vdom.Effect.return Vdom.Effect.Ignore
      | Draw_state.No_draw ->
        if List.is_empty game_state.deck then
          Vdom.Effect.return (set_message "Deck is empty!")
        else
          match Game_state.make_move game_state (Move.Draw_and_maybe_play None) with
        | Core.Result.Ok new_state ->
          Vdom.Effect.return
            (Vdom.Effect.Many [
              set_game_state new_state;
              set_draw_state Draw_state.No_draw;
              set_message ""
            ])
        | Core.Result.Error _ -> 
          Vdom.Effect.return (set_message "Can't draw, you have playable cards!")
  in

  let current_player =
    match game_state.decision with
    | Decision.Winner winner -> winner
    | Decision.In_progress { whose_turn; _ } -> whose_turn
  in
  
  let display_player_slot =
    if is_multiplayer then
      player_slot
    else
      match current_player with
      | Player_kind.P1 -> "P1"
      | Player_kind.P2 -> "P2"
  in

  let opponent_slot =
    match display_player_slot with
    | "P1" -> "P2"
    | "P2" -> "P1"
    | _ -> "Unknown"
  in

  let opponent_hands, current_hand =
    if is_multiplayer then
      let my_player_kind = 
        if String.equal display_player_slot "P1" then Player_kind.P1 
        else Player_kind.P2 
      in
      
      let opponent_hands =
        List.filter_map game_state.hands ~f:(fun (player, hand) ->
            if Player_kind.equal player my_player_kind
            then None
            else
              let face_down_hand = List.map hand ~f:(fun _ -> `Face_down) in
              Some (
                Vdom.Node.div ~attrs:[] [
                  Vdom.Node.div ~attrs:[] [ Vdom.Node.text ("Opponent Hand: " ^ opponent_slot ^ " ↓") ];
                  render_hand
                    ~cards:face_down_hand
                    ~on_card_click:(fun _ -> Vdom.Effect.Ignore)
                    ~selected_cards:[]
                    ~player
                ]
              ))
      in
      
      let current_hand =
        let hand = List.Assoc.find_exn game_state.hands ~equal:Player_kind.equal my_player_kind in
        let hand_as_variant = List.map hand ~f:(fun card -> `Card card) in
        Vdom.Node.div ~attrs:[]
          [
            Vdom.Node.div ~attrs:[] [ Vdom.Node.text ("Your Hand: " ^ display_player_slot ^ " ↓") ];
            render_hand
              ~cards:hand_as_variant
              ~on_card_click
              ~selected_cards
              ~player:my_player_kind
          ]
      in
      (opponent_hands, current_hand)

    else
      let opponent_hands =
        List.filter_map game_state.hands ~f:(fun (player, hand) ->
            if Player_kind.equal player current_player
            then None
            else
              let face_down_hand = List.map hand ~f:(fun _ -> `Face_down) in
              Some (
                Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand-container" ] [
                  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand-label" ]
                    [ Vdom.Node.text ("Opponent Hand: " ^ opponent_slot ^ " ↓") ];
                  render_hand
                    ~cards:face_down_hand
                    ~on_card_click:(fun _ -> Vdom.Effect.Ignore)
                    ~selected_cards:[]
                    ~player
                ]
              ))
      in

      let current_hand =
        let hand = List.Assoc.find_exn game_state.hands ~equal:Player_kind.equal current_player in
        let hand_as_variant = List.map hand ~f:(fun card -> `Card card) in
        Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand-container" ]
          [
            Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand-label" ]
              [ Vdom.Node.text ("Your Hand: " ^ display_player_slot ^ " ↓") ];
            render_hand
              ~cards:hand_as_variant
              ~on_card_click
              ~selected_cards
              ~player:current_player
          ]
      in
      (opponent_hands, current_hand)
  in  

  let play_button =
    match draw_state with
    | Draw_state.Just_drawn _ -> Vdom.Node.none
    | Draw_state.No_draw ->
      if not (List.is_empty selected_cards) then
        Vdom.Node.button
          ~attrs:[ 
            Vdom.Attr.class_ "btn grey"
            ; Vdom.Attr.on_click (fun _ -> 
              Vdom.Effect.bind (on_play_selected ()) ~f:(fun effect -> effect))
          ]
          [ Vdom.Node.text "Play Chosen Card" ]
      else
        Vdom.Node.none
  in

  let deck = game_state.deck in
  let discard = game_state.discard_pile in

  let rules_section =
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "rules-box" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "note-phone" ]
          [ Vdom.Node.text "Note: If playing on phone, please scroll right and down." ];
        Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "rules-title" ]
          [ Vdom.Node.text "Game Rules" ];
        Vdom.Node.create "p" ~attrs:[]
          [ Vdom.Node.text "• Match rank OR suit of top card" ];
        Vdom.Node.create "p" ~attrs:[]
          [ Vdom.Node.text "• Eights are wild (playable on any suit, suit changes to the 8's suit)" ];
        Vdom.Node.create "p" ~attrs:[]
          [ Vdom.Node.text "• Draw if you can't play" ];
        Vdom.Node.create "p" ~attrs:[]
          [ Vdom.Node.text "• First to empty hand wins!" ] ]
  in

  let message_node =
    if String.is_empty message then Vdom.Node.none
    else
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "toast-message" ]
        [ Vdom.Node.text message ]
  in

  let game_content =
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game" ]
      (List.concat [ 
        opponent_hands;
        [ render_piles ~deck ~discard ~on_draw ];
        [ current_hand ];
        [ play_button ]; 
      ])
  in

  let game_with_message =
    Vdom.Node.div ~attrs:[]
      [ game_content; 
        message_node ]
  in

  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "main-container" ]
    [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "sidebar" ]
        [ Vdom.Node.h1 
            ~attrs:[ Vdom.Attr.class_ "gothic-title" ]
            [ Vdom.Node.text "Crazy Eights" ];
          
          rules_section;
          
          Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "room-box" ]
            [
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "room-title" ]
                [ Vdom.Node.text "Default is 'Pass and Play'. To play with others:" ];
              
              Vdom.Node.div ~attrs:[]
                [ Vdom.Node.text "Join existing room or create new room." ];
          
              Vdom.Node.input
                ~attrs:[
                  Vdom.Attr.class_ "room-input";
                  Vdom.Attr.type_ "text";
                  Vdom.Attr.value room_id;
                  Vdom.Attr.placeholder "Enter room ID to join";
                  Vdom.Attr.on_input (fun _ v -> set_room_id v);
                ]
                ();
              
              Vdom.Node.button
                ~attrs:[
                  Vdom.Attr.class_ "btn grey";
                  Vdom.Attr.on_click (fun _ -> 
                    let%bind.Bonsai_web.Effect state = 
                      Bonsai_web.Effect.of_deferred_fun
                        (fun () -> Multiplayer.join_room ~room_id ~player_id)
                        ()
                    in
                    Vdom.Effect.Many [
                      set_multiplayer_state (Some state);
                      set_game_state state;
                      set_message ("Joined room " ^ room_id);
                      set_is_multiplayer true;
                      set_player_slot "P2";
                    ]
                  )
                ]
                [ Vdom.Node.text "Join Room" ];
              
              Vdom.Node.input
                ~attrs:[
                  Vdom.Attr.class_ "room-input";
                  Vdom.Attr.type_ "text";
                  Vdom.Attr.value generated_room_id;
                ]
                ();
              
              Vdom.Node.button
                ~attrs:[
                  Vdom.Attr.class_ "btn grey";
                  Vdom.Attr.on_click (fun _ ->
                    let%bind.Bonsai_web.Effect state = 
                      Bonsai_web.Effect.of_deferred_fun
                        (fun () -> Multiplayer.create_room ~room_id:generated_room_id ~player_id)
                        ()
                    in
                    Vdom.Effect.Many [
                      set_multiplayer_state (Some state);
                      set_game_state state;
                      set_room_id generated_room_id;
                      set_message ("Created and joined room " ^ generated_room_id);
                      set_is_multiplayer true;
                      set_player_slot "P1";
                    ]
                  )
                ]
                [ Vdom.Node.text "Create Room" ];  
              
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "room-id-box" ]
                [ Vdom.Node.text ("Your Current Room ID: " ^ 
                  (if String.is_empty room_id then "None" else room_id)) ];
            ];
 
          ];

      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game-container" ]
        [ 
          Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game-mode-label" ]
            [ Vdom.Node.text (if is_multiplayer then "Now Playing: Multiplayer" else "Now Playing: Pass and Play") ];
          render_turn ~decision:game_state.decision;
          game_with_message 
        ] ]

;;

let app =
  let initial_state = Game_state.create_random_initial_state () in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%sub selected_cards, set_selected_cards = 
    Bonsai.state ~default_model:initial_selection (module Card_selection)
  in
  let%sub draw_state, set_draw_state = 
    Bonsai.state ~default_model:initial_draw_state (module Draw_state)
  in
  let%sub message, set_message = 
    Bonsai.state (module String) ~default_model:""
  in

  let%sub player_id = Bonsai.const (Random.bits () |> Int.to_string) in
  let%sub room_id, set_room_id = Bonsai.state (module String) ~default_model:"" in

  let%sub generated_room_id = 
    Bonsai.const (Random.int 9000 + 1000 |> string_of_int)
  in
  let%sub is_multiplayer, set_is_multiplayer =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub player_slot, set_player_slot =
    Bonsai.state (module String) ~default_model:"none"
  in
  let%sub multiplayer_state, set_multiplayer_state =
    Bonsai.state (module (struct
      type t = Game_state.t option [@@deriving sexp, equal]
    end)) ~default_model:None
  in

  let%sub () =
    let effect =
      let%map is_multiplayer = is_multiplayer
      and room_id = room_id
      and set_game_state = set_game_state in
      if is_multiplayer && not (String.is_empty room_id) then
        let%bind.Bonsai_web.Effect new_state =
          Bonsai_web.Effect.of_deferred_fun
            (fun () -> Multiplayer.poll_room ~room_id)
            ()
        in
        set_game_state new_state
      else
        Vdom.Effect.Ignore
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Wait_period_after_previous_effect_starts_blocking
      ~trigger_on_activate:false
      (Time_ns.Span.of_sec 2.0)
      effect
  in

  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_cards = selected_cards
  and set_selected_cards = set_selected_cards
  and draw_state = draw_state
  and set_draw_state = set_draw_state
  and message = message
  and set_message = set_message
  and player_id = player_id
  and room_id = room_id
  and set_room_id = set_room_id
  and generated_room_id = generated_room_id
  and is_multiplayer = is_multiplayer
  and set_is_multiplayer = set_is_multiplayer
  and player_slot = player_slot
  and set_player_slot = set_player_slot
  and multiplayer_state = multiplayer_state
  and set_multiplayer_state = set_multiplayer_state

  in
  crazy_eights_board 
    ~game_state ~set_game_state 
    ~selected_cards ~set_selected_cards
    ~draw_state ~set_draw_state
    ~message ~set_message
    ~player_id
    ~room_id ~set_room_id
    ~generated_room_id
    ~is_multiplayer ~set_is_multiplayer
    ~player_slot ~set_player_slot
    ~multiplayer_state ~set_multiplayer_state
;;

let () = Bonsai_web.Start.start app
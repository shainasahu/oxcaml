open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

let () = Random.self_init ()

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
      attrs @ [ Vdom.Attr.on_click (fun _ -> on_draw ()) ]
    else attrs
    in
    Vdom.Node.img ~attrs ()
  in

  (* Deck pile *)
  let deck_img =
    if List.is_empty deck then
      make_pile "hw5_html_css/cards/1B.svg" false
    else
      make_pile "hw5_html_css/cards/1B.svg" true
  in

  (* Discard pile *)
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
  ~player_slot
  ~set_player_slot

  =

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
    match draw_state with
    | Draw_state.Just_drawn _ -> Vdom.Effect.Ignore
    | Draw_state.No_draw ->
      if not (List.is_empty selected_cards) then
        match Game_state.make_move game_state (Move.Play selected_cards) with
        | Core.Result.Ok new_state -> 
          Vdom.Effect.Many [
            set_game_state new_state;
            set_selected_cards [];
            set_draw_state Draw_state.No_draw;
            set_message ""
          ]
        | Core.Result.Error _ -> set_message "Can't play that card: it doesn't match the rank or suit!"
      else
        Vdom.Effect.Ignore
  in

  let on_draw () =
    match draw_state with
    | Draw_state.Just_drawn _ -> Vdom.Effect.Ignore
    | Draw_state.No_draw ->
      if List.is_empty game_state.deck then
        set_message "Deck is empty!"
      else
        match Game_state.make_move game_state (Move.Draw_and_maybe_play None) with
      | Core.Result.Ok new_state ->
        Vdom.Effect.Many [
          set_game_state new_state;
          set_draw_state Draw_state.No_draw;
          set_message ""
        ]
      | Core.Result.Error _ -> 
        set_message "Can't draw, you have playable cards!"
  in

  let current_player =
    match game_state.decision with
    | Decision.Winner winner -> winner
    | Decision.In_progress { whose_turn; _ } -> whose_turn
  in

  
  let player_slot =
    if is_multiplayer then
      (* multiplayer: player_slot is fixed and never overwritten *)
      player_slot
    else
      (* pass-and-play: player_slot always equals whose_turn *)
      match current_player with
      | Player_kind.P1 -> "P1"
      | Player_kind.P2 -> "P2"
  in

  let opponent_slot =
    match player_slot with
    | "P1" -> "P2"
    | "P2" -> "P1"
    | _ -> "Unknown"
  in
  

  let opponent_hands, current_hand =
    if is_multiplayer then
      let opponent_hands =
        List.filter_map game_state.hands ~f:(fun (player, hand) ->
            if Player_kind.equal player current_player
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
        let hand = List.Assoc.find_exn game_state.hands ~equal:Player_kind.equal current_player in
        let hand_as_variant = List.map hand ~f:(fun card -> `Card card) in
        Vdom.Node.div ~attrs:[]
          [
            Vdom.Node.div ~attrs:[] [ Vdom.Node.text ("Your Hand: " ^ player_slot ^ " ↓") ];
            render_hand
              ~cards:hand_as_variant
              ~on_card_click
              ~selected_cards
              ~player:current_player
          ]
      in
      (opponent_hands, current_hand)

    else
      (* Pass and Play *)
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
              [ Vdom.Node.text ("Your Hand: " ^ player_slot ^ " ↓") ];
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
            ; Vdom.Attr.on_click (fun _ -> on_play_selected ()) 
          ]
          [ Vdom.Node.text "Play Chosen Card" ]
      else
        Vdom.Node.none
  in

  let deck = game_state.deck in
  let discard = game_state.discard_pile in

  let rules_section =
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "rules-box" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "rules-title" ]
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
                    Vdom.Effect.Many [
                      set_message ("Joined room " ^ room_id);
                      set_is_multiplayer true;
                      set_player_slot "P2"; (* joiner becomes P2 *)
                    ])
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
                    Vdom.Effect.Many [
                      set_room_id generated_room_id;
                      set_message ("Created and joined room " ^ generated_room_id);
                      set_is_multiplayer true;
                      set_player_slot "P1"; (* creator becomes P1 *)
                    ])
                ]
                [ Vdom.Node.text "Create Room" ];  
              
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "player-id-box" ]
                [ Vdom.Node.text ("Your Player ID: " ^ player_id) ];
              
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "room-id-box" ]
                [ Vdom.Node.text ("Your Current Room ID: " ^ 
                  (if String.is_empty room_id then "None" else room_id)) ];
            ];
 
          ];

      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game-container" ]
        [ 
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
;;

let () = Bonsai_web.Start.start app
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
      Vdom.Attr.class_ "card";
      Vdom.Attr.class_ player_class;
    ] in
    let base = if is_selected then base @ [ Vdom.Attr.class_ "selected" ] else base in
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
  let deck_img =
    if List.is_empty deck then
      Vdom.Node.img ~attrs:[ Vdom.Attr.src "hw5_html_css/cards/1B.svg"; Vdom.Attr.class_ "pile" ] ()
    else
      Vdom.Node.img
        ~attrs:[
          Vdom.Attr.src "hw5_html_css/cards/1B.svg";
          Vdom.Attr.class_ "pile";
          Vdom.Attr.on_click (fun _ -> on_draw ())
        ] ()
  in
  let discard_img =
    match List.hd discard with
    | None -> 
      Vdom.Node.img ~attrs:[ Vdom.Attr.src "hw5_html_css/cards/1B.svg"; Vdom.Attr.class_ "pile" ] ()
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
      Vdom.Node.img ~attrs:[ Vdom.Attr.src img_src; Vdom.Attr.class_ "pile" ] ()
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "piles" ] [ deck_img; discard_img ]
;;
   

let render_turn ~decision =
  match decision with
  | Decision.Winner winner -> 
    let player_num = match winner with Player_kind.P1 -> 1 | Player_kind.P2 -> 2 in
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "winner" ] 
      [ Vdom.Node.text (Printf.sprintf "Player %d wins the game!" player_num) ]
  | Decision.In_progress { whose_turn; _ } ->
    let player_num = match whose_turn with Player_kind.P1 -> 1 | Player_kind.P2 -> 2 in
    Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "turn-indicator" ]
      [ Vdom.Node.text (Printf.sprintf "Player %d's turn" player_num) ]
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

  let opponent_hands =
    List.filter_map game_state.hands ~f:(fun (player, hand) ->
        if Player_kind.equal player current_player
        then None
        else
          let face_down_hand = List.map hand ~f:(fun _ -> `Face_down) in
          Some (render_hand ~cards:face_down_hand ~on_card_click:(fun _ -> Vdom.Effect.Ignore) ~selected_cards:[] ~player))
  in

  let current_hand =
    let hand = List.Assoc.find_exn game_state.hands ~equal:Player_kind.equal current_player in
    let hand_as_variant = List.map hand ~f:(fun card -> `Card card) in
    render_hand ~cards:hand_as_variant ~on_card_click ~selected_cards ~player:current_player
  in

  let drawn_card_ui = Vdom.Node.none in

  let play_button =
    match draw_state with
    | Draw_state.Just_drawn _ -> Vdom.Node.none
    | Draw_state.No_draw ->
      if not (List.is_empty selected_cards) then
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> on_play_selected ()) ]
          [ Vdom.Node.text (Printf.sprintf "Play Chosen Card") ]
      else
        Vdom.Node.none
  in

  let deck = game_state.deck in
  let discard = game_state.discard_pile in

  let message_node =
    if String.is_empty message then Vdom.Node.none
    else
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "message" ]
        [ Vdom.Node.text message ]
  in

  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game" ]
    (Vdom.Node.text "Crazy Eights" ::
      (List.concat [ 
        opponent_hands;
        [ render_piles ~deck ~discard ~on_draw ];
        [ drawn_card_ui ];
        [ current_hand ];
        [ play_button ];
        [ message_node ];
        [ render_turn ~decision:game_state.decision ] 
      ])
    )
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
  let%sub message, set_message =  (* Move this here *)
    Bonsai.state (module String) ~default_model:""
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and selected_cards = selected_cards
  and set_selected_cards = set_selected_cards
  and draw_state = draw_state
  and set_draw_state = set_draw_state
  and message = message
  and set_message = set_message in
  crazy_eights_board 
    ~game_state ~set_game_state 
    ~selected_cards ~set_selected_cards
    ~draw_state ~set_draw_state
    ~message ~set_message
;;

let () = Bonsai_web.Start.start app
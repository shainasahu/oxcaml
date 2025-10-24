open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

let render_card ~card ~clickable ~on_click =
  let card_content, is_face_down =
    match card with
    | `Face_down -> ("", true)
    | `Card card_obj -> 
        let rank_str =
          match card_obj.Card.rank with
          | Rank.Two -> "2" | Rank.Three -> "3" | Rank.Four -> "4" | Rank.Five -> "5"
          | Rank.Six -> "6" | Rank.Seven -> "7" | Rank.Eight -> "8" | Rank.Nine -> "9"
          | Rank.Ten -> "10" | Rank.Jack -> "J" | Rank.Queen -> "Q" | Rank.King -> "K" | Rank.Ace -> "A"
        in
        let suit_str =
          match card_obj.Card.suit with
          | Suit.Hearts -> "♥" | Suit.Diamonds -> "♦" | Suit.Clubs -> "♣" | Suit.Spades -> "♠"
        in
        (rank_str ^ suit_str, false)
  in
  let attrs =
    let base = [ Vdom.Attr.class_ "card" ] in
    let base = if is_face_down then base @ [ Vdom.Attr.class_ "face-down" ] else base in
    if clickable
    then base @ [ Vdom.Attr.on_click (fun _ -> on_click card) ]
    else base
  in
  Vdom.Node.div ~attrs [ Vdom.Node.text card_content ]
;;

let render_hand ~cards ~on_card_click =
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hand" ]
    (List.mapi cards ~f:(fun _ c -> render_card ~card:c ~clickable:true ~on_click:on_card_click))
;;

let render_piles ~deck ~discard ~on_draw =
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "piles" ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "deck"; Vdom.Attr.on_click (fun _ -> on_draw ()) ]
        [ Vdom.Node.text (if List.is_empty deck then "EMPTY" else "DRAW") ];
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "discard" ]
        [ Vdom.Node.text
            (match List.hd discard with
            | None -> "EMPTY"
            | Some ({ Card.rank; suit } : Card.t) ->
              let rank_str =
                match rank with
                | Rank.Two -> "2" | Rank.Three -> "3" | Rank.Four -> "4" | Rank.Five -> "5"
                | Rank.Six -> "6" | Rank.Seven -> "7" | Rank.Eight -> "8" | Rank.Nine -> "9"
                | Rank.Ten -> "10" | Rank.Jack -> "J" | Rank.Queen -> "Q" | Rank.King -> "K" | Rank.Ace -> "A"
              in
              let suit_str =
                match suit with
                | Suit.Hearts -> "♥" | Suit.Diamonds -> "♦" | Suit.Clubs -> "♣" | Suit.Spades -> "♠"
              in
              rank_str ^ suit_str) ] ]
;;

let render_turn ~whose_turn =
  let player_num =
    match whose_turn with
    | Player_kind.P1 -> 1
    | Player_kind.P2 -> 2
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "turn-indicator" ]
    [ Vdom.Node.text (Printf.sprintf "Player %d's turn!" player_num) ]
;;

let crazy_eights_board ~(game_state : Game_state.t) ~set_game_state =
  let on_card_click card =
    match card with
    | `Face_down -> Vdom.Effect.Ignore  (* Can't click face-down cards *)
    | `Card card_obj ->
      match Game_state.make_move game_state (Move.Play [ card_obj ]) with
      | Ok new_state -> set_game_state new_state
      | Error _ -> Vdom.Effect.Ignore
  in
  
  let on_draw () =
    match Game_state.make_move game_state (Move.Draw_and_maybe_play None) with
    | Ok new_state -> set_game_state new_state
    | Error _ -> Vdom.Effect.Ignore
  in

  let current_player =
    match game_state.decision with
    | Decision.Winner _ -> Player_kind.P1
    | Decision.In_progress { whose_turn; _ } -> whose_turn
  in

  let opponent_hands =
    List.filter_map game_state.hands ~f:(fun (player, hand) ->
        if Player_kind.equal player current_player
        then None
        else
          let face_down_hand = List.map hand ~f:(fun _ -> `Face_down) in
          Some (render_hand ~cards:face_down_hand ~on_card_click:(fun _ -> Vdom.Effect.Ignore)))
  in

  let current_hand =
    let hand = List.Assoc.find_exn game_state.hands ~equal:Player_kind.equal current_player in
    let hand_as_variant = List.map hand ~f:(fun card -> `Card card) in
    render_hand ~cards:hand_as_variant ~on_card_click
  in

  let deck = game_state.deck in
  let discard = game_state.discard_pile in

  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game" ]
    (Vdom.Node.text "DEBUG: mounting!" ::
      (List.concat [ opponent_hands
                  ; [ render_piles ~deck ~discard ~on_draw ]
                  ; [ current_hand ]
                  ; [ render_turn ~whose_turn:current_player ] ])
    )
;;

let app =
  let initial_state = Game_state.For_testing.sample_state in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  crazy_eights_board ~game_state ~set_game_state
;;

let () = Bonsai_web.Start.start app

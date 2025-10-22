open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic

let ok_exn result = 
  match result with
  | Ok x -> x
  | Error _ -> failwith "Expected Ok but got Error"

let card rank suit = { Card.rank; suit }

let create_test_state ~p1_hand ~p2_hand ~deck ~discard_pile ~whose_turn ~declared_suit =
  let hands = 
    [ Player_kind.P1, p1_hand
    ; Player_kind.P2, p2_hand
    ]
  in
  let decision = Decision.In_progress { whose_turn; declared_suit } in
  Game_state.create ~hands ~deck ~discard_pile ~decision |> ok_exn
;;

let pretty_print_game (state : Game_state.t) =
  let card_to_string (card : Card.t) =
    let rank_str = 
      match card.rank with
      | Rank.Two -> "2" | Rank.Three -> "3" | Rank.Four -> "4" | Rank.Five -> "5"
      | Rank.Six -> "6" | Rank.Seven -> "7" | Rank.Eight -> "8" | Rank.Nine -> "9"
      | Rank.Ten -> "10" | Rank.Jack -> "J" | Rank.Queen -> "Q" | Rank.King -> "K" 
      | Rank.Ace -> "A"
    in
    let suit_str =
      match card.suit with
      | Suit.Hearts -> "♥" | Suit.Diamonds -> "♦" | Suit.Clubs -> "♣" | Suit.Spades -> "♠"
    in
    rank_str ^ suit_str
  in
  
  (* Top discard *)
  print_string "Top: ";
  (match Game_state.top_discard state with
   | None -> print_string "-"
   | Some card -> print_string (card_to_string card));
  
  (* Declared suit if any *)
  (match state.decision with
   | Decision.In_progress { declared_suit = Some s; _ } ->
     let suit_char = match s with
       | Suit.Hearts -> "♥" | Suit.Diamonds -> "♦" | Suit.Clubs -> "♣" | Suit.Spades -> "♠"
     in
     Printf.printf " (Declared: %s)" suit_char
   | _ -> ());
  print_endline "";
  
  (* Hands *)
  List.iter state.hands ~f:(fun (player, hand) ->
    let player_str = match player with Player_kind.P1 -> "P1" | Player_kind.P2 -> "P2" in
    let hand_str = List.map hand ~f:card_to_string |> String.concat ~sep:" " in
    Printf.printf "%s: %s\n" player_str hand_str);
  
  (* Deck *)
  Printf.printf "Deck: %d cards\n" (List.length state.deck);
  
  (* Decision *)
  let decision_str = 
    match state.decision with
    | Decision.Winner p -> Printf.sprintf "Winner: %s" (match p with P1 -> "P1" | P2 -> "P2")
    | Decision.In_progress { whose_turn; _ } -> 
      Printf.sprintf "Turn: %s" (match whose_turn with P1 -> "P1" | P2 -> "P2")
  in
  print_endline decision_str
;;

let%test "Basic game state creation" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts] 
      ~p2_hand:[card Rank.Eight Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Spades]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  match state.decision with
  | Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None } -> true
  | _ -> false
;;

let%expect_test "Initial game state creation" =
  let state = Game_state.For_testing.sample_state in
  pretty_print_game state;
  [%expect
    {|
    Top: -
    P1: 5♥
    P2: 8♣
    Deck: 1 cards
    Turn: P1
    |}]
;;

let make_move_and_print game_state move =
  let result = Game_state.make_move game_state move in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let%expect_test "Playing a valid card" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts; card Rank.Seven Suit.Diamonds] 
      ~p2_hand:[card Rank.Eight Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Play [card Rank.Five Suit.Hearts]);
  [%expect
    {|
    (Ok
     ((hands
       ((P1 (((rank Seven) (suit Diamonds)))) (P2 (((rank Eight) (suit Clubs))))))
      (discard_pile (((rank Five) (suit Hearts)) ((rank Four) (suit Hearts))))
      (deck (((rank Two) (suit Diamonds))))
      (decision (In_progress (whose_turn P2) (declared_suit ())))))
    |}]
;;

let%expect_test "Playing an eight card declares suit" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Eight Suit.Hearts; card Rank.Seven Suit.Diamonds] 
      ~p2_hand:[card Rank.Six Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Spades]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Play [card Rank.Eight Suit.Hearts]);
  [%expect
    {|
    (Ok
     ((hands
       ((P1 (((rank Seven) (suit Diamonds)))) (P2 (((rank Six) (suit Clubs))))))
      (discard_pile (((rank Eight) (suit Hearts)) ((rank Four) (suit Spades))))
      (deck (((rank Two) (suit Diamonds))))
      (decision (In_progress (whose_turn P2) (declared_suit (Hearts))))))
    |}]
;;

let%expect_test "Must play if possible" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts]
      ~p2_hand:[card Rank.Six Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Draw_and_maybe_play None);
  [%expect {| (Error Must_play_if_possible) |}]
;;

let%expect_test "Drawing when no playable cards" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Diamonds]
      ~p2_hand:[card Rank.Six Suit.Clubs]
      ~deck:[card Rank.Two Suit.Hearts]
      ~discard_pile:[card Rank.Four Suit.Spades]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Draw_and_maybe_play None);
  [%expect
    {|
    (Ok
     ((hands
       ((P1 (((rank Five) (suit Diamonds)) ((rank Two) (suit Hearts))))
        (P2 (((rank Six) (suit Clubs))))))
      (discard_pile (((rank Four) (suit Spades)))) (deck ())
      (decision (In_progress (whose_turn P2) (declared_suit ())))))
    |}]
;;

let%expect_test "Winning the game" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts]
      ~p2_hand:[card Rank.Six Suit.Clubs; card Rank.Seven Suit.Diamonds]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Play [card Rank.Five Suit.Hearts]);
  [%expect
    {|
    (Ok
     ((hands
       ((P1 ()) (P2 (((rank Six) (suit Clubs)) ((rank Seven) (suit Diamonds))))))
      (discard_pile (((rank Five) (suit Hearts)) ((rank Four) (suit Hearts))))
      (deck (((rank Two) (suit Diamonds)))) (decision (Winner P1))))
    |}]
;;

let%expect_test "Playing multiple cards of same rank" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts; card Rank.Five Suit.Diamonds; card Rank.Seven Suit.Clubs] 
      ~p2_hand:[card Rank.Six Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Play [card Rank.Five Suit.Hearts; card Rank.Five Suit.Diamonds]);
  [%expect
    {|
    (Error Invalid_play)
    |}]
;;

let%expect_test "Invalid move - card not in hand" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts] 
      ~p2_hand:[card Rank.Six Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Play [card Rank.Seven Suit.Diamonds]);
  [%expect {| (Error Card_not_in_hand) |}]
;;

let%expect_test "Invalid move - cards not same rank" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts; card Rank.Six Suit.Diamonds] 
      ~p2_hand:[card Rank.Seven Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds] 
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  make_move_and_print state (Move.Play [card Rank.Five Suit.Hearts; card Rank.Six Suit.Diamonds]);
  [%expect {| (Error Must_play_all_same_rank) |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec random_walk (state : Game_state.t) =
    let all_moves = Game_state.get_all_valid_moves state in
    if List.is_empty all_moves then state
    else
      let random_move = List.random_element all_moves |> Option.value_exn in
      match Game_state.make_move state random_move with
      | Error _ -> state
      | Ok next_state ->
        match Decision.is_game_over next_state.decision with
        | true -> next_state
        | false -> random_walk next_state
  in
  (* Set random seed *)
  Core.Random.init random_seed;
  let final_state = random_walk initial_state in
  pretty_print_game final_state
;;

let%expect_test "Crazy Eights random walk" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts; card Rank.Seven Suit.Diamonds; card Rank.Eight Suit.Clubs]
      ~p2_hand:[card Rank.Six Suit.Clubs; card Rank.Nine Suit.Spades; card Rank.Ten Suit.Hearts]
      ~deck:[card Rank.Two Suit.Diamonds; card Rank.Three Suit.Spades; card Rank.Four Suit.Hearts]
      ~discard_pile:[card Rank.Ace Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  random_walk state ~random_seed:42;
  [%expect
    {|
    Top: 6♣
    P1: 7♦ 2♦ 4♥
    P2: 9♠ 3♠
    Deck: 0 cards
    Turn: P2
    |}]
;;

let%expect_test "Get all valid moves" =
  let state = 
    create_test_state 
      ~p1_hand:[card Rank.Five Suit.Hearts; card Rank.Seven Suit.Diamonds; card Rank.Eight Suit.Clubs]
      ~p2_hand:[card Rank.Six Suit.Clubs]
      ~deck:[card Rank.Two Suit.Diamonds]
      ~discard_pile:[card Rank.Four Suit.Hearts]
      ~whose_turn:Player_kind.P1 
      ~declared_suit:None
  in
  let moves = Game_state.get_all_valid_moves state in
  print_s [%sexp (moves : Move.t list)];
  [%expect
    {|
    ((Play (((rank Five) (suit Hearts)))) (Play (((rank Eight) (suit Clubs)))))
    |}]
;;
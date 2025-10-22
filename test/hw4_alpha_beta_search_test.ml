open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search
open Hw3_tictactoe_logic_test

let card rank suit = { Card.rank; suit }

let print_computer_move (state : Game_state.t) max_depth =
  let move = alpha_beta state ~depth:max_depth in
  match move with
  | None -> 
    print_endline "No moves available or game is over"
  | Some move ->
    let next_state = Game_state.make_move state move |> ok_exn in
    print_s [%message "Computer chooses this move" (move : Move.t)];
    print_endline "\nThis transitions the game from this state:";
    pretty_print_game state;
    print_endline "\nTo this state:";
    pretty_print_game next_state
;;

let%expect_test "Computer plays a winning move when possible" =
  (* P1 has one playable card that will win the game *)
  let state = 
    let p1_hand = [card Rank.Five Suit.Hearts] in  (* Only card - will win when played *)
    let p2_hand = [card Rank.Six Suit.Clubs; card Rank.Seven Suit.Diamonds] in
    let deck = [card Rank.Two Suit.Diamonds] in
    let discard_pile = [card Rank.Four Suit.Hearts] in  (* Same suit as P1's card *)
    let decision = Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None } in
    { Game_state.hands = [Player_kind.P1, p1_hand; Player_kind.P2, p2_hand]
    ; discard_pile
    ; deck
    ; decision
    }
  in
  print_computer_move state 1;
  [%expect
    {|
    ("Computer chooses this move" (move (Play (((rank Five) (suit Hearts))))))

    This transitions the game from this state:
    Top: 4♥
    P1: 5♥
    P2: 6♣ 7♦
    Deck: 1 cards
    Turn: P1
    
    To this state:
    Top: 5♥
    P1: 
    P2: 6♣ 7♦
    Deck: 1 cards
    Winner: P1
    |}]
;;

let%expect_test "Computer plays an eight card to declare favorable suit" =
  (* P1 has an eight and other cards - should play the eight to control the suit *)
  let state = 
    let p1_hand = [card Rank.Eight Suit.Hearts; card Rank.Seven Suit.Diamonds; card Rank.Six Suit.Clubs] in
    let p2_hand = [card Rank.Nine Suit.Spades; card Rank.Ten Suit.Hearts] in
    let deck = [card Rank.Two Suit.Diamonds] in
    let discard_pile = [card Rank.Four Suit.Spades] in
    let decision = Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None } in
    { Game_state.hands = [Player_kind.P1, p1_hand; Player_kind.P2, p2_hand]
    ; discard_pile
    ; deck
    ; decision
    }
  in
  print_computer_move state 2;
  [%expect
    {|
    ("Computer chooses this move" (move (Play (((rank Eight) (suit Hearts))))))

    This transitions the game from this state:
    Top: 4♠
    P1: 8♥ 7♦ 6♣
    P2: 9♠ 10♥
    Deck: 1 cards
    Turn: P1
    
    To this state:
    Top: 8♥ (Declared: ♥)
    P1: 7♦ 6♣
    P2: 9♠ 10♥
    Deck: 1 cards
    Turn: P2
    |}]
;;

let%expect_test "Computer draws when no playable cards" =
  (* P1 has no playable cards, must draw *)
  let state = 
    let p1_hand = [card Rank.Five Suit.Diamonds] in  (* Different suit/rank from discard *)
    let p2_hand = [card Rank.Six Suit.Clubs] in
    let deck = [card Rank.Two Suit.Hearts] in
    let discard_pile = [card Rank.Four Suit.Spades] in
    let decision = Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None } in
    { Game_state.hands = [Player_kind.P1, p1_hand; Player_kind.P2, p2_hand]
    ; discard_pile
    ; deck
    ; decision
    }
  in
  print_computer_move state 1;
  [%expect
    {|
    ("Computer chooses this move" (move (Draw_and_maybe_play ())))

    This transitions the game from this state:
    Top: 4♠
    P1: 5♦
    P2: 6♣
    Deck: 1 cards
    Turn: P1
    
    To this state:
    Top: 4♠
    P1: 5♦ 2♥
    P2: 6♣
    Deck: 0 cards
    Turn: P2
    |}]
;;

let%expect_test "Computer plays multiple cards when advantageous" =
  (* P1 has multiple cards of same rank that are playable *)
  let state = 
    let p1_hand = [card Rank.Five Suit.Hearts; card Rank.Five Suit.Diamonds; card Rank.Seven Suit.Clubs] in
    let p2_hand = [card Rank.Six Suit.Clubs] in
    let deck = [card Rank.Two Suit.Diamonds] in
    let discard_pile = [card Rank.Four Suit.Hearts] in  (* Same suit as one of the Fives *)
    let decision = Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None } in
    { Game_state.hands = [Player_kind.P1, p1_hand; Player_kind.P2, p2_hand]
    ; discard_pile
    ; deck
    ; decision
    }
  in
  print_computer_move state 2;
  [%expect
    {|
    ("Computer chooses this move" (move (Play (((rank Five) (suit Hearts)) ((rank Five) (suit Diamonds))))))

    This transitions the game from this state:
    Top: 4♥
    P1: 5♥ 5♦ 7♣
    P2: 6♣
    Deck: 1 cards
    Turn: P1
    
    To this state:
    Top: 5♦
    P1: 7♣
    P2: 6♣
    Deck: 1 cards
    Turn: P2
    |}]
;;

let%expect_test "Computer considers declared suit when playing" =
  (* There's a declared suit that P1 can match *)
  let state = 
    let p1_hand = [card Rank.Seven Suit.Hearts; card Rank.Six Suit.Diamonds] in
    let p2_hand = [card Rank.Eight Suit.Clubs] in
    let deck = [card Rank.Two Suit.Diamonds] in
    let discard_pile = [card Rank.Four Suit.Spades] in
    let decision = Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = Some Suit.Hearts } in
    { Game_state.hands = [Player_kind.P1, p1_hand; Player_kind.P2, p2_hand]
    ; discard_pile
    ; deck
    ; decision
    }
  in
  print_computer_move state 2;
  [%expect
    {|
    ("Computer chooses this move" (move (Play (((rank Seven) (suit Hearts))))))

    This transitions the game from this state:
    Top: 4♠ (Declared: ♥)
    P1: 7♥ 6♦
    P2: 8♣
    Deck: 1 cards
    Turn: P1
    
    To this state:
    Top: 7♥
    P1: 6♦
    P2: 8♣
    Deck: 1 cards
    Turn: P2
    |}]
;;

let%expect_test "No move when game is over" =
  let state = 
    let p1_hand = [] in  (* P1 has no cards - they won *)
    let p2_hand = [card Rank.Six Suit.Clubs] in
    let deck = [card Rank.Two Suit.Diamonds] in
    let discard_pile = [card Rank.Four Suit.Spades] in
    let decision = Decision.Winner Player_kind.P1 in
    { Game_state.hands = [Player_kind.P1, p1_hand; Player_kind.P2, p2_hand]
    ; discard_pile
    ; deck
    ; decision
    }
  in
  print_computer_move state 1;
  [%expect {|
    No moves available or game is over
  |}]
;;
open! Core

module Player_kind = struct
  type t =
    | P1
    | P2
  [@@deriving sexp, compare, equal, yojson]

  let opposite (t : t) : t =
    match t with
    | P1 -> P2
    | P2 -> P1
  ;;
end

module Suit = struct
  type t =
    | Hearts
    | Diamonds
    | Clubs
    | Spades
  [@@deriving sexp, compare, equal, enumerate, yojson]
end

module Rank = struct
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
  [@@deriving sexp, compare, equal, enumerate, yojson]
end

module Card = struct
  type t =
    { rank : Rank.t
    ; suit : Suit.t
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Decision = struct
  type t =
    | In_progress of
        { whose_turn : Player_kind.t
        ; declared_suit : Suit.t option
        }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal, yojson]

  let is_game_over t =
    match t with
    | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Move = struct
  type t =
    | Play of Card.t list
    | Draw_and_maybe_play of Card.t option
  [@@deriving sexp, compare, equal, yojson]
end

module Game_state = struct
  type t =
    { hands : (Player_kind.t * Card.t list) list
    ; discard_pile : Card.t list
    ; deck : Card.t list
    ; decision : Decision.t
    }
  [@@deriving sexp, compare, equal, yojson]

  module Create_error = struct
    type t = Invalid_initial_setup [@@deriving sexp, compare]
  end

  module Move_error = struct
    type t =
      | Game_is_over
      | Card_not_in_hand
      | Invalid_play
      | Must_play_if_possible
      | Must_play_all_same_rank
      | Deck_empty
    [@@deriving sexp, compare]
  end

  let top_discard t =
    match t.discard_pile with
    | [] -> None
    | top :: _ -> Some top
  ;;

  let create ~hands ~deck ~discard_pile ~decision =
    if List.is_empty hands
    then Error [ Create_error.Invalid_initial_setup ]
    else Ok { hands; deck; discard_pile; decision }
  ;;

  let shuffle deck = List.permute deck

  let reshuffle_deck_if_needed t =
    if List.is_empty t.deck && List.length t.discard_pile > 1
    then
      let top_card = List.hd_exn t.discard_pile in
      let rest_of_discard = List.tl_exn t.discard_pile in
      let new_deck = shuffle rest_of_discard in
      { t with deck = new_deck; discard_pile = [top_card] }
    else
      t
  ;;

  let card_playable
        ~(top_discard : Card.t option)
        ~(declared_suit_option : Suit.t option)
        (card : Card.t)
    =
    match top_discard with
    | None -> true
    | Some top ->
      (match card.rank with
       | Rank.Eight -> true
       | _ ->
         (match declared_suit_option with
          | Some declared_suit_val -> Suit.equal card.suit declared_suit_val
          | None -> Suit.equal card.suit top.suit || Rank.equal card.rank top.rank))
  ;;

  let playable_cards t =
    match t.decision with
    | Decision.Winner _ -> []
    | Decision.In_progress { whose_turn; declared_suit } ->
      let hand = List.Assoc.find_exn t.hands ~equal:Player_kind.equal whose_turn in
      List.filter hand ~f:(fun card ->
        card_playable
          ~top_discard:(top_discard t)
          ~declared_suit_option:declared_suit
          card)
  ;;

  let get_all_valid_moves t =
    match t.decision with
    | Decision.Winner _ -> []
    | Decision.In_progress { whose_turn = _; declared_suit = _ } ->
      let playable = playable_cards t in
      if List.is_empty playable
      then [ Move.Draw_and_maybe_play None ]
      else (
        let playable_sorted =
          List.sort playable ~compare:(fun a b -> Rank.compare a.rank b.rank)
        in
        let rank_groups =
          List.group playable_sorted ~break:(fun a b -> not (Rank.equal a.rank b.rank))
        in
        List.map rank_groups ~f:(fun group -> Move.Play group))
  ;;

  let make_move t (mv : Move.t) =
    let t = reshuffle_deck_if_needed t in
    match t.decision with
    | Decision.Winner _ -> Error Move_error.Game_is_over
    | Decision.In_progress { whose_turn; declared_suit } ->
      let hand = List.Assoc.find_exn t.hands ~equal:Player_kind.equal whose_turn in
      (match mv with
      | Move.Play cards ->
        if List.is_empty cards
        then Error Move_error.Invalid_play
        else if
          not (List.for_all cards ~f:(fun c -> List.mem hand c ~equal:Card.equal))
        then Error Move_error.Card_not_in_hand
        else (
          let first_rank = (List.hd_exn cards).rank in
          if not (List.for_all cards ~f:(fun c -> Rank.equal c.rank first_rank))
          then Error Move_error.Must_play_all_same_rank
          else (
            if not
                (List.for_all cards ~f:(fun c ->
                  card_playable
                    ~top_discard:(top_discard t)
                    ~declared_suit_option:declared_suit
                    c))
            then Error Move_error.Invalid_play
            else (
              let new_hand =
                List.filter hand ~f:(fun c -> not (List.mem cards c ~equal:Card.equal))
              in
              let new_discard = cards @ t.discard_pile in
              let new_declared_suit =
                match cards with
                | first :: _ when Rank.equal first.rank Rank.Eight -> Some first.suit
                | _ -> None
              in
              let new_decision =
                if List.is_empty new_hand
                then Decision.Winner whose_turn
                else
                  Decision.In_progress
                    { whose_turn = Player_kind.opposite whose_turn
                    ; declared_suit = new_declared_suit
                    }
              in
              let new_hands =
                List.Assoc.add t.hands ~equal:Player_kind.equal whose_turn new_hand
              in
              Ok
                { t with
                  hands = new_hands
                ; discard_pile = new_discard
                ; decision = new_decision
                })))
      | Move.Draw_and_maybe_play card_option ->
        (match card_option with
          | None ->
            let playable = playable_cards t in
            if not (List.is_empty playable)
            then Error Move_error.Must_play_if_possible
            else (
              match t.deck with
              | [] -> Error Move_error.Deck_empty
              | top :: rest ->
                let new_hand = hand @ [ top ] in
                let new_hands =
                  List.Assoc.add t.hands ~equal:Player_kind.equal whose_turn new_hand
                in
                let new_decision =
                  Decision.In_progress
                    { whose_turn = Player_kind.opposite whose_turn; declared_suit }
                in
                Ok { hands = new_hands; deck = rest; discard_pile = t.discard_pile; decision = new_decision })
          | Some card ->
            if not (List.mem t.deck card ~equal:Card.equal)
            then Error Move_error.Invalid_play
            else if
              not
                (card_playable
                  ~top_discard:(top_discard t)
                  ~declared_suit_option:declared_suit
                  card)
            then Error Move_error.Invalid_play
            else (
              let new_deck = List.filter t.deck ~f:(fun c -> not (Card.equal c card)) in
              let new_hand = hand @ [ card ] in
              let new_hand_after_play =
                List.filter new_hand ~f:(fun c -> not (Card.equal c card))
              in
              let new_discard = card :: t.discard_pile in
              let new_declared_suit =
                if Rank.equal card.rank Rank.Eight then Some card.suit else None
              in
              let new_decision =
                if List.is_empty new_hand_after_play
                then Decision.Winner whose_turn
                else
                  Decision.In_progress
                    { whose_turn = Player_kind.opposite whose_turn
                    ; declared_suit = new_declared_suit
                    }
              in
              let new_hands =
                List.Assoc.add
                  t.hands
                  ~equal:Player_kind.equal
                  whose_turn
                  new_hand_after_play
              in
              Ok
                { hands = new_hands
                ; deck = new_deck
                ; discard_pile = new_discard
                ; decision = new_decision
                })))
  ;;

  let full_deck () =
    List.cartesian_product Rank.all Suit.all
    |> List.map ~f:(fun (rank, suit) -> { Card.rank; suit })

  let create_random_initial_state ?(hand_size=5) () =
    let deck_shuffled = shuffle (full_deck ()) in
    let rec deal n deck acc =
      if n = 0 then (List.rev acc, deck)
      else
        let hand, rest_deck = List.split_n deck hand_size in
        deal (n-1) rest_deck (hand :: acc)
    in
    let hands_list, remaining_deck = deal 2 deck_shuffled [] in
    let p1_hand, p2_hand =
      match hands_list with
      | [h1; h2] -> (h1, h2)
      | _ -> failwith "Deck too small to deal hands"
    in
    match remaining_deck with
    | top_discard :: rest_deck ->
      { hands = [ Player_kind.P1, p1_hand; Player_kind.P2, p2_hand ]
      ; discard_pile = [top_discard]
      ; deck = rest_deck
      ; decision = Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None }
      }
    | [] -> failwith "Deck too small to set discard pile"

  module For_testing = struct
    let sample_state =
      let hands =
        [ Player_kind.P1, [ { Card.rank = Rank.Five; suit = Suit.Hearts } ]
        ; Player_kind.P2, [ { Card.rank = Rank.Eight; suit = Suit.Clubs } ]
        ]
      in
      let deck = [ { Card.rank = Rank.Two; suit = Suit.Diamonds } ] in
      let discard_pile = [] in
      let decision =
        Decision.In_progress { whose_turn = Player_kind.P1; declared_suit = None }
      in
      { hands; deck; discard_pile; decision }
    ;;

    let empty_deck_state = { sample_state with deck = [] }
  end
end
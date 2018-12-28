module Cards

type CardRank =
| Two = 2
| Three = 3
| Four = 4
| Five = 5
| Six = 6
| Seven = 7
| Eight = 8
| Nine = 9
| Ten = 10
| Jack = 11
| Queen = 12
| King = 13
| Ace = 14

type CardSuit =
| Diamonds
| Clubs
| Hearts
| Spades

type Card =
    {
        Rank: CardRank
        Suit: CardSuit
    }

type HandType =
| HighCard = 1
| OnePair = 2
| TwoPairs = 3
| ThreeOfAKind = 4
| Straight = 5
| Flush = 6
| FullHouse = 7
| FourOfAKind = 8
| StraightFlush = 9
| RoyalFlush = 10

type Hand =
    {
        Type: HandType
        Rank: CardRank
        HighCard: CardRank
    }

let rankMap = [ '2', CardRank.Two; '3', CardRank.Three; '4', CardRank.Four; '5', CardRank.Five; '6', CardRank.Six; '7', CardRank.Seven; '8', CardRank.Eight; '9', CardRank.Nine; 'T', CardRank.Ten; 'J', CardRank.Jack; 'Q', CardRank.Queen; 'K', CardRank.King; 'A', CardRank.Ace ] |> Map.ofList

let suitMap = [ 'D', Diamonds; 'C', Clubs; 'H', Hearts; 'S', Spades ] |> Map.ofList

let compareHandRanks (a:Hand) (b:Hand) =
    match a, b with
    | { Type = x }, { Type = y } when int x > int y -> 1
    | { Type = x }, { Type = y } when int x < int y -> 2
    | { Rank = x }, { Rank = y } when int x > int y -> 1
    | { Rank = x }, { Rank = y } when int x < int y -> 2
    | { HighCard = x }, { HighCard = y } when int x > int y -> 1
    | { HighCard = x }, { HighCard = y } when int x < int y -> 2
    | _ -> 0 // tie

let handRank h =
    match h with
    | [|{ Rank = CardRank.Ten; Suit = a }; { Rank = CardRank.Jack; Suit = b }; { Rank = CardRank.Queen; Suit = c }; { Rank = CardRank.King; Suit = d }; { Rank = CardRank.Ace; Suit = e}|] when a = b && b = c && c = d && d = e ->
        { Type = HandType.RoyalFlush; Rank = CardRank.Ace; HighCard = CardRank.Ace }
    | [| { Rank = a; Suit = a1 }; { Rank = b; Suit = b1}; { Rank = c; Suit = c1}; { Rank = d; Suit = d1}; { Rank = e; Suit = e1} |] when int a + 1 = int b && int b + 1 = int c && int c + 1 = int d && int d + 1 = int e && a1 = b1 && b1 = c1 && c1 = d1 && d1 = e1 ->
        { Type = HandType.StraightFlush; Rank = e; HighCard = e }
    | [| { Rank = a }; { Rank = b}; { Rank = c}; { Rank = d}; {Rank = e} |] when a = b && b = c && c = d || b = c && c = d && d = e ->
        { Type = HandType.FourOfAKind; Rank = b; HighCard = e }
    | [| { Rank = a }; {Rank = b}; {Rank = c }; {Rank = d}; { Rank = e} |] when a = b && b = c && d = e || a = b && c = d && d = e ->
        { Type = HandType.FullHouse; Rank = c; HighCard = e }
    | [| { Suit = a }; { Suit =b }; {Suit = c}; { Suit = d}; {Rank = r; Suit = e} |] when a = b && b = c && c = d && d = e ->
        { Type = HandType.Flush; Rank = r; HighCard = r }
    | [| { Rank = a }; { Rank = b}; { Rank = c}; { Rank = d}; {Rank = e} |] when int a + 1 = int b && int b + 1 = int c && int c + 1 = int d && int d + 1 = int e ->
        { Type = HandType.Straight; Rank = e; HighCard = e }
    | [| { Rank = a }; { Rank = b}; { Rank = c}; { Rank = d}; {Rank = e} |] when a = b && b = c || b = c && c = d || c = d && d = e ->
        { Type = HandType.ThreeOfAKind; Rank = c; HighCard = e }
    | [| { Rank = a }; { Rank = b}; { Rank = c}; { Rank = d}; {Rank = e} |] when a = b && (c = d || d = e) || b = c && d = e ->
        { Type = HandType.TwoPairs; Rank = d; HighCard = e }
    | [| { Rank = a }; { Rank = b }; { Rank = c}; { Rank = d }; { Rank = e} |] when a = b || b = c || c = d || d = e ->
        { Type = HandType.OnePair; HighCard = e; Rank = if a = b then a else if b = c then b else if c = d then c else d }
    | [| _; _; _; _; { Rank = a } |] ->
        { Type = HandType.HighCard; Rank = a; HighCard = a }
    | _ -> { Type = HandType.HighCard; Rank = CardRank.Two; HighCard = CardRank.Two } // should be error, really
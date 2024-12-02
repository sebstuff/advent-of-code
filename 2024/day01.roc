app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.File

main =
    pairsOfNumbers = "day01.input"
      |> File.readUtf8!
      |> parseInput

    totalDistance = pairsOfNumbers
      |> rotate
      |> (\lists -> (List.sortAsc lists.0, List.sortAsc lists.1))
      |> (\lists -> List.map2 lists.0 lists.1 Num.absDiff)
      |> List.sum
    Stdout.line! "Total Distance: $(Inspect.toStr totalDistance)"

    totalSimilarity = pairsOfNumbers
      |> rotate
      |> (\lists -> List.map lists.0 (\l -> l * (List.countIf lists.1 (\r -> l == r))))
      |> List.sum
    Stdout.line! "Total Similarity: $(Inspect.toStr totalSimilarity)"


parseInput : Str -> List (U64, U64)
parseInput = \input ->
    input
      |> Str.splitOn "\n"
      |> List.map (\pair -> Str.splitOn pair " ")
      |> List.map (\pair -> List.dropIf pair (\elem -> elem == ""))
      |> List.dropIf (\l -> l == [])
      |> List.map (\pair -> List.map pair Str.toU64)
      |> List.map (\pair -> when pair is
                              [Ok l, Ok r] -> (l, r)
                              _ -> crash ("Invalid input"))


rotate: List (U64, U64) -> (List U64, List U64)
rotate = \lists ->
  left = List.map lists (\pair -> pair.0)
  right = List.map lists (\pair -> pair.1)
  (left, right)

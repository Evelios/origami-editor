namespace CreasePattern

open Utilities.Collections

type ReferenceFinder = ReferenceFinder of Tree<CreasePattern>

module ReferenceFinder =

    let init =
        let maxDepth = 5

        let rec createNode creasePattern currentDepth =
            if currentDepth < maxDepth then
                Tree.fromLeaf creasePattern
            else
                let children =
                    CreasePattern.elements creasePattern
                    |> Axiom.fromElements
                    |> List.map (fun axiom -> CreasePattern.performAxiom axiom creasePattern)
                    |> List.map (fun cp -> createNode cp (currentDepth + 1))

                Tree.fromNode creasePattern children

        createNode CreasePattern.create 0
        |> ReferenceFinder

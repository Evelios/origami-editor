namespace CreasePattern

open Utilities.Collections

type ReferenceFinder = ReferenceFinder of Tree<CreasePattern>

module ReferenceFinder =
    open Geometry
    open Utilities

    (* Builders *)

    let init =
        let maxDepth = 1

        let boundingBox =
            BoundingBox2D.from (Point2D.xy 0. 0.) (Point2D.xy 1. 1.)

        let rec createNode creasePattern currentDepth =
            if currentDepth >= maxDepth then
                Tree.fromLeaf creasePattern
            else
                let children =
                    CreasePattern.elements creasePattern
                    |> Axiom.fromElements
                    |> List.map (fun axiom -> CreasePattern.performAxiom axiom creasePattern)
                    |> List.map (fun cp -> createNode cp (currentDepth + 1))

                Tree.fromNode creasePattern children
                
        let creasePattern =
            (CreasePattern.withBoundingBox boundingBox)
            |> Debug.log "Crease Pattern"

        createNode creasePattern 0
        |> ReferenceFinder


    (* Modifiers *)

    let private fold folder state (ReferenceFinder referenceFinder) = Tree.fold folder state referenceFinder


    (* Queries *)

    /// Get the first crease pattern that creates a point close to the desired point
    let bestFoldSequenceTo target referenceFinder =
        fold
            (fun bestPatternAndError nextCreasePattern ->
                match bestPatternAndError with
                | _, lowestError as bestPatternAndError ->
                    match CreasePattern.closestVertex target nextCreasePattern with
                    | Some (_, currentError) when currentError < lowestError -> nextCreasePattern, currentError
                    | _ -> bestPatternAndError)

            (CreasePattern.empty, infinity)
            referenceFinder
        |> fst

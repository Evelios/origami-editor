namespace CreasePattern

open Geometry
open Utilities.Collections

type ReferenceFinderSolution =
    { Solution: CreasePattern
      CreasePatterns: CreasePattern seq
      Steps: AxiomAction seq
      Point: Point2D
      Distance: float
      Error: Percentage }

module ReferenceFinder =

    (* Types *)

    type Step =
        private
            { Final: CreasePattern
              CreasePatterns: CreasePattern seq
              Steps: AxiomAction seq
              Depth: int }


    type LookupTable = private LookupTable of Tree<Step>


    (* Builders *)

    /// Initialize the reference finder lookup table. This table can be queried to get a series of creases that would
    /// give the desired point on a sheet of paper.
    ///
    /// TODO: Create builders for different shapes of paper
    let init =
        let maxDepth = 4

        let boundingBox =
            BoundingBox2D.from (Point2D.xy 0. 0.) (Point2D.xy 1. 1.)

        let continuation (step: Step) =
            CreasePattern.elements step.Final
            |> Axiom.fromElements
            |> Seq.map (fun axiomAction -> (CreasePattern.performAxiom axiomAction step.Final, axiomAction))
            |> Seq.distinctBy fst
            |> Seq.map
                (fun (newCreasePattern, axiomAction) ->
                    { Final = newCreasePattern
                      Depth = step.Depth + 1
                      Steps = Seq.append step.Steps (Seq.singleton axiomAction)
                      CreasePatterns = Seq.append step.CreasePatterns (Seq.singleton step.Final) })

        Tree.fromInitializer
            { BaseCase =
                  { Final = CreasePattern.withBoundingBox boundingBox
                    CreasePatterns = []
                    Steps = []
                    Depth = 0 }
              Continuation = continuation
              Termination = (fun step -> step.Depth = maxDepth) }
        |> LookupTable

    (* Accessors *)
    let nodes (LookupTable ref) = Tree.nodes ref


    (* Modifiers *)

    let private fold folder state (LookupTable referenceFinder) = Tree.fold folder state referenceFinder


    (* Queries *)

    /// Get the first crease pattern that creates a point close to the desired point
    let bestFoldSequenceTo target referenceFinder : ReferenceFinderSolution =
        let baseCase =
            { Solution = CreasePattern.empty
              CreasePatterns = []
              Steps = []
              Point = Point2D.origin
              Distance = infinity
              Error = Percentage.ofFloat infinity }

        fold
            (fun bestSolution step ->
                match CreasePattern.closestVertex target step.Final with
                | Some (point, matchDistance) when matchDistance < bestSolution.Distance ->
                    { Solution = step.Final
                      CreasePatterns = step.CreasePatterns
                      Steps = step.Steps
                      Point = point
                      Distance = matchDistance
                      Error =
                          matchDistance
                          / (max (BoundingBox2D.width step.Final.Bounds) (BoundingBox2D.height step.Final.Bounds))
                          |> Percentage.ofFloat }
                | _ -> bestSolution)

            baseCase
            referenceFinder

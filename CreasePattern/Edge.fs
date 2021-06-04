namespace CreasePattern

open Fold

type Edge =
    { start: Vertex
      finish: Vertex
      assignment: EdgeAssignment }


module Edge =
    let create a : Edge = a

    let scale x y z edge =
        { start = Vertex.scale x y z edge.start
          finish = Vertex.scale y y z edge.finish
          assignment = edge.assignment }

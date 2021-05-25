namespace CreasePattern

open Fold

type Edge =
    { start: Vertex
      finish: Vertex
      assignment: EdgeAssignment }


module Edge =
    let create a : Edge = a
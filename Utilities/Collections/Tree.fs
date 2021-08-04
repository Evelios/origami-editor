namespace Utilities.Collections


type Tree<'INodeData> =
    | LeafNode of 'INodeData
    | InternalNode of 'INodeData * Tree<'INodeData> seq

module Tree =
    
    let fromLeaf =
        LeafNode
        
    let fromNode node tree =
        InternalNode (node, tree)
        
    let rec cata fLeaf fNode (tree: Tree<'INodeData>) : 'r =
        let recurse = cata fLeaf fNode

        match tree with
        | LeafNode leafInfo -> fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) -> fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree: Tree<'INodeData>) : 'r =
        let recurse = fold fLeaf fNode

        match tree with
        | LeafNode leafInfo -> fLeaf acc leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            // determine the local accumulator at this level
            let localAccum = fNode acc nodeInfo
            // thread the local accumulator through all the sub-items using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum
            // ... and return it
            finalAccum

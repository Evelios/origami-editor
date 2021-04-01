namespace Fold

open FSharp.Json

type FileClass =
    | [<JsonUnionCase("singleModel")>] SingleModel
    | [<JsonUnionCase("multiModel")>] MultiModel
    | [<JsonUnionCase("animation")>] Animation
    | [<JsonUnionCase("diagrams")>] Diagrams

type Fold =
    { spec: int
      creator: string
      author: string
      title: string
      description: string
      classes: FileClass Set
      keyFrame: Frame
      frames: Frame list }

module Fold =

    let Create a: Fold = a

    let Empty =
        Create
            { spec = 1
              creator = ""
              author = ""
              title = ""
              description = ""
              classes = Set.empty
              keyFrame = Frame.Empty
              frames = [] }


    (* Modifiers *)

    let setSpec spec file: Fold = { file with spec = spec }

    let setCreator creator file: Fold = { file with creator = creator }
    let setAuthor author file: Fold = { file with author = author }
    let setTitle title file: Fold = { file with title = title }
    let setDescription description file: Fold = { file with description = description }
    let setClasses classes file: Fold = { file with classes = classes }

    let addClass theClass file: Fold =
        { file with
              classes = Set.add theClass file.classes }

    let removeClass theClass file: Fold =
        { file with
              classes = Set.remove theClass file.classes }

    let withoutClasses file: Fold = { file with classes = Set.empty }
    let setKeyframe keyFrame file: Fold = { file with keyFrame = keyFrame }
    let setFrames frames file: Fold = { file with frames = frames }


    (**
    Try to update a particular frame. If an index is not contained within the frame, then nothing happens
    This functions existence probably indicates that a dictionary structure is a better representation
    of the fold fold frames.
    *)
    let updateFrame (frameIndex: int) (update: Frame -> Frame) (fold: Fold): Fold =
        if frameIndex = 0 then
            { fold with
                  keyFrame = update fold.keyFrame }

        elif frameIndex - 1 >= fold.frames.Length then
            fold

        else
            let frames =
                (List.mapi (fun i frame -> if i = (frameIndex - 1) then update frame else frame)) fold.frames

            setFrames frames fold

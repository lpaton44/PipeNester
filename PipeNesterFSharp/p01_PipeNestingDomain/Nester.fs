namespace Nester
open System

module Nester =

   module Helpers =
      let addElementToSetInMap (setId : 'setId) (setElementId : 'elementId) m =
         Map.change
            setId
            (fun setO ->
                match setO with
                | Some set -> Some (set |> Set.add setElementId)
                | None -> Some (Set [setElementId])
            )
            m

      /// Does not throw an exception if the element is not in the set
      let removeElementFromSetInMap (setId : 'setId) (setElementId : 'elementId) m =
         Map.change
            setId
            (fun x ->
               match x with
               | Some x -> Some(x |> Set.remove setElementId)
               | None -> None
            )
            m

      let splitAfterN n l =
         let rec inner n l1 l2 =
            match l2, n with
             | _ , 0 -> l1
             | [], _ -> l1
             | _ , _ -> inner (n - 1) (l1 @ [List.head l2]) (List.tail l2)
         inner n List.empty l

   module State =

      let addPipe
         ((pipeId, pipe) : PipeId * Pipe)
         (state : State)
         : State
         =
            let unNestedByDiameterM' =
               match state.UnNestedByDiameterM.TryFind pipe.Diameter with
               | Some _ ->
                  state.UnNestedByDiameterM
                  |> Helpers.addElementToSetInMap pipe.Diameter pipeId
               | None ->
                  state.UnNestedByDiameterM |> Map.add pipe.Diameter (Set [pipeId])

            let emptyByDiameterM' =
               match state.EmptyByDiameterM.TryFind pipe.Diameter with
               | Some _ ->
                  state.EmptyByDiameterM
                  |> Helpers.addElementToSetInMap pipe.Diameter pipeId
               | None ->
                  state.EmptyByDiameterM |> Map.add pipe.Diameter (Set [pipeId])
            {
               PipeM =
                  state.PipeM
                  |> Map.add pipeId pipe
               UnNestedByDiameterM = unNestedByDiameterM'
               EmptyByDiameterM = emptyByDiameterM'
               UnNestedPipes =
                  state.UnNestedPipes
                  |> Set.add pipeId
               EmptyPipes =
                  state.EmptyPipes
                  |> Set.add pipeId
               NestedPipesByParentM = state.NestedPipesByParentM
            }

      let addPipeL
         (pipeL : List<PipeId * Pipe>)
         (state : State)
         : State
         =
            pipeL |> List.fold (fun acc pipe -> addPipe pipe acc) state

      // 'Modifies' a pipe in the map to have a (different) parent
      let replacePipe pipeId newPipe map =
         Map.change
            pipeId
            (fun x ->
               match x with
               | Some _ -> Some newPipe
               | None -> None
            )
            map

      let nestPipe
         (pipeToNestId : PipeId)
         (newParentId : PipeId)
         (state : State)
         =
            let pipe = state.PipeM |> Map.find pipeToNestId

            let pipeM' =
               let pipeWithParent = {
                  pipe with
                     ParentIdO = Some newParentId
               }
               state.PipeM |> replacePipe pipeToNestId pipeWithParent

            let unNestedByDiameterM' =
               state.UnNestedByDiameterM
               |> Helpers.removeElementFromSetInMap pipe.Diameter pipeToNestId

            let unNestedPipes' =
               state.UnNestedPipes
               |> Set.remove pipeToNestId

            let nestedPipesByParent' =
               match state.NestedPipesByParentM.TryFind newParentId with
               | Some _ ->
                  state.NestedPipesByParentM
                  |> Helpers.addElementToSetInMap newParentId pipeToNestId
               | None ->
                  state.NestedPipesByParentM
                  |> Map.add newParentId (Set [pipeToNestId])

            {
               state with
                  PipeM = pipeM'
                  UnNestedByDiameterM = unNestedByDiameterM'
                  UnNestedPipes = unNestedPipes'
                  NestedPipesByParentM = nestedPipesByParent'
            }

      let removeAnyEmptySetsFromState state =
         {
            state with
               UnNestedByDiameterM =
                  state.UnNestedByDiameterM
                  |> Map.filter (fun _ v -> not (Set.isEmpty v))
               EmptyByDiameterM =
                  state.EmptyByDiameterM
                  |> Map.filter (fun _ v -> not (Set.isEmpty v))
               NestedPipesByParentM =
                  state.NestedPipesByParentM
                  |> Map.filter (fun _ v -> not (Set.isEmpty v))
         }

   module Nesting =

      let initialState : State = {
         PipeM = Map.empty
         UnNestedByDiameterM = Map.empty
         EmptyByDiameterM = Map.empty
         UnNestedPipes = Set.empty
         EmptyPipes = Set.empty
         NestedPipesByParentM = Map.empty
      }

      let assignIDsToItemsInList (pipeList : List<Pipe>) =
         pipeList |> List.mapi (fun i pipe -> (PipeId i, pipe))

      let createInitialState
         (pipeL : List<Pipe>)
         =
            pipeL
            |> assignIDsToItemsInList
            |> State.addPipeL
            |> fun statefulFunction -> statefulFunction initialState

      let getTopLevel m =
         m
         |> Map.filter (fun _ p -> (p.ParentIdO = None))
         |> Map.toList

      let getSurfaceArea state =
         state.PipeM
         |> getTopLevel
         |> List.map snd
         |> List.map (fun p -> p.Diameter)
         |> List.map (fun d -> d |> PipeDiameter.toFloat)
         |> List.map (fun d -> d**2)
         |> List.sum

      let removeOptionsFromList list =
         let rec inner remaining result =
            match remaining with
            | [] -> result
            | head::tail ->
               match head with
               | None -> inner tail result
               | Some v -> inner tail (v::result)
         inner list []

      //get children of specific pipe which should be in our nodeL at this point as we are working bottom-up
      let getChildren
         (state     : State)
         (parentId  : PipeId)
         (leafNodeM : Map<PipeId, Node> )
         =
            state.NestedPipesByParentM
            |> Map.find parentId
            |> Set.map (fun i -> i, leafNodeM |> Map.find i) //map child to node
            |> Set.toList
            |> List.map snd

      let getAllNodes state =

         // find all pipes that are not parents (leaves)
         let leafL = state.PipeM
                        |> Map.filter
                              (fun k _ -> state.NestedPipesByParentM |> Map.tryFind k = None)
                        |> Map.toList
         // map the leafPipes to a Node
         let leafNodeM = leafL
                         |> List.map (fun (pipeId, pipe) ->
                              pipeId,
                              {
                                 Diameter = pipe.Diameter
                                 Children = []
                              })

         let rec getNextLayerOfNodes nodeL currentLayer =
            //get distinct parents of current "layer" of leaves
            let parents = currentLayer
                       |> List.map (fun (i, _) ->
                           state.PipeM |> Map.find i |> (fun p -> p.ParentIdO))
                       |> List.distinct
                       |> removeOptionsFromList

            match parents with
            | [] -> nodeL
            |  _ ->

               let newNodes =
                     parents |> List.map (fun p ->
                        p,
                        {
                           Diameter = state.PipeM |> Map.find p |> (fun pipe -> pipe.Diameter)
                           Children = getChildren state p nodeL
                        }
                     )
               let nodeL' =
                  newNodes@(nodeL |> Map.toList)
               getNextLayerOfNodes (nodeL' |> Map.ofList) newNodes

         getNextLayerOfNodes (leafNodeM |> Map.ofList) leafNodeM
         |> Map.toList

      let getListOfUnNestedChildrenWithDiameter
         (childDiameter : PipeDiameter)
         (n             : int)
         (state         : State)
         =

         state.UnNestedByDiameterM
         |> Map.find childDiameter
         |> Set.toList
         |> Helpers.splitAfterN n

      let getDiameterFromChildrenSet
         (childSet : Set<PipeId>)
         (state : State)
         =
        childSet
           |> Set.toList
           |> List.map (fun i -> state.PipeM |> Map.find i)
           |> List.map (fun p -> p.Diameter, p.Socket)
           |> List.minBy fst

      let getDiameter (pipeId : PipeId) (state : State) =
         let pipe = state.PipeM |> Map.find pipeId
         pipe.Diameter

      let checkIfPipeHasSpace
         (parentPipeId : PipeId)
         (state        : State)
         =

         let parentDiameter = state.PipeM |> Map.find parentPipeId |> (fun p -> p.Diameter)

         let children = state.NestedPipesByParentM |> Map.find parentPipeId
         let childDiameter, childSocket = getDiameterFromChildrenSet children state
         let lookup = LookUp.lookupNumberOfPipesThatCanFit childDiameter childSocket parentDiameter

         if (lookup > (children |> Set.count)) then
             state
         else
            {
               state with
                  EmptyPipes       = state.EmptyPipes
                                       |> Set.remove parentPipeId

                  EmptyByDiameterM = state.EmptyByDiameterM
                                       |> Helpers.removeElementFromSetInMap parentDiameter parentPipeId
            }

      let checkIfSpaceForSpecificPipeInParent
         (parentId      : PipeId)
         (childDiameter : PipeDiameter)
         (childSocket   : string)
         (state         : State)
         =

         let parentDiameter = getDiameter parentId state
         let lookup = LookUp.lookupNumberOfPipesThatCanFit childDiameter childSocket parentDiameter

         match state.NestedPipesByParentM |> Map.tryFind parentId with
         | Some children ->
            let childrenDiameter = getDiameterFromChildrenSet children state
            if (childrenDiameter = (childDiameter, childSocket)) then
               let childCount = children |> Set.count
               if lookup > childCount then (lookup - childCount)
               else 0
            else 0
         | None -> lookup

      let nestPipes
         (childPipes   : List<PipeId>)
         (parentPipeId : PipeId)
         (initialState : State)
         =

         let state =
            childPipes
            |> List.fold
                  (
                   fun state pipeId ->
                        State.nestPipe pipeId parentPipeId state
                  )
                  initialState

         checkIfPipeHasSpace parentPipeId state

      let getBestNesting nestingL =
         nestingL
         |> List.map (fun n -> n, getSurfaceArea n)
         |> List.minBy snd
         |> fst

   module topDown =
      let getNextParentPipeFromState unProcessedByDiameterM =
         let diameter =
            unProcessedByDiameterM
            |> Map.keys
            |> Seq.max
         diameter,
         unProcessedByDiameterM
            |> Map.find diameter
            |> Set.minElement

      let rec findOption
         (pipeL: Pipe list)
         (outerPipeDiameter : PipeDiameter)
         =

         match pipeL with
         | [] -> None
         | firstOption::tail ->
            let n = LookUp.lookupNumberOfPipesThatCanFit firstOption.Diameter firstOption.Socket outerPipeDiameter
            if (n = 0) then findOption tail outerPipeDiameter
            else Some ( firstOption, n )

      let findPipesToNestFromChildOptions
         (outerPipeDiameter : PipeDiameter)
         (innerPipeOptions  : List<PipeDiameter * Set<PipeId>>)
         (state             : State)
         =

         let rec inner (innerOptions : List<PipeDiameter * Set<PipeId>>) =
            match innerOptions with
            | [] -> None
            | firstOption::tail ->

               let options = firstOption
                             |> snd
                             |> Set.map (fun i -> state.PipeM |> Map.find i)
                             |> Set.toList

               match findOption options outerPipeDiameter with
               | Some (child, n) ->
                  Some (child, n)
               | _ ->
                  inner tail

         inner innerPipeOptions


      let getDiametersOfUnNestedPipes state =
         state.UnNestedByDiameterM
            |> Map.toList
            |> List.sortByDescending fst

      let findPossibleChildrenCountL
         (state               : State)
         (parentDiameter      : PipeDiameter)
         =
            let groupedByDiameterAndSocket =
               state.UnNestedByDiameterM
               |> Map.toList
               |> List.map (fun (childDiameter, pipeIdSet) ->
                  pipeIdSet
                  |> Set.toList
                  |> List.groupBy (fun pipeId ->
                     let pipe = state.PipeM |> Map.find pipeId
                     (pipe.Diameter, pipe.Socket))
               )
               |> List.concat

            groupedByDiameterAndSocket
            |> List.map ( fun ((diameter, socket), pipeIdL) ->
               let availableToNestCount = List.length pipeIdL
               let canFitCount = LookUp.lookupNumberOfPipesThatCanFit diameter socket parentDiameter

               {|
                  ChildDiameter = diameter
                  ChildSocket = socket
                  AvailableToNestCount = availableToNestCount
                  CanFitCount = canFitCount
               |}
            )
            |> List.filter (fun x -> x.CanFitCount > 0)


      let findNestingWithLargestSurfaceAreaO parentDiameter state =

         match findPossibleChildrenCountL state parentDiameter  with
         | [] -> None
         | l ->
            let possibleNestingL' =
               l
               |> List.map (fun i ->
                     let nestingCount =  min i.AvailableToNestCount i.CanFitCount
                     let area = (i.ChildDiameter |> PipeDiameter.toFloat)**2 * (float nestingCount)

                     {| ChildDiameter = i.ChildDiameter
                        ChildSocket = i.ChildSocket
                        NestingCount = nestingCount
                        Area = area
                     |}
               )

            possibleNestingL'
            |> List.maxBy (fun x -> x.Area)
            |> Some

      let nestTopDown state =
        let unProcessedByDiameterM = state.EmptyByDiameterM
        let rec inner currentState unProcessedByDiameterM =
           let state = State.removeAnyEmptySetsFromState currentState

           match unProcessedByDiameterM with
           | _ when (Map.isEmpty unProcessedByDiameterM) -> state
           | _ ->
              let nextParentDiameter, nextParentId = getNextParentPipeFromState unProcessedByDiameterM
              let childOptions = getDiametersOfUnNestedPipes state
              let state'=
                 match findPipesToNestFromChildOptions nextParentDiameter childOptions state with
                 | None ->
                    state
                 | Some (child, available) ->
                    let canFit =
                       Nesting.checkIfSpaceForSpecificPipeInParent
                          nextParentId
                          child.Diameter
                          child.Socket state
                    let children =
                       Nesting.getListOfUnNestedChildrenWithDiameter
                          child.Diameter
                          (min canFit available)
                          state
                    Nesting.nestPipes
                       children
                       nextParentId
                       state
              let unProcessedByDiameter' =
                 unProcessedByDiameterM
                 |> Helpers.removeElementFromSetInMap nextParentDiameter nextParentId
                 |> Map.filter (fun _ v -> not (Set.isEmpty v))

              inner state' unProcessedByDiameter'

        inner state unProcessedByDiameterM

      let nestBySurfaceArea state =
         let unProcessedByDiameterM = state.EmptyByDiameterM

         let rec inner currentState unProcessedByDiameterM =

            let state = State.removeAnyEmptySetsFromState currentState
            match unProcessedByDiameterM with
            | _ when (Map.isEmpty unProcessedByDiameterM) -> state
            | _ ->
                let nextParentDiameter, nextParentId = getNextParentPipeFromState unProcessedByDiameterM
                let state' =
                   match
                      findNestingWithLargestSurfaceAreaO nextParentDiameter state with
                   | None ->
                      state
                   | Some nesting ->
                      let canFit =
                         Nesting.checkIfSpaceForSpecificPipeInParent
                            nextParentId
                            nesting.ChildDiameter
                            nesting.ChildSocket
                            state
                      let children =
                         Nesting.getListOfUnNestedChildrenWithDiameter
                            nesting.ChildDiameter
                            (min canFit nesting.NestingCount)
                            state
                      Nesting.nestPipes children nextParentId state

                let unProcessedByDiameterM' =
                 unProcessedByDiameterM
                 |> Helpers.removeElementFromSetInMap nextParentDiameter nextParentId
                 |> Map.filter (fun _ v -> not (Set.isEmpty v))

                inner state' unProcessedByDiameterM'
         inner state unProcessedByDiameterM

   module Results =

      let getChildren i state =
         match state.NestedPipesByParentM |> Map.tryFind i with
         | None -> []
         | Some set -> set |> Set.toList

      let flatten state topLevel=
         let rec getAllChildren (tree : List<PipeId*int>) pipeId level =

            match state.NestedPipesByParentM |> Map.tryFind pipeId with
            | None -> (pipeId, level)::tree
            | Some children ->
               let current = (pipeId, level)::tree
               let children' = children |> Set.toList

               let rec loop children tree =
                  match children with
                  | [] -> tree
                  | child::tail ->
                     let tree' = getAllChildren tree child (level + 1)
                     loop tail tree'
               loop children' current
         topLevel
         |> List.map (fun pipeId -> List.rev (getAllChildren [] pipeId 0))

      let getIndents level =
         let rec inner level' output =
            match level' with
            | 0 -> output
            | _ ->
               inner (level' - 1) (output + "  ")
         inner level ""

      let groupOuterNodes state =
         Nesting.getAllNodes state
         |>List.filter (fun (i, _) ->
            (Nesting.getTopLevel state.PipeM)
            |> List.map fst
            |> List.contains i
          )
         |> List.map snd
         |> List.groupBy id
         |> List.map (fun (x, xs) -> xs.Length, x)

      let appendToString string1 string2 =
         $"{string1} {string2}"

      let getGroupedNodes state =

         let results = groupOuterNodes state
         let finalString = $"Surface Area: {Nesting.getSurfaceArea state}\n"

         let rec processNode node level finalString =
            match node.Children with
            | [] -> appendToString finalString $"{level},{(node.Diameter |> PipeDiameter.toInt)}"
            | childrenL ->

               let newString = appendToString finalString $"{level},{node.Diameter |> PipeDiameter.toInt};"

               let rec loop children level newString =
                  match children with
                  | [] ->  newString
                  | child::tail ->
                     let updatedString = processNode child (level + 1) newString
                     loop tail level updatedString

               loop childrenL (level + 1) newString

         results
         |> List.map (fun (i,node) ->
            processNode node 2 $" {i} trees: ; "
         )

namespace Nester
open System
open System.IO

module Version3 =

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
      let getChildren state parentId leafNodeM =
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

      let getListOfUnNestedChildrenWithDiameter childDiameter n state  =
         state.UnNestedByDiameterM
         |> Map.find childDiameter
         |> Set.toList
         |> Helpers.splitAfterN n

      let getDiameterFromChildrenSet set state =
        set
           |> Set.map (fun i -> state.PipeM |> Map.find i)
           |> Set.map (fun p -> p.Diameter)
           |> Set.minElement

      let getDiameter pipeId state =
         let pipe = state.PipeM |> Map.find pipeId
         pipe.Diameter

      let checkIfPipeHasSpace parentPipeId state =

         let parentDiameter = state.PipeM |> Map.find parentPipeId |> (fun p -> p.Diameter)

         let children = state.NestedPipesByParentM |> Map.find parentPipeId
         let childDiameter = getDiameterFromChildrenSet children state
         let lookup = Attempt1.lookupNumberOfPipesThatCanFit childDiameter parentDiameter

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

      let checkIfSpaceForSpecificPipeInParent parentId childDiameter state =

         let parentDiameter = getDiameter parentId state
         let lookup = Attempt1.lookupNumberOfPipesThatCanFit childDiameter parentDiameter

         match state.NestedPipesByParentM |> Map.tryFind parentId with
         | Some children ->
            let childrenDiameter = getDiameterFromChildrenSet children state
            if (childrenDiameter = childDiameter) then
               let childCount = children |> Set.count
               if lookup > childCount then (lookup - childCount)
               else 0
            else 0
         | None -> lookup

      let nestPipes childPipes parentPipeId initialState =
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

      let rec findPipesToNestFromChildOptions outerPipeSize innerPipeSizes =
         match innerPipeSizes with
         | [] -> None
         | size::tail ->
            let n = Attempt1.lookupNumberOfPipesThatCanFit size outerPipeSize
            if (n = 0) then findPipesToNestFromChildOptions outerPipeSize tail
            else
               Some (size, n)

      let getDiametersOfUnNestedPipes state =
         state.UnNestedByDiameterM
            |> Map.keys
            |> Seq.toList
            |> List.sortDescending

      let findPossibleChildrenCountL parentDiameter (unNestedByDiameterM : Map<PipeDiameter, Set<PipeId>>) =
            unNestedByDiameterM
            |> Map.toList
            |> List.map (fun (childDiameter, s) ->
                  let availableToNestCount = Set.count s
                  let canFitCount = Attempt1.lookupNumberOfPipesThatCanFit childDiameter parentDiameter
                  {|
                     ChildDiameter = childDiameter
                     AvailableToNestCount = availableToNestCount
                     CanFitCount = canFitCount
                  |}
               )
            |> List.filter (fun x -> x.CanFitCount > 0)

      let findNestingWithLargestSurfaceAreaO parentDiameter state =
         match findPossibleChildrenCountL parentDiameter state.UnNestedByDiameterM with
         | [] -> None
         | l ->
            let possibleNestingL' =
               l
               |> List.map (fun i ->
                     let nestingCount =  min i.AvailableToNestCount i.CanFitCount
                     let area = (i.ChildDiameter |> PipeDiameter.toFloat)**2 * (float nestingCount)
                     {| ChildDiameter = i.ChildDiameter; NestingCount = nestingCount; Area = area |}
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
                 match findPipesToNestFromChildOptions nextParentDiameter childOptions with
                 | None ->
                    state
                 | Some (childDiameter, available) ->
                    let canFit = Nesting.checkIfSpaceForSpecificPipeInParent nextParentId childDiameter state
                    let children =
                       Nesting.getListOfUnNestedChildrenWithDiameter
                          childDiameter
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


   module bottomUp =
      let rec findPipeToNestInFromParentOptions outerPipeSizes innerPipeSize =
         match outerPipeSizes with
         | [] -> None
         | size::tail ->
            let n = Attempt1.lookupNumberOfPipesThatCanFit innerPipeSize size
            if (n = 0) then findPipeToNestInFromParentOptions tail innerPipeSize
            else
               Some (size, n)

      let getNextChildDiameterFromState unProcessed =
         let diameter =
            unProcessed
            |> Map.keys
            |> Seq.min
         diameter,
         unProcessed
            |> Map.find diameter
            |> Set.minElement

      let getAvailableParentPipesFromState state =
         state.EmptyByDiameterM
         |> Map.keys
         |> Seq.toList
         |> List.sort

      let findBestPipeToNestIn outerPipeSizes innerPipeSize =
         let result =
            outerPipeSizes
            |> List.map
                  (fun size ->
                     size, Attempt1.lookupNumberOfPipesThatCanFit innerPipeSize size)
            |> List.filter (fun (_, n)-> n > 0)

         match result with
         | [] -> None
         | _ -> Some (result |> List.minBy snd)

      let getNextChildDiameterByQuantity state =
         state.UnNestedByDiameterM
         |> Map.toList
         |> List.map (fun (d, s) -> d, Set.count s)
         |> List.maxBy snd

      let removeUnProcessedChildrenOfDiameter
         nextChildDiameter
         numberOfNextChild
         (unProcessed : Map<PipeDiameter, Set<PipeId>>) =

         let childrenSet = unProcessed
                           |> Map.find nextChildDiameter
                           |> Set.toList
                           |> List.truncate numberOfNextChild
         childrenSet
            |> List.fold (fun acc child ->
               acc |> Helpers.removeElementFromSetInMap nextChildDiameter child)
            unProcessed

      let getNextParentIdFromParentDiameter state diameter =
         state.EmptyByDiameterM
         |> Map.find diameter
         |> Set.minElement

      let nestBottomUp state =
         let unProcessedByDiameterM = state.UnNestedByDiameterM

         let rec inner currentState unProcessedByDiameterM =
            let state = State.removeAnyEmptySetsFromState currentState
            match unProcessedByDiameterM with
            | _ when (Map.isEmpty unProcessedByDiameterM) -> state
            | _ ->
               let nextChildDiameter, nextChildId = getNextChildDiameterFromState unProcessedByDiameterM
               let parentOptions = getAvailableParentPipesFromState state

               let state', unProcessedByDiameterM' =
                  match findPipeToNestInFromParentOptions parentOptions nextChildDiameter with
                  | None ->
                     state,
                     unProcessedByDiameterM
                     |> Helpers.removeElementFromSetInMap nextChildDiameter nextChildId

                  | Some (parentDiameter, available) ->
                     let parentId = getNextParentIdFromParentDiameter state parentDiameter
                     let canFit = Nesting.checkIfSpaceForSpecificPipeInParent parentId nextChildDiameter state

                     match canFit with
                     | 0 ->
                        state,
                        unProcessedByDiameterM
                        |> Helpers.removeElementFromSetInMap nextChildDiameter nextChildId
                     | _ ->
                        let children =
                           Nesting.getListOfUnNestedChildrenWithDiameter
                              nextChildDiameter
                              (min available canFit)
                              state
                        Nesting.nestPipes
                           children
                           parentId
                           state,
                        children
                        |> List.fold (fun acc child ->
                           acc |> Helpers.removeElementFromSetInMap nextChildDiameter child)
                           unProcessedByDiameterM

               inner state' (unProcessedByDiameterM' |> Map.filter (fun _ v -> not (Set.isEmpty v)))
         inner state unProcessedByDiameterM

      let nestByQuantityOfPipe state =
         let unProcessedByDiameterM = state.UnNestedByDiameterM

         let rec inner currentState unProcessedDiameterM =
            let state = State.removeAnyEmptySetsFromState currentState
            match unProcessedDiameterM with
            | _ when (Map.isEmpty unProcessedDiameterM) -> state
            | _ ->
               let nextChildDiameter, numberOfNextChild = getNextChildDiameterByQuantity state
               let parentOptions = getAvailableParentPipesFromState state
               let state', unProcessedDiameterM' =
                  match
                     findBestPipeToNestIn parentOptions nextChildDiameter with
                  | None ->
                     state,
                     removeUnProcessedChildrenOfDiameter
                        nextChildDiameter
                        numberOfNextChild
                        unProcessedDiameterM

                  | Some (parentDiameter, n) ->
                     let parentId = getNextParentIdFromParentDiameter state parentDiameter
                     let children = Nesting.getListOfUnNestedChildrenWithDiameter nextChildDiameter n state

                     let newStateAfterNesting =
                        Nesting.nestPipes
                           children
                           parentId
                           state
                     let newUnProcessed =
                        children
                        |> List.fold (fun acc child ->
                           acc |> Helpers.removeElementFromSetInMap nextChildDiameter child)
                           unProcessedByDiameterM

                     newStateAfterNesting, newUnProcessed

               inner state' unProcessedDiameterM'
         inner state unProcessedByDiameterM

   module UnNesting =

      let checkIfParentIsNowEmpty childId parentId (state : State) =
         let siblings =
            state.PipeM
            |> Map.filter (fun k v ->
               not (k = childId) && (v.ParentIdO = Some parentId))
         match siblings with
         | l when (Map.isEmpty l) ->
            let parentPipe = state.PipeM |> Map.find parentId
            state.EmptyPipes |> Set.add parentId,
            state.EmptyByDiameterM |> Helpers.addElementToSetInMap parentPipe.Diameter parentId
         | _ -> state.EmptyPipes, state.EmptyByDiameterM

      let unNest pipeL state =
         let rec inner pipeL (state : State) =
            match pipeL with
            | [] -> (state |> State.removeAnyEmptySetsFromState)
            | pipeId::tail ->

               let pipe = state.PipeM |> Map.find pipeId
               let emptyPipes', emptyByDiameterM', nestedPipesByParent' =

                  match pipe.ParentIdO with
                  | None ->
                     state.EmptyPipes,
                     state.EmptyByDiameterM,
                     state.NestedPipesByParentM

                  | Some parent ->
                     let empty, emptyByDiameter = checkIfParentIsNowEmpty pipeId parent state
                     empty,
                     emptyByDiameter,
                     state.NestedPipesByParentM |> Helpers.removeElementFromSetInMap parent pipeId

               let pipeM'=
                  state.PipeM
                  |> State.replacePipe pipeId {Diameter = pipe.Diameter; ParentIdO = None}

               let state' =
                  {
                        PipeM = pipeM'
                        UnNestedPipes = state.UnNestedPipes |> Set.add pipeId
                        UnNestedByDiameterM =
                           state.UnNestedByDiameterM
                           |> Helpers.addElementToSetInMap pipe.Diameter pipeId
                        EmptyPipes = emptyPipes'
                        EmptyByDiameterM = emptyByDiameterM'
                        NestedPipesByParentM = nestedPipesByParent'
                  }
               inner tail state'
         inner pipeL state

      let emptyPipe pipeId state =
         let children =
            state.PipeM
               |> Map.filter (fun _ p -> p.ParentIdO = pipeId)
               |> Map.keys
               |> Seq.toList
         unNest children state

      let randomUnNesting (generator : Random) state =
         let numberOfPipes = state.PipeM |> Map.toList |> List.length
         let pipesToUnNest =
            state.PipeM
            |> Map.keys
            |> Seq.toList
            |> List.map (fun i ->
               let n = generator.Next(1, numberOfPipes) |> float
               i, 1.0 / n)
            |> List.filter (fun (_, n) -> n >= 0.5)
            |> List.map fst
         unNest pipesToUnNest state

      let getUnNestingL state unNestingN =
         let rec inner nestingL n =
            match n with
            | 0 -> nestingL
            | _ ->
               let generator = Random(n)
               let unNesting = randomUnNesting generator state
               inner (unNesting::nestingL) (n - 1)
         inner [] unNestingN

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



      let runAllAlgorithms state =

         let td = topDown.nestTopDown state
         let bu = bottomUp.nestBottomUp state
         let sa = topDown.nestBySurfaceArea state

         [ td; bu; sa ]

      let runGenetic state unNestingN generationsN =
         let rec inner state generationsN =
            match generationsN with
            | 0 -> state
            | _ ->
               let state' =
                  UnNesting.getUnNestingL state unNestingN
                  |> List.map runAllAlgorithms
                  |> List.concat
                  |> Nesting.getBestNesting
               //printGroupedNodes state' "grouped.txt" //uncomment to see generations
               inner state' (generationsN - 1)
         inner state generationsN

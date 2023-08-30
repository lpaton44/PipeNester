namespace Nester

open System.IO

module Attempt1 =

   let lookupTableData = [

        [0; 500; 400; 355; 315; 250; 200; 160; 140; 125; 110; 90; 75; 63; 50; 40; 32; 25];
      [500;   0;   0;   0;   0;   0;   0;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [400;   1;   0;   0;   0;   0;   0;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [355;   1;   0;   0;   0;   0;   0;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [315;   1;   1;   0;   0;   0;   0;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [250;   1;   1;   1;   1;   0;   0;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [200;   2;   1;   1;   1;   1;   0;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [160;   4;   2;   1;   1;   1;   1;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [140;   5;   3;   1;   1;   1;   1;   0;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [125;   7;   4;   3;   2;   1;   1;   1;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [110;   8;   5;   4;   3;   1;   1;   1;   0;   0;   0;  0;  0;  0;  0;  0;  0;  0];
      [ 90;  14;   8;   7;   5;   3;   1;   1;   1;   1;   0;  0;  0;  0;  0;  0;  0;  0];
      [ 75;  17;  10;   8;   7;   4;   2;   1;   1;   1;   1;  0;  0;  0;  0;  0;  0;  0];
      [ 63;  22;  14;  10;   9;   5;   4;   1;   1;   1;   1;  0;  0;  0;  0;  0;  0;  0];
      [ 50;  36;  23;  17;  14;   8;   6;   4;   2;   1;   1;  1;  1;  0;  0;  0;  0;  0];
      [ 40;  86;  53;  39;  32;  20;  13;   8;   7;   4;   3;  1;  1;  1;  1;  0;  0;  0];
      [ 32; 123;  79;  57;  48;  30;  19;  11;   8;   7;   5;  3;  1;  1;  1;  1;  0;  0];
      [ 25; 285; 181; 137; 116;  74;  44;  29;  21;  18;  13;  8;  5;  4;  2;  1;  1;  0]

   ]

   let lookUpTable =
      let mutable lookUp = Map.empty
      for i in 1..(lookupTableData.Length - 1) do
         for j in 1..(lookupTableData.Length - 1) do
            let key = (PipeDiameter (lookupTableData[i][0]), PipeDiameter (lookupTableData[0][j]))
            let value = lookupTableData[i][j]
            lookUp <- lookUp.Add (key,value)
      lookUp

   type Node = {
      Diameter : PipeDiameter
      Children : List<Node>
   }

   (*let nesting =
      {
         Diameter = 100
         Children = [
            {
               Diameter = 40
               Children = [
                  {
                     Diameter = 16
                     Children = []
                  }
               ]
            }
            {
               Diameter = 40
               Children = []
            }
         ]
      }*)

   let lookupNumberOfPipesThatCanFit innerPipeDiameter outerPipeDiameter  =
      lookUpTable |> Map.find (innerPipeDiameter,outerPipeDiameter)

   let nextNode nodesWithoutParents =
      nodesWithoutParents |> Map.toList |> List.sortBy fst |> List.head

   let getChildNodes maxCount diameter rootNodeL =
      rootNodeL |> List.filter (fun node -> node.Diameter = diameter) |> List.truncate maxCount

   let findBiggerPipe pipeList smallerPipeSize =
      let biggerPipes = pipeList |> Map.filter (fun k v -> lookupNumberOfPipesThatCanFit smallerPipeSize k > 1)
      match biggerPipes with
         | m when (m = Map.empty) -> None
         | _ -> Some (biggerPipes |> Map.toList |> List.minBy fst)

   let decrementMapValue key n =
      Map.change key (fun x ->
                        match x with
                        | Some x -> Some (x - n)
                        | None -> None)

   let incrementMapValue key n =
      Map.change key (fun x ->
                        match x with
                        | Some x -> Some (x + n)
                        | None -> None)


   (*let nestPipes (input : Map<int, int>) =

      let (startPipeDiameter, count) = input |> Map.toList |> List.minBy fst
      let mutable tree = List.empty

      for i in 0..count do
         tree <- {Diameter = startPipeDiameter ; Children = List.empty}::tree

      let nodesWithoutParents = Map.empty |> Map.add startPipeDiameter count
      let pipesList = input |> Map.remove startPipeDiameter

      let rec inner tree pipeList nodesWithoutParents =
         //printfn $"{tree}"
         if nodesWithoutParents = Map.empty then tree
         else
            let diameter, count = nextNode nodesWithoutParents
            let biggerPipe = findBiggerPipe pipeList diameter
            if (biggerPipe = None) then
               let newNodesWithoutParents =
                  nodesWithoutParents |> decrementMapValue diameter count |> Map.filter (fun k v -> v > 0)
               inner tree pipeList newNodesWithoutParents
            else
               let Diameter =
                  match biggerPipe with
                  | None -> 0
                  | Some v -> v |> fst
               let n = lookupNumberOfPipesThatCanFit diameter Diameter
               let children =  getChildNodes n diameter tree

               let newTree = {Diameter = Diameter ; Children = children} :: tree
               let newPipeList = pipeList |> decrementMapValue Diameter 1
               let newNodesWithoutParents =
                  if (nodesWithoutParents |> Map.containsKey Diameter) then
                     nodesWithoutParents |> incrementMapValue Diameter 1
                  else
                     nodesWithoutParents |> Map.add Diameter 1

               let withoutParents =
                  newNodesWithoutParents
                  |> decrementMapValue diameter (children.Length)
                  |> Map.filter (fun k v -> v > 0)

               inner newTree newPipeList withoutParents

      let answer = inner tree pipesList nodesWithoutParents
      answer*)

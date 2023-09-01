namespace Nester
open Data


module LookUp =
   let DiameterLookUpTable =
      let mutable lookUp = Map.empty
      for i in 0..(DiameterTable.Length - 1) do
         for j in 0..(DiameterTable.Length - 1) do
            let key = (PipeDiameter (DiameterTable[i][0]), PipeDiameter (DiameterTable[0][j]))
            let value = DiameterTable[i][j]
            lookUp <- lookUp.Add (key,value)
      lookUp

   let itemCodeLookUpTable =
      let mutable lookUp = Map.empty
      for i in 0..(itemCodeTable.Length - 1) do
            let key = itemCodeTable[i][0]
            let value = (itemCodeTable[i][1] |> int, itemCodeTable[i][2])
            lookUp <- lookUp.Add (key,value)
      lookUp

   let innerDiameterAndSocketWithOuterDiameterToCountLookUpTable =
      let mutable lookUp = Map.empty
      for i in 0..(DiameterAndSocketTable.Length - 1) do
            let key = (PipeDiameter (DiameterAndSocketTable[i][0] |> int),
                       PipeDiameter (DiameterAndSocketTable[i][1] |> int),
                       DiameterAndSocketTable[i][2])
            let value = DiameterAndSocketTable[i][3] |> int
            lookUp <- lookUp.Add (key,value)
      lookUp


   type Node = {
      Diameter : PipeDiameter
      Children : List<Node>
   }

   let lookupNumberOfPipesThatCanFit
    (innerPipeDiameter : PipeDiameter)
    (innerPipeSocket   : string)
    (outerPipeDiameter : PipeDiameter)
    =
      let n =
         innerDiameterAndSocketWithOuterDiameterToCountLookUpTable
         |> Map.tryFind (outerPipeDiameter, innerPipeDiameter, innerPipeSocket)
      match n with
      | None -> 0
      | Some v -> v

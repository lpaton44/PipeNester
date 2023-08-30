namespace Nester

type PipeId =
   PipeId of int

type PipeDiameter =
   PipeDiameter of int
   with
      static member toFloat (PipeDiameter d) = float d
      static member toInt (PipeDiameter d) = int d

type Pipe =
   {
      Diameter : PipeDiameter
      ParentIdO   : Option<PipeId>
   }

type State =
   {

      PipeM : Map<PipeId, Pipe>
      UnNestedByDiameterM : Map<PipeDiameter, Set<PipeId>>
      EmptyByDiameterM : Map<PipeDiameter, Set<PipeId>>
      UnNestedPipes : Set<PipeId>
      EmptyPipes : Set<PipeId>
      NestedPipesByParentM : Map<PipeId, Set<PipeId>>

   }
type Node =
   {
      Diameter : PipeDiameter
      Children : List<Node>
   }

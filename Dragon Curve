myShapes model =
   [ openPolygon (ink(rewrite [F] (round model.size)))
       |> outlined (solid 0.5) black |> scale 0.15 |> move (0,30) |>rotate (degrees 90)
   , text (Debug.toString model.points) |> size 4 |> filled red |> move (-94,-60)
   ,text (String.fromInt (round model.size) |> String.left 5)  |> filled black |> move(-85,-35)
   , ruler|> notifyTapAt ClickRuler
   ]

type Msg = Tick Float GetKeyState
         | ClickRuler (Float,Float)

update msg model = case msg of
                     Tick t _ -> { model | time = t}
                     ClickRuler (x,y) -> { model | size = x /180 * 31 + 15.5}

init : { size: Float,time : Float, points : List (Float,Float) }
init = { size=0,time = 0, points = [(0,0)] }

-- L-System
-- A system for rewriting a language
-- In the case of curves, a language for specifying motion.

type Moves
  = F
  | G
  | R
  | L
  
-- We need to be able to replace edges with more complicated curves.
-- We call this expression rewriting.

rewriteOne move =
  case move of
    F -> [F,R,G]
    G -> [F,L,G]
    R -> [R]
    L -> [L]

-- applies the rewriteOne to all elments of the list, and then concatenates to produce a single list
rewrite moves n = if n==0 then moves else rewrite(List.concatMap rewriteOne moves) (n-1) 
                    

type Turtle = Turtle (Float,Float)          -- current position
                     (Float,Float)          -- current step direction
                     (List (Float,Float))   -- history of where we've been

-- get the history to draw the curve
getPoints (Turtle _ _ points) = points

-- where our turtle starts before drawing
initTurtle = Turtle (0,0) (0,10) [(0,0)]

ink moves = List.foldl inkOne initTurtle moves
              |> getPoints

inkOne move (Turtle (x,y) (dx,dy) pointsSoFar) =
  case move of
    R  -> Turtle (x,y) (dy,-dx) pointsSoFar
    L  -> Turtle (x,y) (-dy,dx) pointsSoFar
    _     -> let
               new = (x+dx,y+dy)
             in
               Turtle new (dx,dy) ( new :: pointsSoFar )
               
ruler = group
  ( [ rectangle 200 22 |> filled (orange)
    , text "Dragon Curve" 
       |> customFont "cursive" 
       |> size 8 
       |> centered 
       |> filled black
       |> move (0,-7)
    ]
  ++ ( List.map tickMark (List.range 1 30) )
  )
  |> move (0,-50)
  
tickMark cm = 
  group [ rect 0.5 4 |> filled black
        , text (String.fromInt cm) 
             |> size 4 
             |> centered
             |> filled black
           |> move (0,-6.5)
        ]
     |> move (180 * (toFloat cm - 15.5) / 31 ,8)

               
           
